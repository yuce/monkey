-module(monkey_service).

-export([start_link/3,
         stop/2]).
-export([release/2]).
-export([init/4]).

-export([system_continue/3,
         system_terminate/4]).

-callback init(Handler :: atom(), Args :: map()) ->
    {ok, State :: term()}.

-callback handler_args(State :: term()) ->
    {ok, Args :: map()}.

-callback handle(Message :: term(), State :: term()) ->
    {ok, NewState :: term()}.

-define(DEFAULT_PORT, 10901).
-define(DEFAULT_HANDLER_COUNT, 5).

-record(state, {lsock,
                service_mod,
                user_state,
                handler,
                next_handler,
                parent,
                debug,
                free = [],
                used = []}).

%% == API

start_link(ServiceMod, Handler, Args) ->
    proc_lib:start_link(?MODULE, init, [ServiceMod, self(), Handler, Args]).

stop(Service, Reason) ->
    exit(Service, Reason).

%% == Internal API

release(Service, Pid) ->
    Service ! {release, Pid}.

%% == Callbacks

init(ServiceMod, Parent, Handler, Args) when is_list(Args) ->
    init(ServiceMod, Parent, Handler, maps:from_list(Args));

init(ServiceMod, Parent, Handler, Args) ->
    Port = get_port(Args),
    process_flag(trap_exit, true),
    {ok, LSock} = gen_tcp:listen(Port, [binary,
                                        {active, false},
                                        {reuseaddr, true},
                                        {packet, raw}]),
    proc_lib:init_ack(Parent, {ok, self()}),
    NewArgs = maps:put(port, Port, maps:remove(port, Args)),
    {ok, UserState} = ServiceMod:init(Handler, NewArgs),
    HandlerCount = get_handler_count(Args),
    State = #state{lsock = LSock,
                   service_mod = ServiceMod,
                   user_state = UserState,
                   handler = Handler,
                   parent = Parent,
                   debug = sys:debug_options([])},
    NewState = create_handlers(HandlerCount, State),
    loop_msg(NewState).

%%  == Internal

loop_msg(#state{parent = Parent,
                service_mod = ServiceMod,
                user_state = UserState,
                debug = Debug} = State) ->
    receive
        {system, From, Msg} ->
            sys:handle_system_msg(Msg, From, Parent, ?MODULE, Debug, State);
        {'EXIT', Parent, Reason} ->
            terminate(Reason, State);
        {'EXIT', Child, Reason} ->
            io:format("EXIT Child: ~p: ~p~n", [Child, Reason]),
            State1 = replace(Child, State),
            NewDebug = sys:handle_debug(Debug, fun debug/3, stop_request, Child),
            {ok, NewUserState} = ServiceMod:handle({exit_child, Child, Reason}, UserState),
            State2 = State1#state{debug = NewDebug, user_state = NewUserState},
            loop_msg(State2);
        {release, Pid} ->
            NewState = deallocate(Pid, State),
            loop_msg(NewState);
        Message ->
            {ok, NewUserState} = ServiceMod:handle({message, Message}, UserState),
            NewState = State#state{user_state = NewUserState},
            loop_msg(NewState)
        after 0 ->
            allocate_next(State)
    end.

allocate_next(#state{next_handler = undefined} = State) ->
    case allocate(State) of
        no_free ->
            loop_msg(State);
        {Pid, NewState} ->
            State1 = NewState#state{next_handler = Pid},
            accept(State1)
    end;

allocate_next(State) ->
    accept(State).

accept(#state{lsock = LSock,
              next_handler = Pid,
              debug = Debug} = State) ->
    case gen_tcp:accept(LSock, 10) of
        {ok, Sock} ->
            ok = gen_tcp:controlling_process(Sock, Pid),
            monkey_handler:assign_socket(Pid, Sock),
            NewState = State#state{next_handler = undefined},
            loop_msg(NewState);
        {error, timeout} ->
            loop_msg(State);
        {error, Reason} ->
            NewDebug = sys:handle_debug(Debug, fun debug/3, error, Reason),
            NewState = State#state{debug = NewDebug},
            terminate(Reason, NewState)
    end.

terminate(Reason, #state{lsock = LSock,
                         debug = Debug}) ->
    io:format("monkey_service terminate: ~p~n", [Reason]),
    sys:handle_debug(Debug, fun debug/3, terminating, Reason),
    gen_tcp:close(LSock),
    exit(Reason).

system_continue(_Parent, Debug, State) ->
    NewState = State#state{debug = Debug},
    loop_msg(NewState).

system_terminate(Reason, _Parent, Debug, State) ->
    NewState = State#state{debug = Debug},
    terminate(Reason, NewState).

debug(Dev, Event, Data) ->
    io:format(Dev, "monkey_service ~w:~w~n", [Event, Data]).

create_handler(#state{handler = Handler,
                      service_mod = ServiceMod,
                      user_state = UserState}) ->
    {ok, Args} = ServiceMod:handler_args(UserState),
    proc_lib:spawn_link(monkey_handler, init, [Handler, self(), Args]).

create_handlers(NumHandlers, State) when NumHandlers >= 1 ->
    F = fun(_) -> create_handler(State) end,
    Free = lists:map(F, lists:seq(1, NumHandlers)),
    State#state{free = Free}.

allocate(#state{free = []}) ->
     no_free;

allocate(#state{free = [NextFree | RestFree],
                used = Used} = State) ->
    NewState = State#state{free = RestFree,
                           used = [NextFree | Used]},
    {NextFree, NewState}.

deallocate(Pid, #state{free = Free,
                       used = Used} = State) ->
    NewUsed = lists:delete(Pid, Used),
    State#state{free = [Pid | Free], used = NewUsed}.

replace(Pid, #state{free = Free,
                    used = Used} = State) ->
    NewUsed = lists:delete(Pid, Used),
    NewPid = create_handler(State),
    State#state{free = [NewPid | Free], used = NewUsed}.

get_port(Args) ->
    maps:get(port, Args, ?DEFAULT_PORT).

get_handler_count(Args) ->
    maps:get(handler_count, Args, ?DEFAULT_HANDLER_COUNT).
