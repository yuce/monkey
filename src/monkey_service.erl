-module(monkey_service).

-export([start_link/2,
         stop/2]).
-export([release/2]).
-export([init/3]).

-export([system_continue/3,
         system_terminate/4]).

-define(DEFAULT_PORT, 10901).
-define(DEFAULT_HANDLER_COUNT, 5).

-record(state, {lsock,
                handler,
                next_handler,
                parent,
                debug,
                free = [],
                used = []}).

%% == API

start_link(Handler, Args) ->
    proc_lib:start_link(?MODULE, init, [Handler, Args, self()]).

stop(Service, Reason) ->
    exit(Service, Reason).

%% == Internal API

release(Service, Pid) ->
    Service ! {release, Pid}.

%% == Callbacks

init(Handler, Args, Parent) ->
    Port = get_port(Args),
    process_flag(trap_exit, true),
    {ok, LSock} = gen_tcp:listen(Port, [binary,
                                        {active, false},
                                        {reuseaddr, true},
                                        {packet, raw}]),
    proc_lib:init_ack(Parent, {ok, self()}),
    HandlerCount = get_handler_count(Args),
    {Free, Used} = create_handlers(Handler, HandlerCount),
    State = #state{lsock = LSock,
                   handler = Handler,
                   parent = Parent,
                   debug = sys:debug_options([]),
                   free = Free,
                   used = Used},
    loop_msg(State).

%%  == Internal

loop_msg(#state{parent = Parent,
                debug = Debug} = State) ->
    % io:format("Free: ~p~n", [length(State#state.free)]),
    receive
        {system, From, Msg} ->
            sys:handle_system_msg(Msg, From, Parent, ?MODULE, Debug, State);
        {'EXIT', Parent, Reason} ->
            terminate(Reason, State);
        {'EXIT', Child, Reason} ->
            io:format("EXIT Child: ~p: ~p~n", [Child, Reason]),
            State1 = replace(Child, State),
            NewDebug = sys:handle_debug(Debug, fun debug/3, stop_request, Child),
            State2 = State1#state{debug = NewDebug},
            loop_msg(State2);
        {release, Pid} ->
            NewState = deallocate(Pid, State),
            loop_msg(NewState);
        Flush ->
            io:format("monkey_service flushed: ~p~n", [Flush]),
            loop_msg(State)
        after 0 ->
            allocate_next(State)
    end.

allocate_next(#state{next_handler = undefined} = State) ->
    case allocate(State) of
        no_free ->
            % io:format("no_free~n"),
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
    % io:format("next_handler: ~p~n", [Pid]),
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
    io:format(Dev, "Listener ~w:~w~n", [Event, Data]).

create_handlers(Handler, NumHandlers)
        when NumHandlers >= 1 ->
    F = fun(_) ->
        proc_lib:spawn_link(monkey_handler, init, [Handler, self()])
    end,
    Free = lists:map(F, lists:seq(1, NumHandlers)),
    {Free, []}.

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
                    handler = Handler,
                    used = Used} = State) ->
    NewUsed = lists:delete(Pid, Used),
    NewPid = proc_lib:spawn_link(monkey_handler, init, [Handler, self()]),
    State#state{free = [NewPid | Free], used = NewUsed}.

get_port(PropList) ->
    proplists:get_value(port, PropList, ?DEFAULT_PORT).

get_handler_count(PropList) ->
    proplists:get_value(handler_count, PropList, ?DEFAULT_HANDLER_COUNT).

