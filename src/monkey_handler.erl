-module(monkey_handler).

-export([assign_socket/2]).
-export([init/3,
         loop_tcp/2]).

-callback init(Args :: map()) ->
    {ok, State :: term()}.

-callback handle(Message :: term(), UserState :: term()) ->
    % {ok, NewUserState :: term()} | {stop, Reason :: term()}.
    {noreply, NewUserState :: term()} |
    {reply, Reply :: iodata(), NewUserState :: term()} |
    stop.

-record(state, {service, handler, user_state, user_args}).

%% == API

% send(Pid, Data) ->
%     Pid ! {send_data, Data}.

% close(Pid) ->
%     Pid ! close_socket.

%% == Internal API

assign_socket(Pid, Sock) ->
    Pid ! {assign_socket, Sock}.

%% == Callbacks

init(Handler, Service, UserArgs) ->
    State = #state{service = Service,
                   handler = Handler,
                   user_args = UserArgs},
    loop_wait(State).

%%  == Internal

loop_wait(#state{handler = Handler,
                 user_args = UserArgs} = State) ->
    receive
        {assign_socket, Sock} ->
            {ok, UserState} = Handler:init(UserArgs),
            NewState = State#state{user_state = UserState},
            handle(open, Sock, NewState);
        Flush ->
            io:format("tcp_handler flushed: ~p~n", [Flush]),
            loop_wait(State)
    end.

loop_tcp(Sock, State) ->
    receive
        {tcp, Sock, Data} ->
            handle({data, Data}, Sock, State);
        {tcp_closed, Sock} ->
            handle(close, undefined, State);
        Message ->
            handle({message, Message}, Sock, State)
    end.

%% == Internal

handle_new_state(Sock, State) ->
    inet:setopts(Sock, [{active, once}]),
    loop_tcp(Sock, State).

handle_close(undefined, #state{service = Service} = State) ->
    monkey_service:release(Service, self()),
    loop_wait(State);

handle_close(Sock, State) ->
    gen_tcp:close(Sock),
    handle_close(undefined, State).

handle(close, Sock, #state{user_state = UserState,
                           handler = Handler} = State) ->
    Handler:handle(close, UserState),
    handle_close(Sock, State);

handle(Msg, Sock, #state{user_state = UserState,
                         handler = Handler} = State) ->
    case Handler:handle(Msg, UserState) of
        {noreply, NewUserState} ->
            handle_new_state(Sock, State#state{user_state = NewUserState});
        {reply, Data, NewUserState} ->
            ok = gen_tcp:send(Sock, Data),
            handle_new_state(Sock, State#state{user_state = NewUserState});
        stop ->
            handle_close(Sock, State)
    end.
