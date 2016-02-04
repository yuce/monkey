-module(monkey_handler).
-export([send/2,
         close/1,
         assign_socket/2]).

-export([init/2,
         loop_tcp/2]).


-callback init() ->
    {ok, State :: term()}.

-callback handle(Message :: term(), UserState :: term()) ->
    {ok, NewUserState :: term()} | {stop, Reason :: term()}.

-record(state, {service, handler, user_state}).

%% == API

send(Pid, Data) ->
    Pid ! {send_data, Data}.

close(Pid) ->
    Pid ! close_socket.

assign_socket(Pid, Sock) ->
    Pid ! {assign_socket, Sock}.

%% == Callbacks

init(Handler, Service) ->
    State = #state{service = Service,
                   handler = Handler},
    loop_wait(State).

%%  == Internal

loop_wait(#state{handler = Handler} = State) ->
    receive
        {assign_socket, Sock} ->
            {ok, UserState} = Handler:init(),
            NewState = State#state{user_state = UserState},
            inet:setopts(Sock, [{active, once}]),
            loop_tcp(Sock, NewState);
        Flush ->
            io:format("tcp_hander flushed: ~p~n", [Flush]),
            loop_wait(State)
    end.

loop_tcp(Sock, #state{service = Service,
                handler = Handler,
                user_state = UserState} = State) ->
    receive
        {tcp, Sock, Data} ->
            case Handler:handle({tcp, Data}, UserState) of
                {ok, NewUserState} ->
                    NewState = State#state{user_state = NewUserState},
                    inet:setopts(Sock, [{active, once}]),
                    loop_tcp(Sock, NewState);
                {stop, Reason} ->
                    gen_tcp:close(Sock),
                    tcp_service:stop(Service, Reason)
            end;
        {tcp_closed, Sock} ->
            handle_close(Service, State);
        {send_data, Data} ->
            ok = gen_tcp:send(Sock, Data),
            loop_tcp(Sock, State);
        close_socket ->
            gen_tcp:close(Sock),
            handle_close(Service, State);
        Message ->
            case Handler:handle(Message, UserState) of
                {ok, NewUserState} ->
                    NewState = State#state{user_state = NewUserState},
                    loop_tcp(Sock, NewState);
                {stop, Reason} ->
                    tcp_service:stop(Service, Reason)
            end
    end.

%% == Internal

handle_close(Service, #state{handler = Handler,
                             user_state = UserState} = State) ->
    case Handler:handle(tcp_closed, UserState) of
        {ok, _State} ->
            tcp_service:release(Service, self()),
            loop_wait(State);
        {stop, Reason} ->
            tcp_service:stop(Service, Reason)
    end.

