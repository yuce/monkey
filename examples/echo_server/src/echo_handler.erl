-module(echo_handler).
-behaviour(monkey_handler).

-export([init/1,
         handle/2]).


init(Args) ->
    {ok, Args}.

handle(open, #{handler := Handler,
               port := Port} = State) ->
    io:format("CONNECTED. handler: ~p (port: ~p)~n", [Handler, Port]),
    {reply, <<"Connected, yay!\r\n">>, State};

handle({data, Data}, State) ->
    io:format("Received data: ~p~n", [Data]),
    {reply, Data, State};

handle(close, _State) ->
    io:format("DISCONNECTED.~n"),
    stop;

handle(_, State) ->
    {noreply, State}.

