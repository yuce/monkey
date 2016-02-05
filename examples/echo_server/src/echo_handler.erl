-module(echo_handler).
-behaviour(monkey_handler).

-export([init/0,
         handle/2]).


init() ->
    {ok, undefined}.

handle(open, State) ->
    io:format("CONNECTED.~n"),
    % {noreply, State};
    {reply, <<"Connected, yay!\r\n">>, State};

handle({data, Data}, State) ->
    io:format("Received data: ~p~n", [Data]),
    {reply, Data, State};

handle(close, _State) ->
    io:format("DISCONNECTED.~n"),
    stop;

handle(_, State) ->
    {noreply, State}.

