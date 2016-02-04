-module(echo_handler).
-behaviour(monkey_handler).

-export([init/0,
         handle/2]).


init() ->
    {ok, undefined}.

handle({tcp, Data}, State) ->
    io:format("Received data: ~p~n", [Data]),
    monkey_handler:send(self(), Data),
    {ok, State};

handle(tcp_closed, State) ->
    io:format("Received close.~n"),
    {ok, State};

handle(_, State) ->
    {ok, State}.

