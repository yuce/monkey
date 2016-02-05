-module(http_handler).
-behaviour(monkey_handler).

-export([init/0,
         handle/2]).

init() ->
    Body = <<"Hello, World!\r\n">>,
    BinContentLength = integer_to_binary(byte_size(Body)),
    BinDate = <<"Mon, 23 May 2005 22:38:34 GMT">>,
    CannedResponse = [
        <<"HTTP/1.1 200 OK\r\n">>,
        <<"Date: ">>, BinDate, <<"\r\n">>,
        <<"Server: Happy Monkey">>, <<"\r\n">>,
        <<"Content-Type: text/html; charset=UTF-8\r\n">>,
        <<"Content-Length: ">>, BinContentLength, <<"\r\n">>,
        <<"Connection: close\r\n">>,
        <<"\r\n">>,
        Body
    ],
    {ok, #{response => CannedResponse}}.

handle({tcp, _Data}, #{response := Response} = State) ->
    monkey_handler:send(self(), Response),
    {ok, State};

handle(_, State) ->
    {ok, State}.