-module(http_handler).
-behaviour(monkey_handler).

-export([init/1,
         handle/2]).

init(_Args) ->
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

handle({data, _Data}, #{response := Response} = State) ->
    {reply, Response, State};

handle(_Msg, State) ->
    {noreply, State}.