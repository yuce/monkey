-module(calc_handler).
-behaviour(monkey_handler).

-export([init/0,
         handle/2]).


init() ->
    Numbers = [],
    {ok, Numbers}.

handle({tcp, Data}, Numbers) ->
    case Data of
        <<"sum\r\n">> ->
            Result = float_to_binary(lists:sum(Numbers), [compact,
                                                          {decimals, 4}]),
            monkey_handler:send(self(), [Result, <<"\r\n">>]),
            {ok, Numbers};
        <<"mul\r\n">> ->
            Result = float_to_binary(calc_mul(Numbers), [compact,
                                                         {decimals, 4}]),
            monkey_handler:send(self(), [Result, <<"\r\n">>]),
            {ok, Numbers};
        _ ->
            Stripped = strip_bin(Data),
            case parse_number(Stripped) of
                {ok, Float} ->
                    {ok, [Float | Numbers]};
                _Error ->
                    % Ignore invalid numbers
                    monkey_handler:send(self(), [<<"Invalid cmd: ">>, Stripped, <<"\r\n">>]),
                    {ok, Numbers}
            end
    end;

handle(tcp_closed, Numbers) ->
    io:format("Connection closed, numbers: ~p~n", [Numbers]),
    {ok, Numbers};

handle(_, State) ->
    {ok, State}.


parse_number(Bin) ->
    try binary_to_float(Bin) of
        Float ->
            {ok, Float}
    catch
        error:badarg ->
            try binary_to_integer(Bin) of
                Integer ->
                    {ok, Integer + 0.0}
            catch
                error:badarg ->
                    {error, badarg}
            end
    end.

calc_mul(Numbers) ->
    Fun = fun(X, Acc) ->
        X * Acc
    end,
    lists:foldl(Fun, 1, Numbers).

strip_bin(Bin) ->
    binary:replace(Bin, <<"\r\n">>, <<>>).


