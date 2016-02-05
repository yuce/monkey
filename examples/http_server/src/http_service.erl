-module(http_service).

-export([start_link/1]).
-export([init/2,
         handler_args/1]).

start_link(Port) ->
    Args = #{port => Port, handler_count => 10},
    monkey_service:start_link(?MODULE, http_handler, Args).

init(_Handler, _Args) ->
    {ok, []}.

handler_args(_) ->
    {ok, #{}}.
