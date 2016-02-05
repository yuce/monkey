-module(http_service).

-export([start_link/1]).

start_link(Port) ->
    monkey_service:start_link(http_handler, [{port, Port},
                                             {handler_count, 100}]).