-module(echo_service).

-export([start_link/1]).

%% == API

start_link(Port) ->
    monkey_service:start_link(echo_handler, [{port, Port},
                                             {handler_count, 100}]).
