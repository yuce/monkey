-module(echo_server_app).
-behaviour(application).

-export([start/2,
         stop/1]).

-define(DEFAULT_PORT, 9999).

%% == API

start(_StartType, _StartArgs) ->
    echo_server_sup:start_link(?DEFAULT_PORT).

stop(_State) ->
    ok.
