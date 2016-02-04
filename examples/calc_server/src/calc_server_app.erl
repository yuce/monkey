-module(calc_server_app).
-behaviour(application).

-export([start/2,
         stop/1]).

-define(DEFAULT_PORT, 9090).

%% == API

start(_StartType, _StartArgs) ->
    calc_server_sup:start_link(?DEFAULT_PORT).

stop(_State) ->
    ok.
