-module(calc_server_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

%% == API functions

start_link(Port) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Port]).

%% == Calllbacks

init([Port]) ->
    ChildSpec = #{id => calc_service,
                  start => {calc_service, start_link, [Port]},
                  restart => permanent,
                  shutdown => 1000,
                  type => worker,
                  modules => [calc_service]},
    SupSpec = {{one_for_all, 0, 1}, [ChildSpec]},
    {ok, SupSpec}.
