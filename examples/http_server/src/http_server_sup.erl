-module(http_server_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

%% == API

start_link(Port) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Port]).

%% == Callbacks

init([Port]) ->
    ChildSpec = #{id => http_service,
                  start => {http_service, start_link, [Port]},
                  restart => permanent,
                  shutdown => 1000,
                  type => worker,
                  modules => [http_service]},
    SupSpec = {{one_for_all, 0, 1}, [ChildSpec]},
    {ok, SupSpec}.
