-module(echo_service).
-behaviour(monkey_service).

-export([start_link/1]).
-export([init/2,
         handler_args/1]).

%% == API

start_link(Port) ->
    monkey_service:start_link(?MODULE, echo_handler, #{port => Port,
                                                       handler_count => 100}).

% == Callbacks

init(Handler, #{port := Port}) ->
    State = #{handler => Handler,
              port => Port},
    {ok, State}.

handler_args(State) ->
    {ok, State}.

