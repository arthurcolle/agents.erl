%%%-------------------------------------------------------------------
%%% @doc Service boundary for web storage and observability.
%%% @end
%%%-------------------------------------------------------------------
-module(agent_web_services_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 30},
    Children = [
        supervisor_child(agent_web_storage_sup),
        supervisor_child(agent_web_observability_sup)
    ],
    {ok, {SupFlags, Children}}.

supervisor_child(Module) ->
    #{
        id => Module,
        start => {Module, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [Module]
    }.
