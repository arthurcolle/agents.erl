%%%-------------------------------------------------------------------
%%% @doc Self-Scaffolding Supervisor
%%% Manages the self-scaffolding system that discovers and integrates
%%% API endpoints dynamically, with hourly updates
%%% @end
%%%-------------------------------------------------------------------
-module(self_scaffold_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 10,
        period => 60
    },
    
    Children = [
        #{
            id => endpoint_registry,
            start => {endpoint_registry, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [endpoint_registry]
        },
        #{
            id => openapi_fetcher,
            start => {openapi_fetcher, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [openapi_fetcher]
        },
        #{
            id => endpoint_discovery,
            start => {endpoint_discovery, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [endpoint_discovery]
        },
        #{
            id => scaffold_scheduler,
            start => {scaffold_scheduler, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [scaffold_scheduler]
        }
    ],
    
    {ok, {SupFlags, Children}}.