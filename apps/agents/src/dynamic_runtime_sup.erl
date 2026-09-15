%%%-------------------------------------------------------------------
%%% @doc Supervision boundary for runtime-created supervisors.
%%%
%%% The host starts before the manager. With rest_for_one, losing the host
%%% also restarts the manager so it cannot retain stale ownership state.
%%% @end
%%%-------------------------------------------------------------------
-module(dynamic_runtime_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => rest_for_one,
        intensity => 10,
        period => 60
    },
    ChildSpecs = [
        #{
            id => dynamic_supervisors_sup,
            start => {dynamic_supervisors_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [dynamic_supervisors_sup]
        },
        #{
            id => dynamic_supervisor_manager,
            start => {dynamic_supervisor_manager, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [dynamic_supervisor_manager]
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
