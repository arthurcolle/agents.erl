%%%-------------------------------------------------------------------
%%% @doc Observability subtree for the web application.
%%% @end
%%%-------------------------------------------------------------------
-module(agent_web_observability_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 30},
    Children = [
        worker(ai_error_processor),
        worker(comprehensive_error_logger),
        supervisor_child(meta_supervision_manager)
    ],
    {ok, {SupFlags, Children}}.

worker(Module) ->
    #{
        id => Module,
        start => {Module, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [Module]
    }.

supervisor_child(Module) ->
    #{
        id => Module,
        start => {Module, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [Module]
    }.
