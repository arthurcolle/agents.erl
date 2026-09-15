%%%-------------------------------------------------------------------
%%% @doc Root supervisor for the core agent application.
%%%
%%% Platform services live here. agent_supervisor is reserved for
%%% dynamically-created agent instances so their lifecycle has one
%%% authoritative owner.
%%% @end
%%%-------------------------------------------------------------------
-module(agents_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },

    ChildSpecs = [
        worker(agent_time_authority),
        worker(agent_tools, [#{}]),
        worker(agent_registry, [#{}]),

        %% Runtime services formerly hidden under the dynamic-agent
        %% supervisor. They are application infrastructure, not agents.
        worker(agent_dynamic_system),
        worker(deep_code_reflection_engine),
        worker(metacognitive_code_analyzer),
        worker(philosophical_code_evaluator),

        supervisor_child(agent_supervisor),
        supervisor_child(super_agent_sup),
        worker(agent_collaboration),
        worker(knowledge_base_retrieval),
        worker(model_selection_strategy),
        worker(specialized_agent_factory),
        supervisor_child(meta_system_sup),
        worker(pipedream_tool_integration),
        supervisor_child(dynamic_runtime_sup),
        worker(system_introspection),
        worker(agent_scheduler_engine),
        worker(timeline_event_store),
        worker(agent_temporal_awareness)
    ],

    {ok, {SupFlags, ChildSpecs}}.

worker(Module) ->
    worker(Module, []).

worker(Module, Args) ->
    #{
        id => Module,
        start => {Module, start_link, Args},
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
