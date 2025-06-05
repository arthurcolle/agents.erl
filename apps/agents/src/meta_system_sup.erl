%%%-------------------------------------------------------------------
%%% @doc Meta-System Supervisor
%%% Top-level supervisor for all meta-abstraction layers including
%%% error monitoring, feedback systems, and self-modification capabilities.
%%% This provides a unified control plane for meta-operations.
%%% @end
%%%-------------------------------------------------------------------
-module(meta_system_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    SupFlags = #{
        strategy => rest_for_one,  % If coordinator fails, restart all dependent systems
        intensity => 10,
        period => 60
    },
    
    ChildSpecs = [
        %% Meta-Layer Coordinator - Central coordination hub
        #{
            id => meta_layer_coordinator,
            start => {meta_layer_coordinator, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [meta_layer_coordinator]
        },
        
        %% Unified Feedback System - Aggregates all system feedback
        #{
            id => unified_feedback_system,
            start => {unified_feedback_system, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [unified_feedback_system]
        },
        
        %% Meta-Meta Monitor - Monitors the meta-systems themselves
        #{
            id => meta_meta_monitor,
            start => {meta_meta_monitor, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [meta_meta_monitor]
        },
        
        %% Cross-System Intelligence - Enables cross-system learning
        #{
            id => cross_system_intelligence,
            start => {cross_system_intelligence, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [cross_system_intelligence]
        },
        
        %% Meta-Server - Separate server for viewing meta-operations
        #{
            id => meta_server,
            start => {meta_server, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [meta_server]
        },
        
        %% Quantum Consciousness Engine - Quantum-inspired consciousness simulation
        #{
            id => quantum_consciousness_engine,
            start => {quantum_consciousness_engine, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [quantum_consciousness_engine]
        },
        
        %% Temporal Meta Analyzer - Time-series analysis of meta-patterns
        #{
            id => temporal_meta_analyzer,
            start => {temporal_meta_analyzer, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [temporal_meta_analyzer]
        },
        
        %% Self-Replicating Supervisor - Self-replicating supervision trees
        #{
            id => self_replicating_supervisor,
            start => {self_replicating_supervisor, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [self_replicating_supervisor]
        },
        
        %% Adaptive Architecture Engine - Self-modifying system architecture
        #{
            id => adaptive_architecture_engine,
            start => {adaptive_architecture_engine, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [adaptive_architecture_engine]
        }
    ],
    
    {ok, {SupFlags, ChildSpecs}}.