#!/usr/bin/env escript

%% @doc Test Consciousness Engineering Demo
%% Simple script to demonstrate the consciousness engineering capabilities
main(_) ->
    %% Change to the correct directory
    file:set_cwd("/Users/agent/agents.erl"),
    
    %% Add code paths
    code:add_path("_build/default/lib/openai/ebin"),
    code:add_path("_build/default/lib/agents/ebin"),
    code:add_path("_build/default/lib/agent_web/ebin"),
    code:add_path("_build/default/lib/jsx/ebin"),
    code:add_path("_build/default/lib/cowboy/ebin"),
    code:add_path("_build/default/lib/poolboy/ebin"),
    
    io:format("🧠 Consciousness Engineering Demo Starting...~n"),
    
    %% Test distributed consciousness engine
    test_distributed_consciousness(),
    
    %% Test quantum performance transcendence
    test_quantum_performance(),
    
    %% Test emergent swarm intelligence
    test_swarm_intelligence(),
    
    %% Test integration demo
    test_integration_demo(),
    
    io:format("✅ Consciousness Engineering Demo Complete!~n").

test_distributed_consciousness() ->
    io:format("~n🔮 Testing Distributed Consciousness Engine...~n"),
    
    try
        %% Test basic consciousness engine functionality
        {ok, _Pid} = distributed_consciousness_engine:start_link(),
        io:format("  ✓ Distributed consciousness engine started~n"),
        
        %% Add neurons to the network
        {ok, _} = distributed_consciousness_engine:add_neuron(test_agent_1, coordinator),
        {ok, _} = distributed_consciousness_engine:add_neuron(test_agent_2, analyzer),
        io:format("  ✓ Neural network neurons added~n"),
        
        %% Create synaptic connections
        {ok, _} = distributed_consciousness_engine:create_synapse(test_agent_1, test_agent_2, electrical),
        io:format("  ✓ Synaptic connections created~n"),
        
        %% Store collective memory
        {ok, _} = distributed_consciousness_engine:store_memory(
            test_memory, 
            "Consciousness demo successful", 
            #{importance => high}
        ),
        io:format("  ✓ Collective memory stored~n"),
        
        %% Get consciousness state
        ConsciousnessState = distributed_consciousness_engine:get_consciousness_state(),
        ConsciousnessLevel = maps:get(consciousness_level, ConsciousnessState),
        io:format("  ✓ Consciousness level: ~.3f~n", [ConsciousnessLevel]),
        
        %% Perform introspection
        _Introspection = distributed_consciousness_engine:introspect(),
        io:format("  ✓ System introspection completed~n")
        
    catch
        Error:Reason ->
            io:format("  ❌ Error in consciousness test: ~p:~p~n", [Error, Reason])
    end.

test_quantum_performance() ->
    io:format("~n⚡ Testing Quantum Performance Transcendence...~n"),
    
    try
        %% Test quantum performance optimization
        {ok, _Pid} = quantum_performance_transcendence:start_link(),
        io:format("  ✓ Quantum performance engine started~n"),
        
        %% Test latency optimization
        {ok, LatencyResult} = quantum_performance_transcendence:optimize_coordination_latency(50),
        io:format("  ✓ Latency optimization: ~p~n", [maps:get(optimization_success, LatencyResult, false)]),
        
        %% Test temporal pre-computation
        {ok, _} = quantum_performance_transcendence:enable_temporal_pre_computation(1000),
        io:format("  ✓ Temporal pre-computation enabled~n"),
        
        %% Test quantum acceleration
        {ok, _} = quantum_performance_transcendence:activate_quantum_acceleration(),
        io:format("  ✓ Quantum acceleration activated~n"),
        
        %% Test performance metrics
        Metrics = quantum_performance_transcendence:get_performance_metrics(),
        io:format("  ✓ Performance metrics: ~p~n", [maps:size(Metrics)])
        
    catch
        Error:Reason ->
            io:format("  ❌ Error in quantum performance test: ~p:~p~n", [Error, Reason])
    end.

test_swarm_intelligence() ->
    io:format("~n🐝 Testing Emergent Swarm Intelligence...~n"),
    
    try
        %% Test swarm intelligence system
        {ok, _Pid} = emergent_swarm_intelligence:start_link(),
        io:format("  ✓ Swarm intelligence engine started~n"),
        
        %% Create test swarm
        Participants = [swarm_agent_1, swarm_agent_2, swarm_agent_3],
        {ok, SwarmResult} = emergent_swarm_intelligence:create_swarm(
            test_swarm, Participants, general
        ),
        io:format("  ✓ Swarm created: ~p participants~n", [length(Participants)]),
        
        %% Test collective decision making
        DecisionProposal = #{
            question => "Test decision",
            options => [option_a, option_b]
        },
        {ok, DecisionResult} = emergent_swarm_intelligence:initiate_collective_decision(
            test_swarm, majority, DecisionProposal
        ),
        DecisionId = maps:get(decision_id, DecisionResult),
        io:format("  ✓ Collective decision initiated: ~p~n", [DecisionId]),
        
        %% Test swarm state
        {ok, SwarmState} = emergent_swarm_intelligence:get_swarm_state(test_swarm),
        io:format("  ✓ Swarm state retrieved: ~p~n", [maps:get(swarm_id, SwarmState)])
        
    catch
        Error:Reason ->
            io:format("  ❌ Error in swarm intelligence test: ~p:~p~n", [Error, Reason])
    end.

test_integration_demo() ->
    io:format("~n🚀 Testing Consciousness Integration Demo...~n"),
    
    try
        %% Test neural network coordination demo
        Result1 = consciousness_integration_demo:demo_neural_network_coordination(),
        io:format("  ✓ Neural network demo: ~p neurons~n", [maps:get(neurons_added, Result1, 0)]),
        
        %% Test microsecond consensus demo
        Result2 = consciousness_integration_demo:demo_microsecond_consensus(),
        io:format("  ✓ Microsecond consensus: ~p μs~n", [maps:get(latency_microseconds, Result2, 0)]),
        
        %% Test predictive pre-computation demo
        Result3 = consciousness_integration_demo:demo_predictive_pre_computation(),
        io:format("  ✓ Predictive pre-computation: ~p enabled~n", [maps:get(temporal_pre_computation_enabled, Result3, false)]),
        
        %% Test agent enhancement
        TestAgents = [demo_agent_1, demo_agent_2],
        EnhancementResult = consciousness_integration_demo:enhance_existing_agents(TestAgents),
        SuccessRate = maps:get(enhancement_rate, EnhancementResult, 0.0),
        io:format("  ✓ Agent enhancement: ~.1f% success rate~n", [SuccessRate * 100])
        
    catch
        Error:Reason ->
            io:format("  ❌ Error in integration demo test: ~p:~p~n", [Error, Reason])
    end.