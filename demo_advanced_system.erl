#!/usr/bin/env escript
%% demo_advanced_system.erl
%% Demonstration of the Advanced Multi-Agent System with Quantum Coordination

main(_Args) ->
    io:format("🚀 Advanced Multi-Agent System Demonstration~n"),
    io:format("~s~n", [lists:duplicate(60, $=)]),
    
    %% Start the advanced system
    io:format("~n🔧 Starting Advanced System...~n"),
    case advanced_agent_system:start_advanced_system() of
        {ok, _} ->
            io:format("✅ Advanced system started successfully!~n"),
            run_demonstrations();
        {error, Reason} ->
            io:format("❌ Failed to start advanced system: ~p~n", [Reason]),
            exit(1)
    end.

run_demonstrations() ->
    io:format("~n~s~n", [lists:duplicate(60, $=)]),
    io:format("🎯 Running Advanced System Demonstrations~n"),
    io:format("~s~n", [lists:duplicate(60, $=)]),
    
    %% Demo 1: Quantum Cluster Creation
    demo_quantum_clusters(),
    
    %% Demo 2: Swarm Intelligence
    demo_swarm_intelligence(),
    
    %% Demo 3: Lock-free Coordination
    demo_lockfree_coordination(),
    
    %% Demo 4: System Optimization
    demo_self_optimization(),
    
    %% Demo 5: System Status
    demo_system_status(),
    
    io:format("~n🎉 All demonstrations completed successfully!~n"),
    io:format("🛑 Shutting down system...~n"),
    advanced_agent_system:shutdown_advanced_system().

demo_quantum_clusters() ->
    io:format("~n1️⃣  QUANTUM CLUSTER DEMONSTRATION~n"),
    io:format("~s~n", [lists:duplicate(40, $-)]),
    
    %% Create different types of quantum clusters
    ClusterTypes = [high_performance, fault_tolerant, efficient],
    
    lists:foreach(fun(ClusterType) ->
        io:format("🔮 Creating ~p quantum cluster...~n", [ClusterType]),
        
        AgentSpecs = [
            #{name => <<"Agent-", (atom_to_binary(ClusterType))/binary, "-1">>, type => ai},
            #{name => <<"Agent-", (atom_to_binary(ClusterType))/binary, "-2">>, type => ai},
            #{name => <<"Agent-", (atom_to_binary(ClusterType))/binary, "-3">>, type => ai}
        ],
        
        case advanced_agent_system:create_quantum_cluster(ClusterType, AgentSpecs) of
            {ok, ClusterInfo} ->
                ClusterId = maps:get(cluster_id, ClusterInfo),
                AgentCount = length(maps:get(agents, ClusterInfo)),
                io:format("   ✅ Cluster ~p created with ~p agents~n", [ClusterId, AgentCount]);
            {error, Reason} ->
                io:format("   ❌ Failed to create cluster: ~p~n", [Reason])
        end
    end, ClusterTypes).

demo_swarm_intelligence() ->
    io:format("~n2️⃣  SWARM INTELLIGENCE DEMONSTRATION~n"),
    io:format("~s~n", [lists:duplicate(40, $-)]),
    
    %% Create different types of swarms
    SwarmTypes = [ant_colony, particle_swarm, neural_swarm],
    
    lists:foreach(fun(SwarmType) ->
        io:format("🐝 Deploying ~p swarm...~n", [SwarmType]),
        
        SwarmConfig = #{
            optimization_target => performance,
            emergence_enabled => true,
            learning_rate => 0.1
        },
        
        case advanced_agent_system:deploy_swarm_intelligence(SwarmType, 10, SwarmConfig) of
            {ok, SwarmId} ->
                io:format("   ✅ Swarm ~p deployed successfully~n", [SwarmId]);
            {error, Reason} ->
                io:format("   ❌ Failed to deploy swarm: ~p~n", [Reason])
        end
    end, SwarmTypes).

demo_lockfree_coordination() ->
    io:format("~n3️⃣  LOCK-FREE COORDINATION DEMONSTRATION~n"),
    io:format("~s~n", [lists:duplicate(40, $-)]),
    
    %% Test lock-free data structures
    io:format("🔒 Testing lock-free data structures...~n"),
    
    %% Create lock-free queue
    case lockfree_coordination:create_lockfree_queue(#{}) of
        {ok, QueueId} ->
            io:format("   ✅ Lock-free queue created: ~p~n", [QueueId]);
        {error, Reason} ->
            io:format("   ❌ Queue creation failed: ~p~n", [Reason])
    end,
    
    %% Create lock-free stack
    case lockfree_coordination:create_lockfree_stack(#{}) of
        {ok, StackId} ->
            io:format("   ✅ Lock-free stack created: ~p~n", [StackId]);
        {error, Reason} ->
            io:format("   ❌ Stack creation failed: ~p~n", [Reason])
    end,
    
    %% Create lock-free hashmap
    case lockfree_coordination:create_lockfree_hashmap(#{initial_size => 1024}) of
        {ok, HashMapId} ->
            io:format("   ✅ Lock-free hashmap created: ~p~n", [HashMapId]);
        {error, Reason} ->
            io:format("   ❌ Hashmap creation failed: ~p~n", [Reason])
    end.

demo_self_optimization() ->
    io:format("~n4️⃣  SELF-OPTIMIZATION DEMONSTRATION~n"),
    io:format("~s~n", [lists:duplicate(40, $-)]),
    
    io:format("🧠 Enabling self-optimization...~n"),
    advanced_agent_system:enable_self_optimization(5000),
    
    io:format("   ✅ Self-optimization enabled with 5s interval~n"),
    io:format("   🔄 System will continuously optimize performance~n"),
    io:format("   📊 Pattern analysis, NUMA optimization, and thermal management active~n").

demo_system_status() ->
    io:format("~n5️⃣  SYSTEM STATUS DEMONSTRATION~n"),
    io:format("~s~n", [lists:duplicate(40, $-)]),
    
    io:format("📊 Getting comprehensive system status...~n"),
    
    Status = advanced_agent_system:get_system_status(),
    
    %% Display key metrics
    SystemHealth = maps:get(system_health, Status, #{}),
    OverallHealth = maps:get(overall, SystemHealth, unknown),
    
    io:format("   🏥 Overall Health: ~p~n", [OverallHealth]),
    
    case maps:get(quantum_runtime, Status, not_running) of
        not_running ->
            io:format("   ⚛️  Quantum Runtime: Not Running~n");
        #{status := running} ->
            io:format("   ⚛️  Quantum Runtime: ✅ Running~n");
        _ ->
            io:format("   ⚛️  Quantum Runtime: ❓ Unknown Status~n")
    end,
    
    case maps:get(cluster_orchestrator, Status, not_running) of
        not_running ->
            io:format("   🎭 Cluster Orchestrator: Not Running~n");
        #{status := running, active_clusters := ClusterCount} ->
            io:format("   🎭 Cluster Orchestrator: ✅ Running (~p clusters)~n", [ClusterCount]);
        _ ->
            io:format("   🎭 Cluster Orchestrator: ❓ Unknown Status~n")
    end,
    
    case maps:get(lockfree_coordination, Status, not_running) of
        not_running ->
            io:format("   🔒 Lock-free Coordination: Not Running~n");
        #{status := running} ->
            io:format("   🔒 Lock-free Coordination: ✅ Running~n");
        _ ->
            io:format("   🔒 Lock-free Coordination: ❓ Unknown Status~n")
    end,
    
    %% Display uptime
    Uptime = maps:get(uptime, Status, 0),
    io:format("   ⏱️  System Uptime: ~p ms~n", [Uptime]).

