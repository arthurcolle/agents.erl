%% test_advanced_system.erl
%% Simple test runner for the advanced agent system
-module(test_advanced_system).
-export([run_tests/0]).

run_tests() ->
    io:format("🧪 Testing Advanced Multi-Agent System~n"),
    io:format("~s~n", [lists:duplicate(50, $=)]),
    
    %% Test individual components
    test_quantum_runtime(),
    test_cluster_orchestrator(), 
    test_lockfree_coordination(),
    test_advanced_agent_system(),
    
    io:format("~n✅ All tests completed!~n").

test_quantum_runtime() ->
    io:format("~n🔬 Testing Quantum Runtime...~n"),
    
    try
        %% Test module loading
        case code:ensure_loaded(quantum_runtime) of
            {module, quantum_runtime} ->
                io:format("   ✅ Quantum runtime module loaded~n");
            {error, Reason} ->
                io:format("   ❌ Failed to load quantum runtime: ~p~n", [Reason])
        end,
        
        %% Test basic functionality
        case quantum_runtime:start_link() of
            {ok, _Pid} ->
                io:format("   ✅ Quantum runtime started successfully~n"),
                
                %% Test analyze patterns
                try
                    Patterns = quantum_runtime:analyze_patterns(),
                    io:format("   ✅ Pattern analysis: ~p~n", [Patterns])
                catch
                    Error:Reason ->
                        io:format("   ⚠️  Pattern analysis failed: ~p:~p~n", [Error, Reason])
                end;
            {error, Reason} ->
                io:format("   ❌ Failed to start quantum runtime: ~p~n", [Reason])
        end
    catch
        Error:Reason ->
            io:format("   ❌ Quantum runtime test failed: ~p:~p~n", [Error, Reason])
    end.

test_cluster_orchestrator() ->
    io:format("~n🎭 Testing Cluster Orchestrator...~n"),
    
    try
        %% Test module loading
        case code:ensure_loaded(cluster_orchestrator) of
            {module, cluster_orchestrator} ->
                io:format("   ✅ Cluster orchestrator module loaded~n");
            {error, Reason} ->
                io:format("   ❌ Failed to load cluster orchestrator: ~p~n", [Reason])
        end,
        
        %% Test basic functionality
        case cluster_orchestrator:start_link() of
            {ok, _Pid} ->
                io:format("   ✅ Cluster orchestrator started successfully~n");
            {error, Reason} ->
                io:format("   ❌ Failed to start cluster orchestrator: ~p~n", [Reason])
        end
    catch
        Error:Reason ->
            io:format("   ❌ Cluster orchestrator test failed: ~p:~p~n", [Error, Reason])
    end.

test_lockfree_coordination() ->
    io:format("~n🔒 Testing Lock-free Coordination...~n"),
    
    try
        %% Test module loading
        case code:ensure_loaded(lockfree_coordination) of
            {module, lockfree_coordination} ->
                io:format("   ✅ Lock-free coordination module loaded~n");
            {error, Reason} ->
                io:format("   ❌ Failed to load lock-free coordination: ~p~n", [Reason])
        end,
        
        %% Test data structure creation
        try
            {ok, _QueueId} = lockfree_coordination:create_lockfree_queue(#{}),
            io:format("   ✅ Lock-free queue creation works~n")
        catch
            Error:Reason ->
                io:format("   ⚠️  Queue creation failed: ~p:~p~n", [Error, Reason])
        end,
        
        try
            {ok, _StackId} = lockfree_coordination:create_lockfree_stack(#{}),
            io:format("   ✅ Lock-free stack creation works~n")
        catch
            Error:Reason ->
                io:format("   ⚠️  Stack creation failed: ~p:~p~n", [Error, Reason])
        end
    catch
        Error:Reason ->
            io:format("   ❌ Lock-free coordination test failed: ~p:~p~n", [Error, Reason])
    end.

test_advanced_agent_system() ->
    io:format("~n🚀 Testing Advanced Agent System...~n"),
    
    try
        %% Test module loading
        case code:ensure_loaded(advanced_agent_system) of
            {module, advanced_agent_system} ->
                io:format("   ✅ Advanced agent system module loaded~n");
            {error, Reason} ->
                io:format("   ❌ Failed to load advanced agent system: ~p~n", [Reason])
        end,
        
        %% Test system startup
        case advanced_agent_system:start_advanced_system(#{
            quantum_enabled => true,
            numa_aware => false,  % Disable to avoid hardware dependencies
            thermal_monitoring => false,
            self_optimization => false,
            lockfree_coordination => true,
            max_clusters => 10,
            optimization_interval => 30000
        }) of
            {ok, _} ->
                io:format("   ✅ Advanced system started successfully~n"),
                
                %% Test system status
                try
                    Status = advanced_agent_system:get_system_status(),
                    io:format("   ✅ System status retrieved: ~p components~n", [maps:size(Status)])
                catch
                    Error:Reason ->
                        io:format("   ⚠️  Status retrieval failed: ~p:~p~n", [Error, Reason])
                end,
                
                %% Test shutdown
                try
                    advanced_agent_system:shutdown_advanced_system(),
                    io:format("   ✅ System shutdown completed~n")
                catch
                    Error:Reason ->
                        io:format("   ⚠️  Shutdown failed: ~p:~p~n", [Error, Reason])
                end;
            {error, Reason} ->
                io:format("   ❌ Failed to start advanced system: ~p~n", [Reason])
        end
    catch
        Error:Reason ->
            io:format("   ❌ Advanced agent system test failed: ~p:~p~n", [Error, Reason])
    end.