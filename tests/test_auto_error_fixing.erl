#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/*/ebin

-mode(compile).

main([]) ->
    io:format("🚀 Testing Auto Error Fixing System...~n~n"),
    
    %% Start required applications
    application:start(crypto),
    application:start(jsx),
    
    %% Test 1: Error Resilience with Auto-Healing
    io:format("Test 1: Auto-Healing Call~n"),
    test_auto_healing(),
    
    %% Test 2: Auto Error Fixer Pattern Learning
    io:format("~nTest 2: Error Pattern Learning~n"),
    test_pattern_learning(),
    
    %% Test 3: Predictive Error Prevention
    io:format("~nTest 3: Predictive Error Prevention~n"),
    test_predictive_prevention(),
    
    %% Test 4: Self-Evolving Error Patterns
    io:format("~nTest 4: Self-Evolving Patterns~n"),
    test_evolving_patterns(),
    
    %% Test 5: Distributed Error Coordination
    io:format("~nTest 5: Distributed Coordination~n"),
    test_distributed_coordination(),
    
    io:format("~n✅ All tests completed!~n").

test_auto_healing() ->
    %% Simulate a server that doesn't exist
    Result1 = error_resilience:auto_healing_call(
        non_existent_server, 
        {get_data, []}, 
        5000,
        {ok, default_data}
    ),
    io:format("  - Dead server call result: ~p~n", [Result1]),
    
    %% Test with timeout
    spawn(fun() ->
        register(slow_server, self()),
        receive
            {From, Request} ->
                timer:sleep(10000),  % Simulate slow response
                gen_server:reply(From, {ok, late_response})
        end
    end),
    
    Result2 = error_resilience:auto_healing_call(
        slow_server,
        {get_data, []},
        100,  % Short timeout
        {ok, timeout_default}
    ),
    io:format("  - Timeout call result: ~p~n", [Result2]),
    
    %% Register custom error handler
    error_resilience:register_error_handler(
        timeout,
        fun(Error, {Server, Request, _Timeout}) ->
            io:format("  - Custom handler triggered for ~p~n", [Server]),
            {ok, custom_handled}
        end
    ),
    
    Result3 = error_resilience:auto_healing_call(
        another_slow_server,
        {get_data, []},
        100,
        {ok, default}
    ),
    io:format("  - Custom handled result: ~p~n", [Result3]).

test_pattern_learning() ->
    %% Start the auto error fixer
    {ok, Pid} = auto_error_fixer:start_link(),
    
    %% Report several similar errors
    lists:foreach(fun(N) ->
        auto_error_fixer:report_error(
            test_module,
            test_function,
            {timeout, {gen_server, call, [server_x, request, 5000]}}
        ),
        timer:sleep(100)
    end, lists:seq(1, 5)),
    
    %% Check learned patterns
    timer:sleep(1000),
    Patterns = auto_error_fixer:get_error_patterns(),
    io:format("  - Learned patterns: ~p~n", [length(Patterns)]),
    
    %% Apply learned fix
    FixResult = auto_error_fixer:apply_learned_fix(test_module, test_function),
    io:format("  - Applied fix result: ~p~n", [FixResult]),
    
    %% Get fix statistics
    Stats = auto_error_fixer:get_fix_stats(),
    io:format("  - Fix statistics: ~p~n", [Stats]),
    
    gen_server:stop(Pid).

test_predictive_prevention() ->
    %% Start predictive error prevention
    {ok, Pid} = predictive_error_prevention:start_link(),
    
    %% Analyze current system state
    {ok, Snapshot} = predictive_error_prevention:analyze_system_state(),
    io:format("  - System snapshot: Memory: ~.1f%, Processes: ~p~n", 
              [maps:get(memory_usage, Snapshot, 0), 
               maps:get(process_count, Snapshot, 0)]),
    
    %% Predict errors for next 5 minutes
    {ok, Predictions} = predictive_error_prevention:predict_errors(300),
    io:format("  - Predictions generated: ~p~n", [length(Predictions)]),
    
    lists:foreach(fun(Pred) ->
        io:format("    * ~p error - probability: ~.1f%, confidence: ~.1f%~n",
                  [element(4, Pred),  % error_type
                   element(5, Pred) * 100,  % probability
                   element(6, Pred) * 100])  % confidence
    end, lists:sublist(Predictions, 3)),
    
    %% Apply preventive measures
    predictive_error_prevention:apply_preventive_measures(Predictions),
    
    gen_server:stop(Pid).

test_evolving_patterns() ->
    %% Start self-evolving error handler
    {ok, Pid} = self_evolving_error_handler:start_link(),
    
    %% Simulate various errors
    Errors = [
        {timeout, {gen_server, call, timeout}},
        {noproc, {gen_server, call, [dead_server]}},
        {badarg, {erlang, '+', [atom, 5]}},
        {badarith, {erlang, '/', [1, 0]}}
    ],
    
    Results = lists:map(fun(Error) ->
        self_evolving_error_handler:handle_error(test_mod, test_fun, Error)
    end, Errors),
    
    io:format("  - Handled errors: ~p~n", [Results]),
    
    %% Trigger evolution
    self_evolving_error_handler:evolve_patterns(),
    timer:sleep(1000),
    
    %% Get evolution statistics
    Stats = self_evolving_error_handler:get_evolution_stats(),
    io:format("  - Evolution stats: Generation ~p, Avg Fitness: ~.2f~n",
              [maps:get(generation, Stats), maps:get(avg_fitness, Stats)]),
    
    %% Test evolved patterns
    NewResults = lists:map(fun(Error) ->
        self_evolving_error_handler:handle_error(test_mod, test_fun, Error)
    end, Errors),
    
    io:format("  - Evolved handling results: ~p~n", [NewResults]),
    
    gen_server:stop(Pid).

test_distributed_coordination() ->
    %% Start distributed error coordinator
    {ok, Pid} = distributed_error_coordinator:start_link(),
    
    %% Report errors from different "nodes"
    Nodes = [node1@localhost, node2@localhost, node3@localhost],
    
    lists:foreach(fun(Node) ->
        distributed_error_coordinator:report_error(
            Node,
            test_module,
            test_function,
            {timeout, connection_lost}
        )
    end, Nodes),
    
    %% Get cluster health
    Health = distributed_error_coordinator:get_cluster_health(),
    io:format("  - Cluster health: ~p~n", [Health]),
    
    %% Coordinate a fix across nodes
    ErrorPattern = #{
        module => test_module,
        function => test_function,
        error_type => timeout
    },
    
    FixResult = distributed_error_coordinator:coordinate_fix(
        ErrorPattern,
        retry_with_backoff,
        [node()]  % Just current node for testing
    ),
    
    io:format("  - Distributed fix result: ~p~n", [FixResult]),
    
    %% Sync patterns
    distributed_error_coordinator:sync_patterns(),
    io:format("  - Pattern synchronization completed~n"),
    
    gen_server:stop(Pid).

%% Demonstration of the self-healing capabilities
demonstrate_self_healing() ->
    io:format("~n🎯 Live Demonstration of Self-Healing System~n"),
    
    %% Create a faulty server that will crash
    spawn(fun() ->
        register(faulty_server, self()),
        faulty_server_loop(0)
    end),
    
    %% Normal call - will crash
    io:format("~nAttempting normal call to faulty server...~n"),
    try
        gen_server:call(faulty_server, increment, 1000)
    catch
        Type:Reason ->
            io:format("❌ Error: ~p:~p~n", [Type, Reason])
    end,
    
    %% Auto-healing call - will handle the error
    io:format("~nAttempting auto-healing call to faulty server...~n"),
    Result = error_resilience:auto_healing_call(
        faulty_server,
        increment,
        1000,
        {ok, healed_value}
    ),
    io:format("✅ Auto-healed result: ~p~n", [Result]).

faulty_server_loop(Count) when Count > 2 ->
    %% Crash after 3 calls
    exit(intentional_crash);
faulty_server_loop(Count) ->
    receive
        {From, increment} ->
            gen_server:reply(From, {ok, Count + 1}),
            faulty_server_loop(Count + 1)
    end.