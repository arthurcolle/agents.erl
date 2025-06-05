#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/agents/ebin -pa _build/default/lib/agent_web/ebin -pa _build/default/lib/openai/ebin

-mode(compile).

main([]) ->
    io:format("🚀 Testing Self-Fixing Error System...~n~n"),
    
    %% Start required applications
    application:start(crypto),
    
    %% Test 1: Basic Error Resilience
    io:format("Test 1: Error Resilience~n"),
    test_basic_resilience(),
    
    %% Test 2: Auto-healing with existing modules
    io:format("~nTest 2: Auto-Healing System~n"),
    test_auto_healing_system(),
    
    io:format("~n✅ Tests completed!~n").

test_basic_resilience() ->
    %% Test safe_call with non-existent server
    Result1 = error_resilience:safe_call(
        non_existent_server, 
        {get_data, []}, 
        5000,
        {ok, default_data}
    ),
    io:format("  - Dead server call result: ~p~n", [Result1]),
    
    %% Test division protection
    Result2 = error_resilience:protect_division(10, 0),
    io:format("  - Division by zero result: ~p~n", [Result2]),
    
    %% Test memory calculation protection
    Result3 = error_resilience:protect_memory_calc(0, 0),
    io:format("  - Memory calc with zero total: ~p~n", [Result3]),
    
    %% Test with retry
    Result4 = error_resilience:with_retry(
        fun() -> 
            case rand:uniform(2) of
                1 -> error(random_failure);
                2 -> {ok, success}
            end
        end,
        5,  % Max retries
        100  % Delay
    ),
    io:format("  - Retry result: ~p~n", [Result4]),
    
    %% Test with fallback
    Result5 = error_resilience:with_fallback(
        fun() -> error(intentional_error) end,
        {ok, fallback_value}
    ),
    io:format("  - Fallback result: ~p~n", [Result5]).

test_auto_healing_system() ->
    %% Check if auto_healing_startup is running
    case whereis(auto_healing_startup) of
        undefined ->
            io:format("  - Auto-healing system not running~n");
        Pid ->
            io:format("  - Auto-healing system running at ~p~n", [Pid])
    end,
    
    %% Test the self-healing system if available
    case code:is_loaded(self_healing_system) of
        false ->
            io:format("  - Self-healing system module not loaded~n");
        _ ->
            io:format("  - Self-healing system module is loaded~n"),
            test_self_healing_features()
    end.

test_self_healing_features() ->
    %% Create a process that will fail
    TestPid = spawn(fun() ->
        register(test_failing_process, self()),
        receive
            crash -> exit(intentional_crash);
            {From, ping} -> From ! pong
        end
    end),
    
    io:format("  - Created test process: ~p~n", [TestPid]),
    
    %% Test normal operation
    TestPid ! {self(), ping},
    receive
        pong -> io:format("  - Process responding normally~n")
    after 1000 ->
        io:format("  - Process not responding~n")
    end,
    
    %% Make it crash
    TestPid ! crash,
    timer:sleep(100),
    
    %% Check if process is dead
    case is_process_alive(TestPid) of
        true -> io:format("  - Process still alive (unexpected)~n");
        false -> io:format("  - Process crashed as expected~n")
    end,
    
    %% In a real self-healing system, the process would be restarted
    %% For now, we just demonstrate the concept
    case whereis(test_failing_process) of
        undefined -> 
            io:format("  - Process not registered (would be restarted by healing system)~n");
        NewPid -> 
            io:format("  - Process restarted by healing system: ~p~n", [NewPid])
    end.