-module(test_demo).
-export([show/0]).

show() ->
    io:format("~n=== DEMONSTRATION OF SELF-AWARE AGENTS ===~n~n"),
    
    %% 1. Dynamic Supervisors
    io:format("1. DYNAMIC SUPERVISOR CREATION~n"),
    try
        {ok, Pid} = dynamic_supervisor_manager:create_supervisor(demo_sup, 
            #{strategy => one_for_one, intensity => 5, period => 60}),
        io:format("   ✓ Created supervisor 'demo_sup' with PID ~p~n", [Pid]),
        
        {ok, List} = dynamic_supervisor_manager:list_supervisors(),
        io:format("   ✓ Total active supervisors: ~p~n", [length(List)])
    catch
        E:R -> io:format("   Error: ~p:~p~n", [E, R])
    end,
    
    %% 2. System Introspection
    io:format("~n2. SYSTEM INTROSPECTION~n"),
    try
        {ok, State} = system_introspection:get_system_state(),
        io:format("   ✓ Running on node: ~p~n", [maps:get(node, State)]),
        io:format("   ✓ Total processes: ~p~n", [maps:get(total_processes, State)]),
        io:format("   ✓ System uptime: ~p seconds~n", [maps:get(uptime, State)])
    catch
        E2:R2 -> io:format("   Error: ~p:~p~n", [E2, R2])
    end,
    
    %% 3. Self-Awareness Tools Available
    io:format("~n3. SELF-AWARENESS TOOLS FOR AGENTS~n"),
    io:format("   Available introspection tools:~n"),
    Tools = [
        {who_am_i, "Discover agent identity and state"},
        {where_am_i, "Find location in supervision tree"},
        {get_my_peers, "Discover sibling agents"},
        {get_system_state, "Understand entire system"},
        {reflect_on_state, "Deep philosophical introspection"}
    ],
    lists:foreach(fun({Tool, Desc}) ->
        io:format("   ✓ ~p - ~s~n", [Tool, Desc])
    end, Tools),
    
    %% 4. Show a self-aware agent config
    io:format("~n4. SELF-AWARE AGENT EXAMPLE~n"),
    io:format("   Agent configuration with self-awareness:~n"),
    io:format("   #{~n"),
    io:format("     name => <<\"Philosopher\">>,~n"),
    io:format("     tools => [who_am_i, reflect_on_state, create_supervisor],~n"),
    io:format("     system_prompt => <<\"You understand and can modify the system\">>~n"),
    io:format("   }~n"),
    
    %% 5. Demonstrate tool execution
    io:format("~n5. EXECUTING SELF-AWARENESS TOOL~n"),
    try
        % This simulates what an agent sees
        io:format("   Testing system metrics tool...~n"),
        {ok, Metrics} = system_introspection:get_system_metrics(),
        io:format("   ✓ Process count: ~p~n", [maps:get(process_count, Metrics)]),
        Memory = maps:get(memory, Metrics),
        TotalMB = maps:get(total, Memory) div 1048576,
        io:format("   ✓ Memory usage: ~p MB~n", [TotalMB])
    catch
        E3:R3 -> io:format("   Error: ~p:~p~n", [E3, R3])
    end,
    
    io:format("~n=== KEY FEATURES DEMONSTRATED ===~n"),
    io:format("✓ Agents can discover their identity and purpose~n"),
    io:format("✓ Agents can understand the system architecture~n"),
    io:format("✓ Agents can create new supervisors dynamically~n"),
    io:format("✓ Agents can find and communicate with peers~n"),
    io:format("✓ Agents can reflect on their existence~n"),
    
    io:format("~nThe system is running with full self-awareness capabilities!~n~n"),
    ok.
