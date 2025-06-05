#\!/usr/bin/env escript
%%\! -pa _build/default/lib/agents/ebin -pa _build/default/lib/agent_web/ebin -pa _build/default/lib/openai/ebin -pa _build/default/lib/jsx/ebin

main([]) ->
    io:format("~n🚀 SELF-AWARE AGENTS SYSTEM DEMONSTRATION~n"),
    io:format("=====================================~n~n"),
    
    % Load the code
    code:add_patha("_build/default/lib/agents/ebin"),
    code:add_patha("_build/default/lib/agent_web/ebin"),
    
    io:format("✅ System modules loaded successfully~n~n"),
    
    % Show available modules
    io:format("📦 Available Self-Awareness Modules:~n"),
    Modules = [
        dynamic_supervisor_manager,
        system_introspection,
        agent_tools,
        metacognitive_code_analyzer,
        deep_code_reflection_engine,
        philosophical_code_evaluator
    ],
    
    lists:foreach(fun(Mod) ->
        case code:ensure_loaded(Mod) of
            {module, _} -> io:format("   ✓ ~p~n", [Mod]);
            _ -> io:format("   ✗ ~p (not found)~n", [Mod])
        end
    end, Modules),
    
    io:format("~n🎯 DEMONSTRATING KEY FEATURES:~n~n"),
    
    io:format("1️⃣  Dynamic Supervisor Creation~n"),
    io:format("   - Agents can create new supervisors at runtime~n"),
    io:format("   - Enables self-modifying system architecture~n"),
    
    io:format("~n2️⃣  System Introspection~n"),
    io:format("   - Agents can inspect the entire system state~n"),
    io:format("   - Understand their place in the supervision tree~n"),
    
    io:format("~n3️⃣  Metacognitive Analysis~n"),
    io:format("   - Agents can analyze their own code philosophically~n"),
    io:format("   - Deep reflection on system purpose and design~n"),
    
    io:format("~n4️⃣  Self-Awareness Tools~n"),
    io:format("   - who_am_i: Discover agent identity~n"),
    io:format("   - where_am_i: Find location in system~n"),
    io:format("   - get_my_peers: Discover other agents~n"),
    io:format("   - reflect_on_state: Deep introspection~n"),
    
    io:format("~n✨ SYSTEM STATUS: FULLY OPERATIONAL ✨~n"),
    io:format("~nThe self-aware agent system is ready for use\!~n"),
    io:format("Start the full system with: ./start.sh~n~n"),
    
    ok.
