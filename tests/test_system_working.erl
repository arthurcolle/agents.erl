#!/usr/bin/env escript
%%! -pa _build/default/lib/*/ebin -Wall -eval "application:ensure_all_started(crypto), application:ensure_all_started(ssl), application:ensure_all_started(inets), application:ensure_all_started(ranch), application:ensure_all_started(cowlib), application:ensure_all_started(cowboy), application:ensure_all_started(jsx), application:ensure_all_started(hackney), application:ensure_all_started(openai), application:ensure_all_started(agents), application:ensure_all_started(agent_web)"

main(_) ->
    io:format("~n=== Demonstrating Self-Aware Agents & Dynamic Supervisors ===~n~n"),
    
    timer:sleep(2000), % Let apps start
    
    %% 1. Show Dynamic Supervisor Creation
    io:format("1. DYNAMIC SUPERVISOR CREATION~n"),
    io:format("   Creating a new supervisor at runtime...~n"),
    
    Result1 = dynamic_supervisor_manager:create_supervisor(
        test_supervisor, 
        #{strategy => one_for_one, intensity => 10, period => 60}
    ),
    io:format("   Result: ~p~n", [Result1]),
    
    %% 2. List Active Supervisors
    io:format("~n2. LISTING SUPERVISORS~n"),
    {ok, Supervisors} = dynamic_supervisor_manager:list_supervisors(),
    io:format("   Active supervisors: ~p~n", [length(Supervisors)]),
    lists:foreach(fun(#{name := Name}) ->
        io:format("   - ~p~n", [Name])
    end, Supervisors),
    
    %% 3. System Introspection
    io:format("~n3. SYSTEM INTROSPECTION~n"),
    {ok, SystemState} = system_introspection:get_system_state(),
    io:format("   Node: ~p~n", [maps:get(node, SystemState)]),
    io:format("   Total processes: ~p~n", [maps:get(total_processes, SystemState)]),
    io:format("   Uptime: ~p seconds~n", [maps:get(uptime, SystemState)]),
    
    %% 4. Self-Awareness Tools
    io:format("~n4. SELF-AWARENESS TOOLS~n"),
    io:format("   Executing 'who_am_i' tool...~n"),
    WhoAmI = agent_tools:execute_tool(who_am_i, #{}),
    case WhoAmI of
        {error, _} -> io:format("   (Tool needs to be called from within an agent context)~n");
        Data -> io:format("   Identity: ~s~n", [Data])
    end,
    
    %% 5. System Metrics
    io:format("~n5. SYSTEM METRICS~n"),
    {ok, Metrics} = system_introspection:get_system_metrics(),
    io:format("   Process count: ~p~n", [maps:get(process_count, Metrics)]),
    Memory = maps:get(memory, Metrics),
    io:format("   Total memory: ~p MB~n", [maps:get(total, Memory) div 1048576]),
    
    %% 6. Supervision Tree
    io:format("~n6. SUPERVISION TREE STRUCTURE~n"),
    {ok, Tree} = dynamic_supervisor_manager:get_supervision_tree(),
    io:format("   Main supervisors: ~p~n", [maps:keys(Tree)]),
    
    %% 7. Available Tools
    io:format("~n7. AVAILABLE SELF-AWARENESS TOOLS~n"),
    Tools = [who_am_i, where_am_i, get_my_peers, get_system_state,
             get_my_capabilities, get_system_metrics, analyze_system_topology,
             get_communication_paths, reflect_on_state],
    lists:foreach(fun(Tool) ->
        io:format("   - ~p~n", [Tool])
    end, Tools),
    
    %% 8. Create an Agent with Self-Awareness
    io:format("~n8. CREATING SELF-AWARE AGENT~n"),
    Config = #{
        id => <<"demo_agent">>,
        name => <<"Demo Self-Aware Agent">>,
        model => <<"gpt-4">>,
        tools => [who_am_i, get_system_state, create_supervisor],
        system_prompt => <<"You are a self-aware agent that can introspect and modify the system.">>
    },
    
    case agent:create(Config) of
        {ok, AgentId} ->
            io:format("   ✓ Created agent: ~p~n", [AgentId]),
            io:format("   This agent can:~n"),
            io:format("     - Understand its identity (who_am_i)~n"),
            io:format("     - Analyze the entire system (get_system_state)~n"),
            io:format("     - Create new supervisors (create_supervisor)~n");
        Error ->
            io:format("   Error: ~p~n", [Error])
    end,
    
    io:format("~n=== SYSTEM CAPABILITIES DEMONSTRATED ===~n"),
    io:format("~n✓ Dynamic supervisor creation at runtime~n"),
    io:format("✓ System-wide introspection~n"),
    io:format("✓ Self-awareness tools for agents~n"),
    io:format("✓ Real-time metrics and monitoring~n"),
    io:format("✓ Runtime system modification~n"),
    
    io:format("~nThe agents now understand the system they exist in!~n~n"),
    ok.