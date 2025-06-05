#!/usr/bin/env escript

-module(test_agent_startup).

main(_) ->
    io:format("🚀 Starting Agent System with 30+ Agents...~n"),
    
    %% Start the application
    application:ensure_all_started(agents),
    application:ensure_all_started(openai),
    application:ensure_all_started(agent_web),
    
    %% Initialize default agents
    io:format("📦 Initializing comprehensive agent fleet...~n"),
    Results = agent_initializer:init_default_agents(),
    
    %% Print summary
    Ok = [R || R = {ok, _, _} <- Results],
    Errors = [R || R = {error, _, _} <- Results],
    Skipped = [R || R = {skipped, _, _} <- Results],
    
    io:format("✅ Agent initialization complete:~n"),
    io:format("  - ~p agents created successfully~n", [length(Ok)]),
    io:format("  - ~p agents failed to create~n", [length(Errors)]),
    io:format("  - ~p agents skipped (already exist)~n", [length(Skipped)]),
    
    %% List all agents
    AllAgents = agent_registry:list_agents(),
    io:format("🤖 Total active agents: ~p~n", [length(AllAgents)]),
    
    %% Print first few agents as examples
    io:format("~nAgent examples:~n"),
    lists:foreach(fun({Id, _Pid, Meta}) ->
        Name = maps:get(name, Meta, "Unknown"),
        Model = maps:get(model, Meta, "default"),
        io:format("  - ~s (~s) - Model: ~s~n", [Name, binary_to_list(Id), Model])
    end, lists:sublist(AllAgents, 5)),
    
    if 
        length(AllAgents) >= 30 ->
            io:format("🎉 SUCCESS: System has ~p agents (30+ target achieved!)~n", [length(AllAgents)]);
        true ->
            io:format("⚠️  WARNING: Only ~p agents active (target was 30+)~n", [length(AllAgents)])
    end,
    
    io:format("~n🌐 Web interface should be available at: http://localhost:8080~n"),
    io:format("Press Ctrl+C to stop the system.~n"),
    
    %% Keep the system running
    timer:sleep(infinity).