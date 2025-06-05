#!/usr/bin/env escript

main(_) ->
    io:format("🚀 Starting Agents.ERL with 30+ Specialized Agents...~n~n"),
    
    %% Start applications in order
    application:ensure_all_started(crypto),
    application:ensure_all_started(ssl),
    application:ensure_all_started(inets),
    application:ensure_all_started(jsx),
    application:ensure_all_started(cowboy),
    
    %% Start our applications
    {ok, _} = application:ensure_all_started(openai),
    {ok, _} = application:ensure_all_started(agents),
    {ok, _} = application:ensure_all_started(agent_web),
    
    timer:sleep(2000), %% Give time for systems to start
    
    %% Initialize the comprehensive agent fleet
    io:format("📦 Initializing comprehensive agent fleet with gpt-4.1 models...~n"),
    try
        Results = agent_initializer:init_default_agents(),
        
        %% Count results
        Ok = [R || R = {ok, _, _} <- Results],
        Errors = [R || R = {error, _, _} <- Results],
        Skipped = [R || R = {skipped, _, _} <- Results],
        
        io:format("~n✅ Agent Fleet Initialization Complete:~n"),
        io:format("  🟢 ~p agents created successfully~n", [length(Ok)]),
        io:format("  🔴 ~p agents failed to create~n", [length(Errors)]),
        io:format("  🟡 ~p agents skipped (already exist)~n", [length(Skipped)]),
        
        %% Show error details if any
        case Errors of
            [] -> ok;
            _ -> 
                io:format("~n❌ Error details:~n"),
                lists:foreach(fun({error, Name, Reason}) ->
                    io:format("  - ~s: ~p~n", [Name, Reason])
                end, Errors)
        end,
        
        %% Get final agent count
        timer:sleep(1000),
        AllAgents = agent_registry:list_agents(),
        io:format("~n🤖 Total active agents: ~p~n", [length(AllAgents)]),
        
        %% Show some examples
        io:format("~n📋 Agent Examples (showing first 10):~n"),
        Examples = lists:sublist(AllAgents, 10),
        lists:foreach(fun({Id, _Pid, Meta}) ->
            Name = maps:get(name, Meta, "Unknown"),
            Model = maps:get(model, Meta, "default"),
            IdStr = case Id of
                B when is_binary(B) -> binary_to_list(B);
                _ -> io_lib:format("~p", [Id])
            end,
            io:format("  • ~s (~s) [Model: ~s]~n", [Name, IdStr, Model])
        end, Examples),
        
        %% Success check
        if 
            length(AllAgents) >= 30 ->
                io:format("~n🎉 SUCCESS: Fleet has ~p agents (30+ target achieved!)~n", [length(AllAgents)]);
            true ->
                io:format("~n⚠️  Note: Currently ~p agents active (target was 30+)~n", [length(AllAgents)])
        end
        
    catch
        Error:Reason ->
            io:format("~n❌ Error during agent initialization: ~p:~p~n", [Error, Reason])
    end,
    
    io:format("~n🌐 Web interface available at: http://localhost:8080~n"),
    io:format("🚀 Agent chat and management ready!~n"),
    io:format("~n📱 Available endpoints:~n"),
    io:format("  • Dashboard: http://localhost:8080~n"),
    io:format("  • Agents API: http://localhost:8080/api/agents~n"),
    io:format("  • WebSocket Chat: ws://localhost:8080/ws~n"),
    io:format("~nPress Ctrl+C to stop the system.~n~n"),
    
    %% Keep running
    receive
        stop -> ok
    end.