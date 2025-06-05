#!/usr/bin/env escript

main(_) ->
    %% Add code paths for all dependencies
    LibDir = "_build/default/lib",
    {ok, Deps} = file:list_dir(LibDir),
    [code:add_pathz(filename:join([LibDir, Dep, "ebin"])) || Dep <- Deps],
    
    %% Add our application paths
    code:add_pathz("apps/openai/ebin"),
    code:add_pathz("apps/agents/ebin"), 
    code:add_pathz("apps/agent_web/ebin"),
    
    %% Try to start applications manually
    try
        ok = application:start(crypto),
        ok = application:start(asn1),
        ok = application:start(public_key), 
        ok = application:start(ssl),
        ok = application:start(inets),
        ok = application:start(ranch),
        ok = application:start(cowlib),
        ok = application:start(cowboy),
        ok = application:start(jsx),
        io:format("✓ Basic dependencies started successfully~n"),
        
        %% Try to start our applications
        ok = application:start(openai),
        io:format("✓ OpenAI application started~n"),
        
        ok = application:start(agents),
        io:format("✓ Agents application started~n"),
        
        ok = application:start(agent_web),
        io:format("✓ Agent web application started~n"),
        
        io:format("🚀 All applications started successfully!~n"),
        io:format("🌐 Web server should be running on http://localhost:8080~n"),
        
        %% Keep alive for a moment
        timer:sleep(2000),
        
        %% Stop applications
        application:stop(agent_web),
        application:stop(agents), 
        application:stop(openai),
        
        io:format("✅ Startup test completed successfully!~n")
        
    catch
        Type:Error:Stack ->
            io:format("❌ Startup failed: ~p:~p~n", [Type, Error]),
            io:format("Stack: ~p~n", [Stack]),
            halt(1)
    end.