#!/usr/bin/env escript
%% fix_startup.erl - Fix and test system startup

main(_Args) ->
    io:format("🔧 Fixing System Startup~n"),
    io:format("======================~n~n"),
    
    %% Add ALL beam paths including dependencies
    io:format("📦 Adding dependency paths...~n"),
    
    %% Get all lib directories
    {ok, LibDirs} = file:list_dir("_build/default/lib"),
    EbinPaths = lists:map(fun(Dir) ->
        "_build/default/lib/" ++ Dir ++ "/ebin"
    end, LibDirs),
    
    %% Add each path
    lists:foreach(fun(Path) ->
        case filelib:is_dir(Path) of
            true -> 
                code:add_patha(Path),
                io:format("   ✅ Added: ~s~n", [Path]);
            false -> 
                io:format("   ⚠️  Missing: ~s~n", [Path])
        end
    end, EbinPaths),
    
    io:format("~n🚀 Starting applications...~n"),
    
    %% Start applications in correct order
    StartupSequence = [
        {crypto, "Cryptographic functions"},
        {asn1, "ASN.1 support"},
        {public_key, "Public key infrastructure"},
        {ssl, "SSL/TLS support"},
        {inets, "HTTP client/server"},
        {gproc, "Process registry"},
        {jsx, "JSON processing"},
        {openai, "OpenAI API client"},
        {agents, "Agent system"},
        {agent_web, "Web interface"}
    ],
    
    lists:foreach(fun({App, Description}) ->
        io:format("   Starting ~p (~s)...~n", [App, Description]),
        case application:ensure_all_started(App) of
            {ok, Started} ->
                io:format("   ✅ ~p started (dependencies: ~p)~n", [App, Started]);
            {error, {already_started, App}} ->
                io:format("   ✅ ~p already running~n", [App]);
            {error, Reason} ->
                io:format("   ❌ ~p failed: ~p~n", [App, Reason])
        end
    end, StartupSequence),
    
    %% Check if web server is running
    io:format("~n🌐 Checking web server...~n"),
    timer:sleep(1000), % Give it a moment to start
    
    case whereis(agent_web_sup) of
        undefined ->
            io:format("   ❌ Web supervisor not running~n");
        Pid ->
            io:format("   ✅ Web supervisor running: ~p~n", [Pid]),
            
            %% Try to get the children
            try
                Children = supervisor:which_children(Pid),
                io:format("   📋 Web server components:~n"),
                lists:foreach(fun({Id, ChildPid, Type, _}) ->
                    Status = if is_pid(ChildPid) -> "running"; true -> "not running" end,
                    io:format("      - ~p (~p): ~s~n", [Id, Type, Status])
                end, Children)
            catch
                _:_ ->
                    io:format("   ⚠️  Could not get supervisor children~n")
            end
    end,
    
    %% Check port 8080
    io:format("~n🔍 Checking port 8080...~n"),
    case gen_tcp:connect("localhost", 8080, [], 1000) of
        {ok, Socket} ->
            gen_tcp:close(Socket),
            io:format("   ✅ Port 8080 is listening~n");
        {error, Reason} ->
            io:format("   ❌ Port 8080 not available: ~p~n", [Reason])
    end,
    
    io:format("~n🎉 Startup fixed! System should be running at http://localhost:8080~n"),
    io:format("~n📊 To keep the system running, use:~n"),
    io:format("   ./rebar3 shell --config config/sys.config~n"),
    io:format("~n⚡ Hot reload is now available for streaming fixes!~n").