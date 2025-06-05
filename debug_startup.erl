#!/usr/bin/env escript
%% debug_startup.erl - Debug system startup issues

main(_Args) ->
    io:format("🔍 Debugging System Startup~n"),
    io:format("=========================~n~n"),
    
    %% Add beam paths
    BeamPaths = [
        "_build/default/lib/openai/ebin",
        "_build/default/lib/agents/ebin", 
        "_build/default/lib/agent_web/ebin",
        "_build/default/lib/openapi_scaffold/ebin"
    ],
    
    lists:foreach(fun(Path) ->
        code:add_patha(Path)
    end, BeamPaths),
    
    %% Test configuration
    io:format("📋 1. Configuration Test~n"),
    case file:consult("config/sys.config") of
        {ok, _Config} ->
            io:format("   ✅ sys.config is valid~n");
        {error, ConfigError} ->
            io:format("   ❌ sys.config error: ~p~n", [ConfigError]),
            halt(1)
    end,
    
    %% Test application files
    io:format("~n📦 2. Application Files Test~n"),
    Apps = [
        {"_build/default/lib/openai/ebin/openai.app", openai},
        {"_build/default/lib/agents/ebin/agents.app", agents},
        {"_build/default/lib/agent_web/ebin/agent_web.app", agent_web}
    ],
    
    lists:foreach(fun({AppFile, AppName}) ->
        case file:consult(AppFile) of
            {ok, [{application, AppName, Props}]} ->
                io:format("   ✅ ~p app file is valid~n", [AppName]);
            {error, AppError} ->
                io:format("   ❌ ~p app file error: ~p~n", [AppName, AppError]);
            Other ->
                io:format("   ⚠️  ~p app file format: ~p~n", [AppName, Other])
        end
    end, Apps),
    
    %% Test key modules
    io:format("~n🔧 3. Key Modules Test~n"),
    KeyModules = [
        agent_web_app,
        agent_web_sup, 
        agents_app,
        openai_app
    ],
    
    lists:foreach(fun(Module) ->
        case code:ensure_loaded(Module) of
            {module, Module} ->
                io:format("   ✅ ~p loaded~n", [Module]);
            {error, Reason} ->
                io:format("   ❌ ~p failed: ~p~n", [Module, Reason])
        end
    end, KeyModules),
    
    %% Test startup sequence
    io:format("~n🚀 4. Startup Sequence Test~n"),
    try
        %% Start dependencies
        io:format("   Starting inets...~n"),
        case application:ensure_all_started(inets) of
            {ok, _} -> io:format("   ✅ inets started~n");
            Error1 -> io:format("   ❌ inets failed: ~p~n", [Error1])
        end,
        
        io:format("   Starting crypto...~n"),
        case application:ensure_all_started(crypto) of
            {ok, _} -> io:format("   ✅ crypto started~n");
            Error2 -> io:format("   ❌ crypto failed: ~p~n", [Error2])
        end,
        
        io:format("   Starting openai...~n"),
        case application:ensure_all_started(openai) of
            {ok, _} -> io:format("   ✅ openai started~n");
            Error3 -> io:format("   ❌ openai failed: ~p~n", [Error3])
        end,
        
        io:format("   Starting agents...~n"),
        case application:ensure_all_started(agents) of
            {ok, _} -> io:format("   ✅ agents started~n");
            Error4 -> io:format("   ❌ agents failed: ~p~n", [Error4])
        end,
        
        io:format("   Starting agent_web...~n"),
        case application:ensure_all_started(agent_web) of
            {ok, _} -> 
                io:format("   ✅ agent_web started~n"),
                io:format("~n🎉 SUCCESS: All applications started!~n"),
                io:format("🌐 Web interface should be available at http://localhost:8080~n");
            Error5 -> 
                io:format("   ❌ agent_web failed: ~p~n", [Error5])
        end
        
    catch
        E:R:S ->
            io:format("   ❌ Startup exception: ~p:~p~n", [E, R]),
            io:format("   Stack: ~p~n", [S])
    end,
    
    io:format("~n✅ Startup debug completed~n").