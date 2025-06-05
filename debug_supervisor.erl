#!/usr/bin/env escript
%% debug_supervisor.erl
%% Debug what's wrong with the supervisor startup

main(_Args) ->
    io:format("🔍 Debugging Supervisor Issues~n"),
    io:format("==============================~n~n"),
    
    %% Add all the paths
    code:add_paths([
        "_build/default/lib/agents/ebin", 
        "_build/default/lib/openai/ebin", 
        "_build/default/lib/agent_web/ebin",
        "_build/default/lib/jsx/ebin",
        "_build/default/lib/cowboy/ebin",
        "_build/default/lib/cowlib/ebin",
        "_build/default/lib/ranch/ebin"
    ]),
    
    %% Test loading key modules
    ModulesToTest = [
        mcp_registry,
        mcp_connection_manager, 
        mcp_manager,
        mcp_advanced_config,
        oauth_manager,
        mcp_monitor,
        colored_logger,
        persistent_table_manager
    ],
    
    io:format("🧪 Testing module loading:~n"),
    lists:foreach(fun(Module) ->
        io:format("   ~p: ", [Module]),
        case code:ensure_loaded(Module) of
            {module, Module} ->
                io:format("✅ OK~n");
            {error, Error} ->
                io:format("❌ ERROR: ~p~n", [Error])
        end
    end, ModulesToTest),
    
    io:format("~n🔧 Testing module exports:~n"),
    TestStart = fun(Module) ->
        io:format("   ~p start_link/0: ", [Module]),
        case erlang:function_exported(Module, start_link, 0) of
            true ->
                io:format("✅ exported~n");
            false ->
                io:format("❌ NOT exported~n")
        end
    end,
    
    lists:foreach(TestStart, ModulesToTest),
    
    %% Try to manually start one of the problematic modules
    io:format("~n🚀 Testing manual startup of mcp_registry:~n"),
    try
        case mcp_registry:start_link() of
            {ok, Pid} ->
                io:format("   ✅ mcp_registry started successfully with PID ~p~n", [Pid]),
                %% Clean up
                exit(Pid, normal);
            {error, Reason} ->
                io:format("   ❌ mcp_registry failed to start: ~p~n", [Reason])
        end
    catch
        Error:Reason2 ->
            io:format("   ❌ Exception during startup: ~p:~p~n", [Error, Reason2])
    end,
    
    io:format("~n💡 If modules load but don't start, the issue is likely:~n"),
    io:format("   • Missing dependencies during gen_server init~n"),
    io:format("   • Logger configuration issues~n"),
    io:format("   • ETS table conflicts~n"),
    io:format("   • Application environment not set~n").