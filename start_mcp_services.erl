#!/usr/bin/env escript
%% start_mcp_services.erl
%% Start MCP services manually to fix health check issues

main(_Args) ->
    io:format("🚀 Starting MCP Services~n"),
    io:format("=======================~n~n"),
    
    %% Add paths
    code:add_paths([
        "_build/default/lib/agents/ebin", 
        "_build/default/lib/openai/ebin", 
        "_build/default/lib/agent_web/ebin",
        "_build/default/lib/jsx/ebin",
        "_build/default/lib/cowboy/ebin",
        "_build/default/lib/cowlib/ebin",
        "_build/default/lib/ranch/ebin"
    ]),
    
    %% Initialize applications that might be needed
    application:ensure_all_started(crypto),
    application:ensure_all_started(ssl),
    
    %% Start services in dependency order
    Services = [
        {persistent_table_manager, "Table manager"},
        {mcp_registry, "MCP registry"}, 
        {mcp_advanced_config, "MCP advanced config"},
        {mcp_connection_manager, "MCP connection manager"},
        {mcp_manager, "MCP manager"},
        {oauth_manager, "OAuth manager"},
        {mcp_monitor, "MCP monitor"}
    ],
    
    StartedPids = lists:foldl(fun({Service, Description}, Acc) ->
        io:format("Starting ~s (~p): ", [Description, Service]),
        try
            case Service:start_link() of
                {ok, Pid} ->
                    io:format("✅ PID ~p~n", [Pid]),
                    [{Service, Pid} | Acc];
                {error, {already_started, Pid}} ->
                    io:format("✅ Already running with PID ~p~n", [Pid]),
                    [{Service, Pid} | Acc];
                {error, Reason} ->
                    io:format("❌ Error: ~p~n", [Reason]),
                    Acc
            end
        catch
            Error:Reason2 ->
                io:format("❌ Exception: ~p:~p~n", [Error, Reason2]),
                Acc
        end
    end, [], Services),
    
    io:format("~n✅ Started ~p services successfully~n", [length(StartedPids)]),
    
    %% Test the services
    io:format("~n🧪 Testing services:~n"),
    try
        Servers = mcp_registry:list_servers(),
        io:format("   MCP Registry: ✅ ~p servers registered~n", [length(Servers)])
    catch
        _:_ ->
            io:format("   MCP Registry: ❌ Not responding~n")
    end,
    
    io:format("~n🎯 Services are now running and should appear in health check!~n").