#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/*/ebin -Wall

-mode(compile).

main([]) ->
    io:format("Testing MCP Connection Manager with stdio fix...~n~n"),
    
    % Start required applications
    application:ensure_all_started(crypto),
    application:ensure_all_started(asn1),
    application:ensure_all_started(public_key),
    application:ensure_all_started(ssl),
    application:ensure_all_started(jsx),
    application:ensure_all_started(gun),
    application:ensure_all_started(cowlib),
    
    % Start mnesia
    mnesia:start(),
    
    % Initialize the configuration
    mcp_server_config:init(),
    
    % Start the registry
    {ok, _RegistryPid} = mcp_registry:start_link(),
    io:format("✓ Registry started~n"),
    
    % Start the connection manager
    {ok, _ConnMgrPid} = mcp_connection_manager:start_link(),
    io:format("✓ Connection manager started~n"),
    
    % Get a stdio server from config (filesystem)
    case mcp_server_config:get_server(<<"filesystem_mcp">>) of
        {ok, ServerRecord} ->
            io:format("~nFound filesystem server in config~n"),
            
            % Extract fields from record
            ServerId = element(2, ServerRecord),
            ServerName = element(3, ServerRecord),
            ServerUrl = element(5, ServerRecord),
            Metadata = element(12, ServerRecord),
            
            io:format("  ID: ~s~n", [ServerId]),
            io:format("  Name: ~s~n", [ServerName]),
            io:format("  URL: ~s~n", [ServerUrl]),
            io:format("  Transport: ~p~n", [maps:get(<<"transport">>, Metadata, <<"not specified">>)]),
            
            % Register with the registry
            ServerConfig = #{
                id => ServerId,
                name => ServerName,
                url => ServerUrl,
                metadata => Metadata
            },
            
            case mcp_registry:register_server(ServerName, ServerUrl, ServerConfig) of
                {ok, RegistryId} ->
                    io:format("~n✓ Server registered with ID: ~s~n", [RegistryId]),
                    
                    % Now try to connect
                    io:format("~nAttempting connection...~n"),
                    case mcp_connection_manager:connect_server(RegistryId) of
                        {ok, Pid} ->
                            io:format("✓ Connection successful! Client PID: ~p~n", [Pid]),
                            
                            % Give it a moment to initialize
                            timer:sleep(2000),
                            
                            % Check connection status
                            case mcp_connection_manager:get_connection(RegistryId) of
                                {ok, {Pid, Status, _Config}} ->
                                    io:format("✓ Connection status: ~p~n", [Status]);
                                {error, Reason} ->
                                    io:format("✗ Failed to get connection: ~p~n", [Reason])
                            end;
                        {error, Reason} ->
                            io:format("✗ Connection failed: ~p~n", [Reason])
                    end;
                {error, RegError} ->
                    io:format("✗ Failed to register server: ~p~n", [RegError])
            end;
        {error, not_found} ->
            io:format("✗ Filesystem server not found in config~n")
    end,
    
    % Also test a network server for comparison
    io:format("~n~nTesting network server (Zapier)...~n"),
    case mcp_server_config:get_server(<<"zapier">>) of
        {ok, ZapierRecord} ->
            ZapierId = element(2, ZapierRecord),
            ZapierName = element(3, ZapierRecord),
            ZapierUrl = element(5, ZapierRecord),
            ZapierMetadata = element(12, ZapierRecord),
            
            io:format("  URL: ~s~n", [ZapierUrl]),
            
            ZapierConfig = #{
                id => ZapierId,
                name => ZapierName,
                url => ZapierUrl,
                metadata => ZapierMetadata
            },
            
            case mcp_registry:register_server(ZapierName, ZapierUrl, ZapierConfig) of
                {ok, ZapierRegId} ->
                    io:format("✓ Zapier registered with ID: ~s~n", [ZapierRegId]);
                {error, ZapierRegError} ->
                    io:format("✗ Failed to register Zapier: ~p~n", [ZapierRegError])
            end;
        {error, _} ->
            io:format("✗ Zapier server not found~n")
    end,
    
    io:format("~nTest complete!~n"),
    halt(0).