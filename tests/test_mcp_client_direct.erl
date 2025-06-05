#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/*/ebin -Wall

-mode(compile).

main([]) ->
    io:format("Testing MCP Client directly with stdio transport...~n~n"),
    
    % Start required applications
    application:ensure_all_started(crypto),
    application:ensure_all_started(asn1),
    application:ensure_all_started(public_key),
    application:ensure_all_started(ssl),
    application:ensure_all_started(jsx),
    application:ensure_all_started(gun),
    application:ensure_all_started(cowlib),
    
    % Create a test server config for filesystem with stdio transport
    ServerConfig = #{
        id => <<"test_filesystem">>,
        name => <<"Test Filesystem">>,
        url => <<"npx -y @modelcontextprotocol/server-filesystem">>,
        metadata => #{
            <<"transport">> => <<"stdio">>,
            <<"command">> => <<"npx">>,
            <<"args">> => [<<"-y">>, <<"@modelcontextprotocol/server-filesystem">>, <<"/tmp">>]
        }
    },
    
    io:format("Starting MCP client with config:~n"),
    io:format("  URL: ~s~n", [maps:get(url, ServerConfig)]),
    io:format("  Transport: ~s~n", [maps:get(<<"transport">>, maps:get(metadata, ServerConfig, #{}), <<"unknown">>)]),
    
    % Start the client
    case mcp_client_v2:start_link(<<"test_filesystem">>, ServerConfig) of
        {ok, ClientPid} ->
            io:format("~n✓ Client started with PID: ~p~n", [ClientPid]),
            
            % Try to connect
            io:format("~nAttempting to connect...~n"),
            case mcp_client_v2:connect(ClientPid) of
                {ok, ConnInfo} ->
                    io:format("✓ Connection successful!~n"),
                    io:format("  Connection info: ~p~n", [ConnInfo]),
                    
                    % Try to list tools
                    io:format("~nListing available tools...~n"),
                    case mcp_client_v2:list_tools(ClientPid) of
                        {ok, Tools} ->
                            io:format("✓ Tools: ~p~n", [Tools]);
                        {error, ToolError} ->
                            io:format("✗ Failed to list tools: ~p~n", [ToolError])
                    end;
                {error, ConnError} ->
                    io:format("✗ Connection failed: ~p~n", [ConnError])
            end,
            
            % Clean up
            mcp_client_v2:stop(ClientPid);
        {error, StartError} ->
            io:format("✗ Failed to start client: ~p~n", [StartError])
    end,
    
    io:format("~nTest complete!~n"),
    halt(0).