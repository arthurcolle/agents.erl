#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin _build/default/lib/*/ebin

-mode(compile).

main(_) ->
    io:format("Testing Graphlit MCP Integration...~n"),
    
    % Check if MCP servers.json file was created
    case file:read_file("apps/agent_web/priv/mcp_servers.json") of
        {ok, ConfigData} ->
            io:format("✓ MCP servers configuration file created~n"),
            % Check if the file contains the key components
            DataStr = binary_to_list(ConfigData),
            case {string:find(DataStr, "graphlit"), 
                  string:find(DataStr, "GRAPHLIT_ORGANIZATION_ID"),
                  string:find(DataStr, "GRAPHLIT_ENVIRONMENT_ID"),
                  string:find(DataStr, "GRAPHLIT_JWT_SECRET")} of
                {nomatch, _, _, _} ->
                    io:format("✗ Graphlit configuration not found~n");
                {_, nomatch, _, _} ->
                    io:format("✗ Organization ID not found~n");
                {_, _, nomatch, _} ->
                    io:format("✗ Environment ID not found~n");
                {_, _, _, nomatch} ->
                    io:format("✗ JWT Secret not found~n");
                {_, _, _, _} ->
                    io:format("✓ All required Graphlit configuration found~n")
            end;
        {error, _} ->
            io:format("✗ MCP servers configuration file not found~n")
    end,
    
    io:format("~nGraphlit MCP Integration Test Complete!~n"),
    io:format("~nNext steps:~n"),
    io:format("1. Start the web application: ./start_web.sh~n"),
    io:format("2. Access the MCP dashboard to see Graphlit server~n"),
    io:format("3. Test Graphlit tools via the web interface~n"),
    halt(0).