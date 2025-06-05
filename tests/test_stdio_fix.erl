#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/*/ebin -Wall

-mode(compile).

main([]) ->
    io:format("Testing MCP stdio transport fix...~n~n"),
    
    % Start required applications
    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssl),
    application:start(jsx),
    application:start(gun),
    
    % Test cases for URL detection
    TestCases = [
        {<<"npx -y @modelcontextprotocol/server-filesystem">>, true, <<"npx">>, [<<"-y">>, <<"@modelcontextprotocol/server-filesystem">>]},
        {<<"node /path/to/server.js">>, true, <<"node">>, [<<"/path/to/server.js">>]},
        {<<"python -m mcp_server">>, true, <<"python">>, [<<"-m">>, <<"mcp_server">>]},
        {<<"https://mcp.zapier.com/">>, false, undefined, []},
        {<<"ws://localhost:8767/mcp">>, false, undefined, []},
        {<<"http://example.com/api">>, false, undefined, []}
    ],
    
    io:format("Testing URL detection logic:~n"),
    lists:foreach(fun({Url, ExpectedIsCmd, ExpectedCmd, ExpectedArgs}) ->
        Result = is_command_url(Url),
        case Result of
            {true, Cmd, Args} when ExpectedIsCmd ->
                if Cmd =:= ExpectedCmd andalso Args =:= ExpectedArgs ->
                    io:format("  ✓ ~s -> Command: ~s, Args: ~p~n", [Url, Cmd, Args]);
                true ->
                    io:format("  ✗ ~s -> Expected ~s ~p, got ~s ~p~n", 
                              [Url, ExpectedCmd, ExpectedArgs, Cmd, Args])
                end;
            false when not ExpectedIsCmd ->
                io:format("  ✓ ~s -> Network URL~n", [Url]);
            _ ->
                io:format("  ✗ ~s -> Unexpected result: ~p~n", [Url, Result])
        end
    end, TestCases),
    
    io:format("~nTesting metadata-based transport detection:~n"),
    
    % Test with stdio metadata
    StdioConfig = #{
        url => <<"npx -y @modelcontextprotocol/server-filesystem">>,
        metadata => #{
            <<"transport">> => <<"stdio">>,
            <<"command">> => <<"npx">>,
            <<"args">> => [<<"-y">>, <<"@modelcontextprotocol/server-filesystem">>]
        }
    },
    
    StdioState = {state, undefined, <<"npx -y @modelcontextprotocol/server-filesystem">>, 
                  undefined, undefined, undefined, undefined, disconnected, 
                  #{}, StdioConfig, 1, #{}, #{}, <<>>},
    
    case determine_connection_type(StdioState) of
        {stdio, <<"npx">>, Args} ->
            io:format("  ✓ Stdio transport detected from metadata~n");
        Other ->
            io:format("  ✗ Expected stdio transport, got: ~p~n", [Other])
    end,
    
    % Test without metadata (should parse URL)
    NoMetadataConfig = #{
        url => <<"npx -y @modelcontextprotocol/server-postgres">>
    },
    
    NoMetadataState = {state, undefined, <<"npx -y @modelcontextprotocol/server-postgres">>, 
                       undefined, undefined, undefined, undefined, disconnected, 
                       #{}, NoMetadataConfig, 1, #{}, #{}, <<>>},
    
    case determine_connection_type(NoMetadataState) of
        {stdio, <<"npx">>, [<<"-y">>, <<"@modelcontextprotocol/server-postgres">>]} ->
            io:format("  ✓ Stdio transport detected from URL parsing~n");
        Other2 ->
            io:format("  ✗ Expected stdio transport from URL, got: ~p~n", [Other2])
    end,
    
    % Test network URL
    NetworkConfig = #{
        url => <<"https://mcp.zapier.com/">>
    },
    
    NetworkState = {state, undefined, <<"https://mcp.zapier.com/">>, 
                    undefined, undefined, undefined, undefined, disconnected, 
                    #{}, NetworkConfig, 1, #{}, #{}, <<>>},
    
    case determine_connection_type(NetworkState) of
        network ->
            io:format("  ✓ Network transport detected for HTTPS URL~n");
        Other3 ->
            io:format("  ✗ Expected network transport, got: ~p~n", [Other3])
    end,
    
    io:format("~nTest complete!~n"),
    halt(0).

%% Helper functions copied from mcp_client_v2

is_command_url(Url) when is_binary(Url) ->
    % Check if URL starts with common command patterns
    case binary:match(Url, [<<"npx ">>, <<"node ">>, <<"python ">>, <<"ruby ">>, <<"bash ">>]) of
        {0, _} ->
            % This is a command, not a URL
            Parts = binary:split(Url, <<" ">>, [global, trim_all]),
            case Parts of
                [Command | Args] -> {true, Command, Args};
                _ -> false
            end;
        _ ->
            false
    end;
is_command_url(_) ->
    false.

determine_connection_type({state, _ServerId, Url, _WebsocketPid, _StreamRef, 
                          _ConnectionPid, _ConnectionType, _Status, _Capabilities, 
                          Config, _RequestCounter, _PendingRequests, _Subscriptions, 
                          _SseBuffer}) ->
    Metadata = maps:get(metadata, Config, #{}),
    
    % First check if transport is explicitly specified in metadata
    case maps:get(<<"transport">>, Metadata, undefined) of
        <<"stdio">> ->
            Command = maps:get(<<"command">>, Metadata, <<"npx">>),
            Args = maps:get(<<"args">>, Metadata, []),
            {stdio, Command, Args};
        _ ->
            % If no explicit transport, check if URL looks like a command
            case is_command_url(Url) of
                {true, ParsedCommand, ParsedArgs} ->
                    {stdio, ParsedCommand, ParsedArgs};
                false ->
                    network
            end
    end.