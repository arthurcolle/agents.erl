# MCP Transport Fix Summary

## Problem
The MCP client was treating stdio transport commands (like `npx -y @modelcontextprotocol/server-filesystem`) as network URLs, causing connection failures with "Invalid URL format" errors.

## Root Cause
The system has two types of MCP servers:
1. **Network servers**: Use HTTP/HTTPS/WebSocket URLs (e.g., `https://mcp.zapier.com/`)
2. **Stdio servers**: Use command-line programs (e.g., `npx -y @modelcontextprotocol/server-filesystem`)

The MCP client was only checking for network URLs and didn't properly handle stdio transport configurations.

## Solution
Modified `mcp_client_v2.erl` to:

1. **Check metadata first**: Look for `transport: "stdio"` in the server metadata
2. **Parse URL as command**: If no metadata, check if the URL starts with common command patterns (`npx`, `node`, `python`, etc.)
3. **Use appropriate transport**: Route to stdio transport for commands, network transport for URLs

### Code Changes

#### In `determine_connection_type/1`:
```erlang
determine_connection_type(#state{server_info = Config, url = Url}) ->
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
```

#### New function `is_command_url/1`:
```erlang
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
```

### Also Fixed
- Fixed double spawn issue in `mcp_transport_stdio.erl` where port options included `{spawn, PortCommand}` twice

## Result
The system now correctly:
- Connects to network MCP servers using HTTP/WebSocket protocols
- Launches stdio MCP servers as child processes using the command specified
- Handles both explicit metadata and URL parsing for transport detection

## Verification
From the logs, we can see:
- Stdio servers like Graphlit (`npx -y graphlit-mcp-server`) are being processed correctly
- Network servers like Square (`https://mcp.squareup.com/sse`) are connecting via HTTPS
- The system no longer shows "Invalid URL format" errors for stdio commands