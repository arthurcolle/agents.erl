-module(mcp_cli).
-export([main/1, start_stdio_server/0]).

%% MCP CLI tool for starting stdio servers and managing MCP operations
%% Usage: 
%%   escript mcp_cli.erl server        - Start MCP server in stdio mode
%%   escript mcp_cli.erl client <url>  - Connect to remote MCP server
%%   escript mcp_cli.erl list          - List available servers
%%   escript mcp_cli.erl discover      - Discover MCP servers

main([]) ->
    usage();

main(["server"]) ->
    start_stdio_server();

main(["client", Url]) ->
    connect_client(Url);

main(["list"]) ->
    list_servers();

main(["discover"]) ->
    discover_servers();

main(["tools", ServerId]) ->
    list_tools(ServerId);

main(["resources", ServerId]) ->
    list_resources(ServerId);

main(["prompts", ServerId]) ->
    list_prompts(ServerId);

main(["call", ServerId, ToolName | Args]) ->
    call_tool(ServerId, ToolName, Args);

main(_) ->
    usage().

usage() ->
    io:format("MCP CLI Tool~n"),
    io:format("Usage:~n"),
    io:format("  mcp_cli server                    - Start MCP server in stdio mode~n"),
    io:format("  mcp_cli client <url>              - Connect to remote MCP server~n"),
    io:format("  mcp_cli list                      - List available servers~n"),
    io:format("  mcp_cli discover                  - Discover MCP servers~n"),
    io:format("  mcp_cli tools <server_id>         - List tools from server~n"),
    io:format("  mcp_cli resources <server_id>     - List resources from server~n"),
    io:format("  mcp_cli prompts <server_id>       - List prompts from server~n"),
    io:format("  mcp_cli call <server_id> <tool> [args...]  - Call tool~n"),
    halt(1).

start_stdio_server() ->
    %% Start the application if not already started
    application:ensure_all_started(agent_web),
    
    %% Create stdio MCP server
    Config = #{
        server_id => <<"stdio_server">>,
        transport => stdio
    },
    
    case mcp_server:start_link(Config) of
        {ok, Pid} ->
            %% Register the server
            register(mcp_stdio_server, Pid),
            
            %% Start stdio communication loop
            stdio_loop(Pid),
            
            ok;
        {error, Reason} ->
            io:format(standard_error, "Failed to start MCP server: ~p~n", [Reason]),
            halt(1)
    end.

stdio_loop(ServerPid) ->
    case io:get_line(standard_io, "") of
        eof ->
            ok;
        {error, Reason} ->
            io:format(standard_error, "Error reading from stdin: ~p~n", [Reason]);
        Line ->
            LineStr = string:trim(Line),
            case LineStr of
                "" ->
                    stdio_loop(ServerPid);
                _ ->
                    try
                        Message = jsx:decode(list_to_binary(LineStr), [return_maps]),
                        handle_stdio_message(ServerPid, Message),
                        stdio_loop(ServerPid)
                    catch
                        _:Error ->
                            ErrorResponse = #{
                                <<"jsonrpc">> => <<"2.0">>,
                                <<"error">> => #{
                                    <<"code">> => -32700,
                                    <<"message">> => <<"Parse error">>,
                                    <<"data">> => iolist_to_binary(io_lib:format("~p", [Error]))
                                }
                            },
                            io:format("~s~n", [jsx:encode(ErrorResponse)]),
                            stdio_loop(ServerPid)
                    end
            end
    end.

handle_stdio_message(ServerPid, Message) ->
    %% Forward message to server and get response
    ClientId = <<"stdio_client">>,
    gen_server:cast(ServerPid, {client_message, ClientId, Message}),
    
    %% For stdio, we need to wait for the response
    %% This is a simplified implementation
    receive
        {mcp_response, Response} ->
            io:format("~s~n", [jsx:encode(Response)])
    after 5000 ->
        Timeout = #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"error">> => #{
                <<"code">> => -32603,
                <<"message">> => <<"Request timeout">>
            }
        },
        io:format("~s~n", [jsx:encode(Timeout)])
    end.

connect_client(Url) ->
    application:ensure_all_started(agent_web),
    
    ServerId = <<"cli_client">>,
    Config = #{url => list_to_binary(Url)},
    
    case mcp_registry:register_server(ServerId, list_to_binary(Url), Config) of
        {ok, _} ->
            case mcp_connection_manager:connect_server(ServerId) of
                {ok, _} ->
                    io:format("Connected to MCP server: ~s~n", [Url]),
                    
                    %% Start interactive session
                    interactive_session(ServerId);
                {error, Reason} ->
                    io:format(standard_error, "Failed to connect: ~p~n", [Reason]),
                    halt(1)
            end;
        {error, Reason} ->
            io:format(standard_error, "Failed to register server: ~p~n", [Reason]),
            halt(1)
    end.

interactive_session(ServerId) ->
    io:format("MCP Interactive Session (type 'help' for commands, 'quit' to exit)~n"),
    interactive_loop(ServerId).

interactive_loop(ServerId) ->
    case io:get_line("mcp> ") of
        eof ->
            ok;
        {error, _} ->
            ok;
        Line ->
            Command = string:trim(Line),
            case Command of
                "quit" ->
                    mcp_connection_manager:disconnect_server(ServerId),
                    ok;
                "help" ->
                    print_interactive_help(),
                    interactive_loop(ServerId);
                "tools" ->
                    list_tools_interactive(ServerId),
                    interactive_loop(ServerId);
                "resources" ->
                    list_resources_interactive(ServerId),
                    interactive_loop(ServerId);
                "prompts" ->
                    list_prompts_interactive(ServerId),
                    interactive_loop(ServerId);
                _ ->
                    case string:split(Command, " ", all) of
                        ["call", ToolName | Args] ->
                            call_tool_interactive(ServerId, ToolName, Args),
                            interactive_loop(ServerId);
                        _ ->
                            io:format("Unknown command: ~s~n", [Command]),
                            interactive_loop(ServerId)
                    end
            end
    end.

print_interactive_help() ->
    io:format("Available commands:~n"),
    io:format("  tools                    - List available tools~n"),
    io:format("  resources                - List available resources~n"),
    io:format("  prompts                  - List available prompts~n"),
    io:format("  call <tool> [args...]    - Call a tool~n"),
    io:format("  help                     - Show this help~n"),
    io:format("  quit                     - Exit session~n").

list_servers() ->
    application:ensure_all_started(agent_web),
    
    io:format("Local MCP Servers:~n"),
    case mcp_manager:list_local_servers() of
        Servers when is_list(Servers) ->
            lists:foreach(fun(#{id := Id, status := Status, config := Config}) ->
                Port = maps:get(port, Config, "N/A"),
                io:format("  ~s (~p) - Port: ~p~n", [Id, Status, Port])
            end, Servers);
        _ ->
            io:format("  No local servers found~n")
    end,
    
    io:format("~nRemote MCP Servers:~n"),
    case mcp_manager:list_remote_servers() of
        RemoteServers when is_list(RemoteServers) ->
            lists:foreach(fun(Server) ->
                Id = maps:get(id, Server, "unknown"),
                Url = maps:get(url, Server, "unknown"),
                Status = maps:get(status, Server, "unknown"),
                io:format("  ~s (~s) - ~s~n", [Id, Status, Url])
            end, RemoteServers);
        _ ->
            io:format("  No remote servers found~n")
    end.

discover_servers() ->
    application:ensure_all_started(agent_web),
    
    io:format("Discovering MCP servers...~n"),
    case mcp_manager:discover_servers() of
        Discovered when is_list(Discovered) ->
            case Discovered of
                [] ->
                    io:format("No servers discovered~n");
                _ ->
                    io:format("Discovered servers:~n"),
                    lists:foreach(fun(Server) ->
                        Type = maps:get(type, Server, "unknown"),
                        Url = maps:get(url, Server, "unknown"),
                        io:format("  ~s: ~s~n", [Type, Url])
                    end, Discovered)
            end;
        _ ->
            io:format("Discovery failed~n")
    end.

list_tools(ServerId) ->
    application:ensure_all_started(agent_web),
    
    case mcp_client:list_tools(list_to_binary(ServerId)) of
        {ok, #{<<"tools">> := Tools}} ->
            io:format("Tools from server ~s:~n", [ServerId]),
            lists:foreach(fun(Tool) ->
                Name = maps:get(<<"name">>, Tool, "unknown"),
                Description = maps:get(<<"description">>, Tool, ""),
                io:format("  ~s - ~s~n", [Name, Description])
            end, Tools);
        {error, Reason} ->
            io:format("Failed to list tools: ~p~n", [Reason])
    end.

list_tools_interactive(ServerId) ->
    case mcp_client:list_tools(ServerId) of
        {ok, #{<<"tools">> := Tools}} ->
            io:format("Available tools:~n"),
            lists:foreach(fun(Tool) ->
                Name = maps:get(<<"name">>, Tool, "unknown"),
                Description = maps:get(<<"description">>, Tool, ""),
                io:format("  ~s - ~s~n", [Name, Description])
            end, Tools);
        {error, Reason} ->
            io:format("Failed to list tools: ~p~n", [Reason])
    end.

list_resources(ServerId) ->
    application:ensure_all_started(agent_web),
    
    case mcp_client:list_resources(list_to_binary(ServerId)) of
        {ok, #{<<"resources">> := Resources}} ->
            io:format("Resources from server ~s:~n", [ServerId]),
            lists:foreach(fun(Resource) ->
                URI = maps:get(<<"uri">>, Resource, "unknown"),
                Name = maps:get(<<"name">>, Resource, ""),
                io:format("  ~s - ~s~n", [URI, Name])
            end, Resources);
        {error, Reason} ->
            io:format("Failed to list resources: ~p~n", [Reason])
    end.

list_resources_interactive(ServerId) ->
    case mcp_client:list_resources(ServerId) of
        {ok, #{<<"resources">> := Resources}} ->
            io:format("Available resources:~n"),
            lists:foreach(fun(Resource) ->
                URI = maps:get(<<"uri">>, Resource, "unknown"),
                Name = maps:get(<<"name">>, Resource, ""),
                io:format("  ~s - ~s~n", [URI, Name])
            end, Resources);
        {error, Reason} ->
            io:format("Failed to list resources: ~p~n", [Reason])
    end.

list_prompts(ServerId) ->
    application:ensure_all_started(agent_web),
    
    case mcp_client:list_prompts(list_to_binary(ServerId)) of
        {ok, #{<<"prompts">> := Prompts}} ->
            io:format("Prompts from server ~s:~n", [ServerId]),
            lists:foreach(fun(Prompt) ->
                Name = maps:get(<<"name">>, Prompt, "unknown"),
                Description = maps:get(<<"description">>, Prompt, ""),
                io:format("  ~s - ~s~n", [Name, Description])
            end, Prompts);
        {error, Reason} ->
            io:format("Failed to list prompts: ~p~n", [Reason])
    end.

list_prompts_interactive(ServerId) ->
    case mcp_client:list_prompts(ServerId) of
        {ok, #{<<"prompts">> := Prompts}} ->
            io:format("Available prompts:~n"),
            lists:foreach(fun(Prompt) ->
                Name = maps:get(<<"name">>, Prompt, "unknown"),
                Description = maps:get(<<"description">>, Prompt, ""),
                io:format("  ~s - ~s~n", [Name, Description])
            end, Prompts);
        {error, Reason} ->
            io:format("Failed to list prompts: ~p~n", [Reason])
    end.

call_tool(ServerId, ToolName, Args) ->
    application:ensure_all_started(agent_web),
    
    %% Parse arguments as JSON if they look like JSON, otherwise as strings
    Arguments = case Args of
        [] -> #{};
        [JsonArg] ->
            try
                jsx:decode(list_to_binary(JsonArg), [return_maps])
            catch
                _:_ ->
                    #{<<"input">> => list_to_binary(JsonArg)}
            end;
        _ ->
            #{<<"args">> => [list_to_binary(Arg) || Arg <- Args]}
    end,
    
    case mcp_client:call_tool(list_to_binary(ServerId), list_to_binary(ToolName), Arguments) of
        {ok, Result} ->
            io:format("Tool result:~n~s~n", [jsx:encode(Result, [space, {indent, 2}])]);
        {error, Reason} ->
            io:format("Tool call failed: ~p~n", [Reason])
    end.

call_tool_interactive(ServerId, ToolName, Args) ->
    Arguments = case Args of
        [] -> #{};
        [JsonArg] ->
            try
                jsx:decode(list_to_binary(JsonArg), [return_maps])
            catch
                _:_ ->
                    #{<<"input">> => list_to_binary(JsonArg)}
            end;
        _ ->
            #{<<"args">> => [list_to_binary(Arg) || Arg <- Args]}
    end,
    
    case mcp_client:call_tool(ServerId, list_to_binary(ToolName), Arguments) of
        {ok, Result} ->
            io:format("Tool result:~n~s~n", [jsx:encode(Result, [space, {indent, 2}])]);
        {error, Reason} ->
            io:format("Tool call failed: ~p~n", [Reason])
    end.