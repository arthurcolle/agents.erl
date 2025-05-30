-module(mcp_agent_integration).
-export([get_available_tools/0, get_available_tools/1, execute_mcp_tool/3, 
         format_tools_for_llm/0, format_tools_for_llm/1, handle_llm_tool_call/2]).

-record(tool_definition, {
    name,
    description,
    parameters,
    server_id,
    server_name
}).

%% Get all available tools from all connected MCP servers
get_available_tools() ->
    case mcp_registry:list_servers() of
        Servers when is_list(Servers) ->
            ConnectedServers = [S || S <- Servers, maps:get(status, S) =:= <<"connected">>],
            lists:foldl(fun(Server, Acc) ->
                ServerId = maps:get(id, Server),
                ServerName = maps:get(name, Server),
                case get_server_tools(ServerId, ServerName) of
                    {ok, Tools} -> Acc ++ Tools;
                    {error, _} -> Acc
                end
            end, [], ConnectedServers);
        _ ->
            []
    end.

%% Get available tools from a specific server
get_available_tools(ServerId) ->
    case mcp_registry:get_server(ServerId) of
        {ok, #{name := ServerName}} ->
            get_server_tools(ServerId, ServerName);
        {error, _} ->
            {error, server_not_found}
    end.

%% Execute a tool on a specific MCP server
execute_mcp_tool(ServerId, ToolName, Arguments) ->
    case mcp_client:call_tool(ServerId, ToolName, Arguments) of
        {ok, Result} ->
            {ok, Result};
        {error, Reason} ->
            {error, Reason}
    end.

%% Format all available tools for LLM consumption
format_tools_for_llm() ->
    Tools = get_available_tools(),
    format_tools_list(Tools).

%% Format tools from a specific server for LLM consumption
format_tools_for_llm(ServerId) ->
    case get_available_tools(ServerId) of
        {ok, Tools} ->
            {ok, format_tools_list(Tools)};
        {error, Reason} ->
            {error, Reason}
    end.

%% Internal functions

get_server_tools(ServerId, ServerName) ->
    case mcp_client:list_tools(ServerId) of
        {ok, #{<<"tools">> := ToolsList}} when is_list(ToolsList) ->
            Tools = lists:map(fun(Tool) ->
                #tool_definition{
                    name = maps:get(<<"name">>, Tool, <<"unknown">>),
                    description = maps:get(<<"description">>, Tool, <<"No description available">>),
                    parameters = maps:get(<<"inputSchema">>, Tool, #{}),
                    server_id = ServerId,
                    server_name = ServerName
                }
            end, ToolsList),
            {ok, Tools};
        {ok, ToolsMap} when is_map(ToolsMap) ->
            % Handle case where tools are returned as a map
            case maps:get(<<"tools">>, ToolsMap, []) of
                ToolsList when is_list(ToolsList) ->
                    Tools = lists:map(fun(Tool) ->
                        #tool_definition{
                            name = maps:get(<<"name">>, Tool, <<"unknown">>),
                            description = maps:get(<<"description">>, Tool, <<"No description available">>),
                            parameters = maps:get(<<"inputSchema">>, Tool, #{}),
                            server_id = ServerId,
                            server_name = ServerName
                        }
                    end, ToolsList),
                    {ok, Tools};
                _ ->
                    {ok, []}
            end;
        {ok, _} ->
            {ok, []};
        {error, Reason} ->
            {error, Reason}
    end.

format_tools_list(Tools) ->
    lists:map(fun(#tool_definition{
        name = Name,
        description = Description,
        parameters = Parameters,
        server_id = ServerId,
        server_name = ServerName
    }) ->
        #{
            type => <<"function">>,
            function => #{
                name => format_tool_name(Name, ServerId),
                description => format_tool_description(Description, ServerName),
                parameters => format_parameters(Parameters)
            },
            mcp_metadata => #{
                server_id => ServerId,
                server_name => ServerName,
                original_name => Name
            }
        }
    end, Tools).

format_tool_name(Name, ServerId) ->
    %% Prefix with server ID to avoid conflicts
    ServerIdStr = binary_to_list(ServerId),
    NameStr = binary_to_list(Name),
    list_to_binary("mcp_" ++ ServerIdStr ++ "_" ++ NameStr).

format_tool_description(Description, ServerName) ->
    %% Add server context to description
    DescStr = binary_to_list(Description),
    ServerStr = binary_to_list(ServerName),
    list_to_binary(DescStr ++ " (via MCP server: " ++ ServerStr ++ ")").

format_parameters(Parameters) when is_map(Parameters) ->
    %% Ensure parameters follow OpenAI function calling schema
    case maps:find(<<"type">>, Parameters) of
        {ok, <<"object">>} ->
            Parameters;
        _ ->
            %% Wrap in object if not already
            #{
                type => <<"object">>,
                properties => Parameters,
                required => []
            }
    end;
format_parameters(_) ->
    %% Default empty object schema
    #{
        type => <<"object">>,
        properties => #{},
        required => []
    }.

%% Function to handle tool calls from LLM responses
handle_llm_tool_call(ToolName, Arguments) ->
    case parse_mcp_tool_name(ToolName) of
        {ok, {ServerId, OriginalName}} ->
            execute_mcp_tool(ServerId, OriginalName, Arguments);
        {error, not_mcp_tool} ->
            %% Not an MCP tool, let other handlers deal with it
            {error, not_mcp_tool};
        {error, Reason} ->
            {error, Reason}
    end.

parse_mcp_tool_name(ToolName) ->
    case binary:split(ToolName, <<"_">>, [global]) of
        [<<"mcp">>, ServerId | NameParts] ->
            OriginalName = iolist_to_binary(lists:join(<<"_">>, NameParts)),
            {ok, {ServerId, OriginalName}};
        _ ->
            {error, not_mcp_tool}
    end.

%% Function to inject MCP tools into agent tool lists
enhance_agent_tools(ExistingTools) ->
    McpTools = format_tools_for_llm(),
    ExistingTools ++ McpTools.