-module(mcp_client).
-export([test_connection/1, list_tools/1, call_tool/3, start_connection/1, stop_connection/1,
         list_resources/1, read_resource/2, list_prompts/1, get_prompt/2]).

-record(mcp_connection, {
    server_id,
    url,
    websocket_pid,
    request_id_counter = 1,
    pending_requests = #{}
}).

test_connection(ServerId) ->
    mcp_connection_manager:test_connection(ServerId).

list_tools(ServerId) ->
    case mcp_connection_manager:get_connection(ServerId) of
        {ok, {Pid, connected}} ->
            mcp_client_v2:list_tools(Pid);
        {ok, {_Pid, Status}} ->
            {error, {not_connected, Status}};
        {error, not_connected} ->
            % Try to auto-connect
            case mcp_connection_manager:connect_server(ServerId) of
                {ok, Pid} ->
                    mcp_client_v2:list_tools(Pid);
                {error, Reason} ->
                    {error, Reason}
            end
    end.

call_tool(ServerId, ToolName, Arguments) ->
    case get_or_connect(ServerId) of
        {ok, Pid} ->
            mcp_client_v2:call_tool(Pid, ToolName, Arguments);
        {error, Reason} ->
            {error, Reason}
    end.

list_resources(ServerId) ->
    case get_or_connect(ServerId) of
        {ok, Pid} ->
            mcp_client_v2:list_resources(Pid);
        {error, Reason} ->
            {error, Reason}
    end.

read_resource(ServerId, URI) ->
    case get_or_connect(ServerId) of
        {ok, Pid} ->
            mcp_client_v2:read_resource(Pid, URI);
        {error, Reason} ->
            {error, Reason}
    end.

list_prompts(ServerId) ->
    case get_or_connect(ServerId) of
        {ok, Pid} ->
            mcp_client_v2:list_prompts(Pid);
        {error, Reason} ->
            {error, Reason}
    end.

get_prompt(ServerId, Name) ->
    case get_or_connect(ServerId) of
        {ok, Pid} ->
            mcp_client_v2:get_prompt(Pid, Name);
        {error, Reason} ->
            {error, Reason}
    end.

get_or_connect(ServerId) ->
    case mcp_connection_manager:get_connection(ServerId) of
        {ok, {Pid, connected}} ->
            {ok, Pid};
        {ok, {_Pid, Status}} ->
            {error, {not_connected, Status}};
        {error, not_connected} ->
            % Try to auto-connect
            case mcp_connection_manager:connect_server(ServerId) of
                {ok, Pid} ->
                    {ok, Pid};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

start_connection(ServerId) ->
    mcp_connection_manager:connect_server(ServerId).

stop_connection(ServerId) ->
    mcp_connection_manager:disconnect_server(ServerId).