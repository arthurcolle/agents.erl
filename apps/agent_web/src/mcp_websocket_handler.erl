-module(mcp_websocket_handler).
-behaviour(cowboy_websocket).

%% WebSocket handler for MCP server connections

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

-record(state, {
    server_id,
    client_id,
    server_pid
}).

%%====================================================================
%% cowboy_websocket callbacks
%%====================================================================

init(Req, [ServerId]) ->
    % Extract client information from request
    ClientId = generate_client_id(),
    
    % Check for MCP protocol in subprotocols
    case cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req) of
        [<<"mcp">> | _] ->
            Req2 = cowboy_req:set_resp_header(<<"sec-websocket-protocol">>, <<"mcp">>, Req),
            {cowboy_websocket, Req2, #state{
                server_id = ServerId,
                client_id = ClientId
            }};
        _ ->
            % Accept without specific protocol
            {cowboy_websocket, Req, #state{
                server_id = ServerId,
                client_id = ClientId
            }}
    end.

websocket_init(State) ->
    % Find the MCP server process
    case find_mcp_server(State#state.server_id) of
        {ok, ServerPid} ->
            % Register this connection with the server
            ServerPid ! {mcp_client_connected, State#state.client_id, self(), #{}},
            {ok, State#state{server_pid = ServerPid}};
        {error, not_found} ->
            error_logger:error_msg("MCP Server not found: ~p~n", [State#state.server_id]),
            {stop, State}
    end.

websocket_handle({text, Data}, State) ->
    try
        Message = jsx:decode(Data, [return_maps]),
        % Forward message to MCP server
        gen_server:cast(State#state.server_pid, {client_message, State#state.client_id, Message}),
        {ok, State}
    catch
        _:_ ->
            error_logger:warning_msg("Invalid JSON received from MCP client: ~p~n", [Data]),
            ErrorMessage = jsx:encode(#{
                <<"jsonrpc">> => <<"2.0">>,
                <<"error">> => #{
                    <<"code">> => -32700,
                    <<"message">> => <<"Parse error">>
                }
            }),
            {reply, {text, ErrorMessage}, State}
    end;

websocket_handle({binary, _Data}, State) ->
    % MCP protocol uses text frames only
    {ok, State};

websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({send_message, Message}, State) ->
    try
        Data = jsx:encode(Message),
        {reply, {text, Data}, State}
    catch
        _:_ ->
            error_logger:error_msg("Failed to encode MCP message: ~p~n", [Message]),
            {ok, State}
    end;

websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _PartialReq, #state{server_pid = ServerPid, client_id = ClientId}) when is_pid(ServerPid) ->
    % Notify server of client disconnection
    ServerPid ! {mcp_client_disconnected, ClientId},
    ok;

terminate(_Reason, _PartialReq, _State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

generate_client_id() ->
    iolist_to_binary(io_lib:format("client_~p", [erlang:system_time(microsecond)])).

find_mcp_server(ServerId) ->
    % This would typically look up the server in a registry
    % For now, we'll use a simple approach
    case whereis(list_to_atom("mcp_server_" ++ binary_to_list(ServerId))) of
        undefined ->
            {error, not_found};
        Pid when is_pid(Pid) ->
            {ok, Pid}
    end.