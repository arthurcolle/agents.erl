-module(mcp_sse_handler).
-behaviour(cowboy_handler).

%% MCP Server-Sent Events (SSE) handler for Inspector compatibility
%% This provides HTTP SSE transport as alternative to WebSocket

-export([init/2, terminate/3]).

-record(state, {
    server_pid,
    server_id,
    bearer_token
}).

init(Req0, Opts) ->
    ServerId = maps:get(server_id, Opts, <<"agents_main">>),
    
    % Check for bearer token authentication
    BearerToken = case cowboy_req:header(<<"authorization">>, Req0) of
        <<"Bearer ", Token/binary>> -> Token;
        _ -> undefined
    end,
    
    % Validate authentication if required
    case validate_bearer_token(BearerToken) of
        {ok, authorized} ->
            % Find the MCP server
            case find_mcp_server(ServerId) of
                {ok, ServerPid} ->
                    % Set SSE headers
                    Req1 = cowboy_req:stream_reply(200, #{
                        <<"content-type">> => <<"text/event-stream">>,
                        <<"cache-control">> => <<"no-cache">>,
                        <<"connection">> => <<"keep-alive">>,
                        <<"access-control-allow-origin">> => <<"*">>,
                        <<"access-control-allow-headers">> => <<"Authorization, Content-Type">>,
                        <<"access-control-allow-methods">> => <<"GET, POST, OPTIONS">>
                    }, Req0),
                    
                    % Send initial connection event
                    send_sse_event(Req1, <<"connected">>, jsx:encode(#{
                        <<"type">> => <<"connection">>,
                        <<"status">> => <<"connected">>,
                        <<"server_id">> => ServerId
                    })),
                    
                    % Set up message monitoring
                    monitor(process, ServerPid),
                    
                    State = #state{
                        server_pid = ServerPid,
                        server_id = ServerId,
                        bearer_token = BearerToken
                    },
                    
                    % Process MCP requests
                    process_mcp_requests(Req1, State);
                {error, not_found} ->
                    ErrorReq = cowboy_req:reply(404, #{
                        <<"content-type">> => <<"application/json">>
                    }, jsx:encode(#{
                        <<"error">> => <<"MCP server not found">>,
                        <<"server_id">> => ServerId
                    }), Req0),
                    {ok, ErrorReq, Opts}
            end;
        {error, unauthorized} ->
            ErrorReq = cowboy_req:reply(401, #{
                <<"content-type">> => <<"application/json">>,
                <<"www-authenticate">> => <<"Bearer realm=\"MCP Server\"">>
            }, jsx:encode(#{
                <<"error">> => <<"Invalid or missing bearer token">>
            }), Req0),
            {ok, ErrorReq, Opts}
    end.

process_mcp_requests(Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> ->
            % Handle SSE connection - keep alive and stream events
            loop_sse(Req, State);
        <<"POST">> ->
            % Handle MCP JSON-RPC request
            handle_mcp_request(Req, State);
        <<"OPTIONS">> ->
            % Handle CORS preflight
            CorsReq = cowboy_req:reply(200, #{
                <<"access-control-allow-origin">> => <<"*">>,
                <<"access-control-allow-headers">> => <<"Authorization, Content-Type">>,
                <<"access-control-allow-methods">> => <<"GET, POST, OPTIONS">>,
                <<"access-control-max-age">> => <<"86400">>
            }, <<>>, Req),
            {ok, CorsReq, State}
    end.

loop_sse(Req, State) ->
    receive
        {mcp_event, EventType, Data} ->
            send_sse_event(Req, EventType, jsx:encode(Data)),
            loop_sse(Req, State);
        {mcp_response, Id, Response} ->
            send_sse_event(Req, <<"response">>, jsx:encode(#{
                <<"id">> => Id,
                <<"result">> => Response
            })),
            loop_sse(Req, State);
        {mcp_error, Id, Error} ->
            send_sse_event(Req, <<"error">>, jsx:encode(#{
                <<"id">> => Id,
                <<"error">> => Error
            })),
            loop_sse(Req, State);
        {'DOWN', _MonitorRef, process, Pid, Reason} when Pid =:= State#state.server_pid ->
            send_sse_event(Req, <<"disconnected">>, jsx:encode(#{
                <<"type">> => <<"disconnection">>,
                <<"reason">> => atom_to_binary(Reason, utf8)
            })),
            {ok, Req, State};
        timeout ->
            % Send keepalive
            send_sse_event(Req, <<"keepalive">>, jsx:encode(#{
                <<"timestamp">> => erlang:system_time(second)
            })),
            loop_sse(Req, State)
    after 30000 ->
        % Keepalive timeout
        loop_sse(Req, State)
    end.

handle_mcp_request(Req, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    
    try
        JsonRpcRequest = jsx:decode(Body, [return_maps]),
        
        % Validate JSON-RPC format
        case validate_jsonrpc_request(JsonRpcRequest) of
            {ok, Method, Params, Id} ->
                % Forward to MCP server
                case mcp_server:handle_request(State#state.server_pid, Method, Params) of
                    {ok, Result} ->
                        Response = jsx:encode(#{
                            <<"jsonrpc">> => <<"2.0">>,
                            <<"id">> => Id,
                            <<"result">> => Result
                        }),
                        ResponseReq = cowboy_req:reply(200, #{
                            <<"content-type">> => <<"application/json">>,
                            <<"access-control-allow-origin">> => <<"*">>
                        }, Response, Req1),
                        {ok, ResponseReq, State};
                    {error, ErrorCode, ErrorMessage} ->
                        ErrorResponse = jsx:encode(#{
                            <<"jsonrpc">> => <<"2.0">>,
                            <<"id">> => Id,
                            <<"error">> => #{
                                <<"code">> => ErrorCode,
                                <<"message">> => ErrorMessage
                            }
                        }),
                        ErrorReq = cowboy_req:reply(200, #{
                            <<"content-type">> => <<"application/json">>,
                            <<"access-control-allow-origin">> => <<"*">>
                        }, ErrorResponse, Req1),
                        {ok, ErrorReq, State}
                end;
            {error, invalid_request} ->
                ErrorResponse = jsx:encode(#{
                    <<"jsonrpc">> => <<"2.0">>,
                    <<"id">> => null,
                    <<"error">> => #{
                        <<"code">> => -32600,
                        <<"message">> => <<"Invalid Request">>
                    }
                }),
                ErrorReq = cowboy_req:reply(400, #{
                    <<"content-type">> => <<"application/json">>,
                    <<"access-control-allow-origin">> => <<"*">>
                }, ErrorResponse, Req1),
                {ok, ErrorReq, State}
        end
    catch
        _:_ ->
            ParseErrorResponse = jsx:encode(#{
                <<"jsonrpc">> => <<"2.0">>,
                <<"id">> => null,
                <<"error">> => #{
                    <<"code">> => -32700,
                    <<"message">> => <<"Parse error">>
                }
            }),
            ParseErrorReq = cowboy_req:reply(400, #{
                <<"content-type">> => <<"application/json">>,
                <<"access-control-allow-origin">> => <<"*">>
            }, ParseErrorResponse, Req1),
            {ok, ParseErrorReq, State}
    end.

terminate(_Reason, _Req, _State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

send_sse_event(Req, EventType, Data) ->
    EventData = [
        <<"event: ">>, EventType, <<"\n">>,
        <<"data: ">>, Data, <<"\n\n">>
    ],
    cowboy_req:stream_body(EventData, nofin, Req).

validate_bearer_token(undefined) ->
    % No token required for now - could be configured
    {ok, authorized};
validate_bearer_token(Token) ->
    % Validate token - for now accept any non-empty token
    case byte_size(Token) > 0 of
        true -> {ok, authorized};
        false -> {error, unauthorized}
    end.

find_mcp_server(ServerId) ->
    % Try to find the server via manager
    case mcp_manager:list_local_servers() of
        Servers when is_list(Servers) ->
            case lists:keyfind(ServerId, 2, [{maps:get(id, S), maps:get(pid, S)} || S <- Servers]) of
                {ServerId, Pid} -> {ok, Pid};
                false -> {error, not_found}
            end;
        _ ->
            {error, not_found}
    end.

validate_jsonrpc_request(#{<<"jsonrpc">> := <<"2.0">>, <<"method">> := Method} = Request) ->
    Id = maps:get(<<"id">>, Request, null),
    Params = maps:get(<<"params">>, Request, #{}),
    {ok, Method, Params, Id};
validate_jsonrpc_request(_) ->
    {error, invalid_request}.