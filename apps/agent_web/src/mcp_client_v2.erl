-module(mcp_client_v2).
-behaviour(gen_server).

%% API
-export([start_link/2, stop/1, connect/1, disconnect/1, 
         list_tools/1, call_tool/3, list_resources/1, read_resource/2,
         list_prompts/1, get_prompt/2, ping/1, send_progress/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-define(PROTOCOL_VERSION, <<"2024-11-05">>).
-define(CLIENT_NAME, <<"agents.erl">>).
-define(CLIENT_VERSION, <<"1.0.0">>).

-record(state, {
    server_id,
    url,
    websocket_pid,
    stream_ref,
    connection_pid,
    connection_type, % websocket | sse | http_post | stdio
    status = disconnected, % disconnected | connecting | connected | error
    capabilities = #{},
    server_info = #{},
    request_counter = 1,
    pending_requests = #{}, % RequestId -> {From, RequestData}
    subscriptions = #{}, % URI -> true
    sse_buffer = <<>> % Buffer for incomplete SSE messages
}).

-record(mcp_request, {
    id,
    method,
    params,
    from,
    timestamp
}).

%%====================================================================
%% API functions
%%====================================================================

start_link(ServerId, Config) ->
    gen_server:start_link(?MODULE, {ServerId, Config}, []).

stop(Pid) ->
    gen_server:stop(Pid).

connect(Pid) ->
    gen_server:call(Pid, connect, 30000).

disconnect(Pid) ->
    gen_server:call(Pid, disconnect).

list_tools(Pid) ->
    gen_server:call(Pid, {request, <<"tools/list">>, #{}}, 30000).

call_tool(Pid, ToolName, Arguments) ->
    Params = #{
        <<"name">> => ToolName,
        <<"arguments">> => Arguments
    },
    gen_server:call(Pid, {request, <<"tools/call">>, Params}, 60000).

list_resources(Pid) ->
    gen_server:call(Pid, {request, <<"resources/list">>, #{}}, 30000).

read_resource(Pid, URI) ->
    Params = #{<<"uri">> => URI},
    gen_server:call(Pid, {request, <<"resources/read">>, Params}, 30000).

list_prompts(Pid) ->
    gen_server:call(Pid, {request, <<"prompts/list">>, #{}}, 30000).

get_prompt(Pid, Name) ->
    Params = #{<<"name">> => Name},
    gen_server:call(Pid, {request, <<"prompts/get">>, Params}, 30000).

ping(Pid) ->
    gen_server:call(Pid, {request, <<"ping">>, #{}}, 10000).

send_progress(Pid, ProgressToken, Progress) ->
    Params = #{
        <<"progressToken">> => ProgressToken,
        <<"progress">> => Progress
    },
    gen_server:cast(Pid, {notify, <<"notifications/progress">>, Params}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init({ServerId, Config}) ->
    process_flag(trap_exit, true),
    Url = maps:get(url, Config),
    State = #state{
        server_id = ServerId,
        url = Url,
        server_info = Config
    },
    {ok, State}.

handle_call(connect, From, #state{status = disconnected} = State) ->
    case determine_connection_type(State) of
        {stdio, Command, Args} ->
            error_logger:info_msg("[MCP_CLIENT] Attempting stdio connection: ~s ~p~n", [Command, Args]),
            start_stdio_connection(Command, Args, From, State);
        network ->
            error_logger:info_msg("[MCP_CLIENT] Attempting network connection to: ~s~n", [State#state.url]),
            case start_websocket_connection(State#state.url) of
                {ok, ConnPid, StreamRef} ->
                    error_logger:info_msg("[MCP_CLIENT] WebSocket connection established to ~s~n", [State#state.url]),
            NewState = State#state{
                connection_pid = ConnPid,
                stream_ref = StreamRef,
                connection_type = websocket,
                status = connecting
            },
            % Don't reply yet - will reply after initialization completes
            {noreply, NewState#state{pending_requests = #{init => From}}};
        {ok, ConnPid, StreamRef, sse} ->
            error_logger:info_msg("[MCP_CLIENT] SSE connection established to ~s~n", [State#state.url]),
            NewState = State#state{
                connection_pid = ConnPid,
                stream_ref = StreamRef,
                connection_type = sse,
                status = connecting
            },
            % Send initialization request for SSE
            InitRequest = #{
                <<"jsonrpc">> => <<"2.0">>,
                <<"id">> => <<"init">>,
                <<"method">> => <<"initialize">>,
                <<"params">> => #{
                    <<"protocolVersion">> => ?PROTOCOL_VERSION,
                    <<"capabilities">> => #{
                        <<"roots">> => #{
                            <<"listChanged">> => true
                        },
                        <<"sampling">> => #{},
                        <<"elicitation">> => #{}
                    },
                    <<"clientInfo">> => #{
                        <<"name">> => ?CLIENT_NAME,
                        <<"version">> => ?CLIENT_VERSION
                    }
                }
            },
            case send_message(InitRequest, NewState) of
                ok ->
                    {noreply, NewState#state{pending_requests = #{init => From}}};
                {error, Reason} ->
                    error_logger:error_msg("[MCP_CLIENT] Failed to send init message: ~p~n", [Reason]),
                    cleanup_connection(NewState),
                    {reply, {error, {init_send_failed, Reason}}, State#state{status = error}}
            end;
        {ok, ConnPid, _, http_post} ->
            error_logger:info_msg("[MCP_CLIENT] HTTP POST connection established to ~s~n", [State#state.url]),
            NewState = State#state{
                connection_pid = ConnPid,
                stream_ref = undefined,
                connection_type = http_post,
                status = connecting
            },
            % Send initialization request for HTTP POST
            InitRequest = #{
                <<"jsonrpc">> => <<"2.0">>,
                <<"id">> => <<"init">>,
                <<"method">> => <<"initialize">>,
                <<"params">> => #{
                    <<"protocolVersion">> => ?PROTOCOL_VERSION,
                    <<"capabilities">> => #{
                        <<"roots">> => #{
                            <<"listChanged">> => true
                        },
                        <<"sampling">> => #{},
                        <<"elicitation">> => #{}
                    },
                    <<"clientInfo">> => #{
                        <<"name">> => ?CLIENT_NAME,
                        <<"version">> => ?CLIENT_VERSION
                    }
                }
            },
            case send_message(InitRequest, NewState) of
                ok ->
                    {noreply, NewState#state{pending_requests = #{init => From}}};
                {error, Reason} ->
                    error_logger:error_msg("[MCP_CLIENT] Failed to send init message: ~p~n", [Reason]),
                    cleanup_connection(NewState),
                    {reply, {error, {init_send_failed, Reason}}, State#state{status = error}}
            end;
                {error, Reason} ->
                    error_logger:error_msg("[MCP_CLIENT] Failed to establish connection: ~p~n", [Reason]),
                    {reply, {error, {connection_failed, Reason}}, State#state{status = error}}
            end
    end;

handle_call(connect, _From, #state{status = Status} = State) ->
    {reply, {error, {already_in_state, Status}}, State};

handle_call(disconnect, _From, State) ->
    NewState = cleanup_connection(State),
    {reply, ok, NewState#state{status = disconnected}};

handle_call({request, Method, Params}, From, #state{status = connected} = State) ->
    case send_request(Method, Params, State) of
        {ok, RequestId, NewState} ->
            UpdatedState = NewState#state{
                pending_requests = maps:put(RequestId, From, NewState#state.pending_requests)
            },
            {noreply, UpdatedState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({request, _Method, _Params}, _From, #state{status = Status} = State) ->
    {reply, {error, {not_connected, Status}}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({notify, Method, Params}, #state{status = connected} = State) ->
    send_notification(Method, Params, State),
    {noreply, State};

handle_cast({notify, _Method, _Params}, State) ->
    % Ignore notifications when not connected
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({gun_ws, ConnPid, StreamRef, {text, Data}}, 
            #state{connection_pid = ConnPid, stream_ref = StreamRef, connection_type = websocket} = State) ->
    handle_websocket_message(Data, State);

handle_info({gun_data, ConnPid, StreamRef, nofin, Data}, 
            #state{connection_pid = ConnPid, stream_ref = StreamRef, connection_type = sse} = State) ->
    handle_sse_data(Data, State);

handle_info({gun_data, ConnPid, StreamRef, fin, Data}, 
            #state{connection_pid = ConnPid, stream_ref = StreamRef, connection_type = sse} = State) ->
    handle_sse_data(Data, State);

handle_info({gun_ws, ConnPid, StreamRef, {close, Code, Reason}}, 
            #state{connection_pid = ConnPid, stream_ref = StreamRef} = State) ->
    error_logger:warning_msg("MCP WebSocket closed: ~p ~p~n", [Code, Reason]),
    NewState = cleanup_connection(State),
    reply_to_pending_requests({error, connection_closed}, NewState),
    {noreply, NewState#state{status = error}};

handle_info({gun_error, ConnPid, StreamRef, Reason}, 
            #state{connection_pid = ConnPid, stream_ref = StreamRef} = State) ->
    error_logger:error_msg("MCP WebSocket error: ~p~n", [Reason]),
    NewState = cleanup_connection(State),
    reply_to_pending_requests({error, {websocket_error, Reason}}, NewState),
    {noreply, NewState#state{status = error}};

handle_info({gun_error, ConnPid, Reason}, 
            #state{connection_pid = ConnPid} = State) ->
    error_logger:error_msg("MCP Connection error: ~p~n", [Reason]),
    NewState = cleanup_connection(State),
    reply_to_pending_requests({error, {connection_error, Reason}}, NewState),
    {noreply, NewState#state{status = error}};

handle_info({'EXIT', ConnPid, Reason}, 
            #state{connection_pid = ConnPid} = State) ->
    error_logger:info_msg("MCP Connection process exited: ~p~n", [Reason]),
    NewState = cleanup_connection(State),
    reply_to_pending_requests({error, connection_died}, NewState),
    {noreply, NewState#state{status = error}};

handle_info({transport_message, Message}, #state{connection_type = stdio} = State) ->
    % Handle messages from stdio transport
    process_mcp_message(Message, State);

handle_info({transport_closed, Reason}, #state{connection_type = stdio} = State) ->
    error_logger:warning_msg("MCP stdio transport closed: ~p~n", [Reason]),
    NewState = cleanup_connection(State),
    reply_to_pending_requests({error, transport_closed}, NewState),
    {noreply, NewState#state{status = error}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    cleanup_connection(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

start_websocket_connection(Url) ->
    case uri_string:parse(Url) of
        #{scheme := Scheme, host := Host} = ParsedUrl 
        when Scheme =:= <<"ws">> orelse Scheme =:= <<"wss">> ->
            Port = maps:get(port, ParsedUrl, case Scheme of
                <<"wss">> -> 443;
                <<"ws">> -> 80
            end),
            Path = maps:get(path, ParsedUrl, <<"/">>),
            
            error_logger:info_msg("[MCP_CLIENT] Connecting to WebSocket: ~s://~s:~p~s~n", [Scheme, Host, Port, Path]),
            
            Opts = case Scheme of
                <<"wss">> -> #{protocols => [http], transport => tls};
                <<"ws">> -> #{protocols => [http]}
            end,
            
            case gun:open(binary_to_list(Host), Port, Opts) of
                {ok, ConnPid} ->
                    error_logger:info_msg("[MCP_CLIENT] Gun connection opened, awaiting up~n"),
                    case gun:await_up(ConnPid, 5000) of
                        {ok, _Protocol} ->
                            error_logger:info_msg("[MCP_CLIENT] Gun connection up, upgrading to WebSocket~n"),
                            Headers = [
                                {<<"sec-websocket-protocol">>, <<"mcp">>}
                            ],
                            StreamRef = gun:ws_upgrade(ConnPid, binary_to_list(Path), Headers),
                            case gun:await(ConnPid, StreamRef, 5000) of
                                {upgrade, [<<"websocket">>], _Headers} ->
                                    error_logger:info_msg("[MCP_CLIENT] WebSocket upgrade successful~n"),
                                    % Start initialization sequence after WebSocket is established
                                    InitRequest = #{
                                        <<"jsonrpc">> => <<"2.0">>,
                                        <<"id">> => <<"init">>,
                                        <<"method">> => <<"initialize">>,
                                        <<"params">> => #{
                                            <<"protocolVersion">> => ?PROTOCOL_VERSION,
                                            <<"capabilities">> => #{
                                                <<"roots">> => #{
                                                    <<"listChanged">> => true
                                                },
                                                <<"sampling">> => #{},
                                                <<"elicitation">> => #{}
                                            },
                                            <<"clientInfo">> => #{
                                                <<"name">> => ?CLIENT_NAME,
                                                <<"version">> => ?CLIENT_VERSION
                                            }
                                        }
                                    },
                                    Data = jsx:encode(InitRequest),
                                    gun:ws_send(ConnPid, StreamRef, {text, Data}),
                                    {ok, ConnPid, StreamRef};
                                {response, _Fin, Status, _Headers} ->
                                    error_logger:error_msg("[MCP_CLIENT] WebSocket upgrade failed with status: ~p~n", [Status]),
                                    gun:close(ConnPid),
                                    {error, {ws_upgrade_failed, Status}};
                                {error, Reason} ->
                                    error_logger:error_msg("[MCP_CLIENT] WebSocket upgrade error: ~p~n", [Reason]),
                                    gun:close(ConnPid),
                                    {error, {ws_upgrade_error, Reason}}
                            end;
                        {error, Reason} ->
                            error_logger:error_msg("[MCP_CLIENT] Gun await_up error: ~p~n", [Reason]),
                            gun:close(ConnPid),
                            {error, {gun_await_up_error, Reason}}
                    end;
                {error, Reason} ->
                    error_logger:error_msg("[MCP_CLIENT] Gun open error: ~p~n", [Reason]),
                    {error, {gun_open_error, Reason}}
            end;
        #{scheme := Scheme, host := Host} = ParsedUrl 
        when Scheme =:= <<"http">> orelse Scheme =:= <<"https">> ->
            % For HTTP/HTTPS URLs, try multiple connection strategies
            Port = maps:get(port, ParsedUrl, case Scheme of
                <<"https">> -> 443;
                <<"http">> -> 80
            end),
            Path = maps:get(path, ParsedUrl, <<"/">>),
            error_logger:info_msg("[MCP_CLIENT] Trying HTTP connection to: ~s://~s:~p~s~n", [Scheme, Host, Port, Path]),
            start_http_connection(Scheme, Host, Port, Path);
        #{scheme := Scheme} ->
            error_logger:error_msg("[MCP_CLIENT] Unsupported URL scheme: ~p~n", [Scheme]),
            {error, {unsupported_scheme, Scheme}};
        _ ->
            error_logger:error_msg("[MCP_CLIENT] Invalid URL format: ~s~n", [Url]),
            {error, invalid_url}
    end.

start_sse_connection(Scheme, Host, Port, Path) ->
    Opts = case Scheme of
        <<"https">> -> #{protocols => [http], transport => tls};
        <<"http">> -> #{protocols => [http]}
    end,
    
    case gun:open(binary_to_list(Host), Port, Opts) of
        {ok, ConnPid} ->
            case gun:await_up(ConnPid, 5000) of
                {ok, _Protocol} ->
                    Headers = [
                        {<<"accept">>, <<"text/event-stream">>},
                        {<<"cache-control">>, <<"no-cache">>},
                        {<<"connection">>, <<"keep-alive">>}
                    ],
                    StreamRef = gun:get(ConnPid, binary_to_list(Path), Headers),
                    case gun:await(ConnPid, StreamRef, 5000) of
                        {response, nofin, 200, _ResponseHeaders} ->
                            % SSE connection established
                            {ok, ConnPid, StreamRef, sse};
                        {response, _Fin, Status, _Headers} ->
                            gun:close(ConnPid),
                            {error, {http_error, Status}};
                        {error, Reason} ->
                            gun:close(ConnPid),
                            {error, Reason}
                    end;
                {error, Reason} ->
                    gun:close(ConnPid),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

start_http_connection(Scheme, Host, Port, Path) ->
    % Try multiple HTTP connection strategies for MCP servers
    % 1. Try direct JSON-RPC HTTP POST first (most common)
    % 2. Fall back to SSE if direct POST fails
    Opts = case Scheme of
        <<"https">> -> #{protocols => [http], transport => tls};
        <<"http">> -> #{protocols => [http]}
    end,
    
    error_logger:info_msg("[MCP_CLIENT] Opening HTTP connection to ~s:~p~n", [Host, Port]),
    case gun:open(binary_to_list(Host), Port, Opts) of
        {ok, ConnPid} ->
            error_logger:info_msg("[MCP_CLIENT] HTTP connection opened, awaiting up~n"),
            case gun:await_up(ConnPid, 5000) of
                {ok, _Protocol} ->
                    error_logger:info_msg("[MCP_CLIENT] HTTP connection up, trying POST endpoint~n"),
                    % First try to establish direct HTTP POST connection
                    case try_http_post_connection(ConnPid, Path) of
                        {ok, ConnPid, post} ->
                            error_logger:info_msg("[MCP_CLIENT] HTTP POST connection established~n"),
                            {ok, ConnPid, undefined, http_post};
                        {error, PostReason} ->
                            error_logger:warning_msg("[MCP_CLIENT] HTTP POST failed: ~p, trying SSE~n", [PostReason]),
                            % Fall back to SSE connection
                            try_sse_connection(ConnPid, Path)
                    end;
                {error, Reason} ->
                    error_logger:error_msg("[MCP_CLIENT] HTTP await_up error: ~p~n", [Reason]),
                    gun:close(ConnPid),
                    {error, {http_await_up_error, Reason}}
            end;
        {error, Reason} ->
            error_logger:error_msg("[MCP_CLIENT] HTTP gun:open error: ~p~n", [Reason]),
            {error, {http_gun_open_error, Reason}}
    end.

try_http_post_connection(ConnPid, Path) ->
    % Test if the server accepts JSON-RPC POST requests
    TestRequest = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => <<"test">>,
        <<"method">> => <<"initialize">>,
        <<"params">> => #{
            <<"protocolVersion">> => ?PROTOCOL_VERSION,
            <<"capabilities">> => #{},
            <<"clientInfo">> => #{
                <<"name">> => ?CLIENT_NAME,
                <<"version">> => ?CLIENT_VERSION
            }
        }
    },
    
    try
        Data = jsx:encode(TestRequest),
        Headers = [
            {<<"content-type">>, <<"application/json">>},
            {<<"accept">>, <<"application/json">>}
        ],
        StreamRef = gun:post(ConnPid, binary_to_list(Path), Headers, Data),
        case gun:await(ConnPid, StreamRef, 5000) of
            {response, nofin, 200, _ResponseHeaders} ->
                % Server accepts POST requests
                {ok, ConnPid, post};
            {response, fin, 200, _ResponseHeaders} ->
                % Server accepts POST requests
                {ok, ConnPid, post};
            {response, _Fin, Status, _Headers} ->
                {error, {http_error, Status}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        _:CatchReason ->
            {error, {encode_error, CatchReason}}
    end.

try_sse_connection(ConnPid, Path) ->
    % Try SSE connection as fallback
    Headers = [
        {<<"accept">>, <<"text/event-stream">>},
        {<<"cache-control">>, <<"no-cache">>},
        {<<"connection">>, <<"keep-alive">>}
    ],
    StreamRef = gun:get(ConnPid, binary_to_list(Path), Headers),
    case gun:await(ConnPid, StreamRef, 5000) of
        {response, nofin, 200, _ResponseHeaders} ->
            % SSE connection established
            {ok, ConnPid, StreamRef, sse};
        {response, _Fin, Status, _Headers} ->
            gun:close(ConnPid),
            {error, {http_error, Status}};
        {error, Reason} ->
            gun:close(ConnPid),
            {error, Reason}
    end.

handle_websocket_message(Data, State) ->
    try jsx:decode(Data, [return_maps]) of
        Message ->
            process_mcp_message(Message, State)
    catch
        _:_ ->
            error_logger:warning_msg("Invalid JSON received: ~p~n", [Data]),
            {noreply, State}
    end.

handle_sse_data(Data, #state{sse_buffer = Buffer} = State) ->
    % Append new data to buffer
    NewBuffer = <<Buffer/binary, Data/binary>>,
    % Process complete SSE events
    process_sse_events(NewBuffer, State).

process_sse_events(Buffer, State) ->
    case binary:split(Buffer, <<"\n\n">>) of
        [Event, Rest] ->
            % Process the complete event
            NewState = case parse_sse_event(Event) of
                {ok, JsonData} ->
                    try jsx:decode(JsonData, [return_maps]) of
                        Message ->
                            case process_mcp_message(Message, State) of
                                {noreply, ProcessedState} -> ProcessedState;
                                {reply, _Reply, ProcessedState} -> ProcessedState
                            end
                    catch
                        _:_ ->
                            error_logger:warning_msg("Invalid JSON in SSE event: ~p~n", [JsonData]),
                            State
                    end;
                ignore ->
                    State;
                {error, Reason} ->
                    error_logger:warning_msg("Failed to parse SSE event: ~p~n", [Reason]),
                    State
            end,
            % Continue processing remaining buffer
            process_sse_events(Rest, NewState#state{sse_buffer = <<>>});
        [_IncompleteEvent] ->
            % Store incomplete event in buffer
            {noreply, State#state{sse_buffer = Buffer}}
    end.

parse_sse_event(EventData) ->
    Lines = binary:split(EventData, <<"\n">>, [global]),
    parse_sse_lines(Lines, #{}).

parse_sse_lines([], Acc) ->
    case maps:get(data, Acc, undefined) of
        undefined -> ignore;
        Data -> {ok, Data}
    end;
parse_sse_lines([Line | Rest], Acc) ->
    case binary:split(Line, <<": ">>) of
        [<<"data">>, Data] ->
            % Accumulate data lines
            ExistingData = maps:get(data, Acc, <<>>),
            NewData = case ExistingData of
                <<>> -> Data;
                _ -> <<ExistingData/binary, "\n", Data/binary>>
            end,
            parse_sse_lines(Rest, Acc#{data => NewData});
        [<<"event">>, EventType] ->
            parse_sse_lines(Rest, Acc#{event => EventType});
        [<<"id">>, Id] ->
            parse_sse_lines(Rest, Acc#{id => Id});
        [<<"retry">>, RetryTime] ->
            parse_sse_lines(Rest, Acc#{retry => RetryTime});
        [<<>>] ->
            % Empty line, ignore
            parse_sse_lines(Rest, Acc);
        _ ->
            % Comment or malformed line, ignore
            parse_sse_lines(Rest, Acc)
    end.

process_mcp_message(#{<<"jsonrpc">> := <<"2.0">>, <<"id">> := Id} = Message, State) ->
    case maps:find(<<"result">>, Message) of
        {ok, Result} ->
            handle_response(Id, {ok, Result}, State);
        error ->
            case maps:find(<<"error">>, Message) of
                {ok, Error} ->
                    handle_response(Id, {error, Error}, State);
                error ->
                    error_logger:warning_msg("Invalid response message: ~p~n", [Message]),
                    {noreply, State}
            end
    end;

process_mcp_message(#{<<"jsonrpc">> := <<"2.0">>, <<"method">> := Method} = Message, State) ->
    % Handle notifications and requests from server
    Params = maps:get(<<"params">>, Message, #{}),
    handle_server_message(Method, Params, State);

process_mcp_message(Message, State) ->
    error_logger:warning_msg("Invalid MCP message: ~p~n", [Message]),
    {noreply, State}.

handle_response(init, {ok, Result}, #state{pending_requests = Pending} = State) ->
    % Handle initialization response
    case maps:take(init, Pending) of
        {From, UpdatedPending} ->
            ServerInfo = maps:get(<<"serverInfo">>, Result, #{}),
            Capabilities = maps:get(<<"capabilities">>, Result, #{}),
            
            error_logger:info_msg("[MCP_CLIENT] Initialization successful for server ~s~n", [State#state.server_id]),
            
            % Send initialized notification
            InitializedNotification = #{
                <<"jsonrpc">> => <<"2.0">>,
                <<"method">> => <<"notifications/initialized">>
            },
            send_message(InitializedNotification, State),
            
            NewState = State#state{
                status = connected,
                server_info = ServerInfo,
                capabilities = Capabilities,
                pending_requests = UpdatedPending
            },
            
            % Update registry with connected status
            gen_server:cast(mcp_registry, {update_server_status, State#state.server_id, connected}),
            
            gen_server:reply(From, {ok, #{
                server_info => ServerInfo,
                capabilities => Capabilities
            }}),
            {noreply, NewState};
        error ->
            {noreply, State}
    end;

handle_response(RequestId, Response, #state{pending_requests = Pending} = State) ->
    case maps:take(RequestId, Pending) of
        {From, UpdatedPending} ->
            % Log error responses
            case Response of
                {error, Error} ->
                    error_logger:error_msg("[MCP_CLIENT] Request ~s failed: ~p~n", [RequestId, Error]);
                _ ->
                    ok
            end,
            gen_server:reply(From, Response),
            {noreply, State#state{pending_requests = UpdatedPending}};
        error ->
            error_logger:warning_msg("Received response for unknown request: ~p~n", [RequestId]),
            {noreply, State}
    end.

handle_server_message(<<"notifications/message">>, Params, State) ->
    % Handle logging messages
    Level = maps:get(<<"level">>, Params, <<"info">>),
    Logger = maps:get(<<"logger">>, Params, <<"">>),
    Data = maps:get(<<"data">>, Params, #{}),
    
    error_logger:info_msg("MCP Server Log [~s] ~s: ~p~n", [Level, Logger, Data]),
    {noreply, State};

handle_server_message(<<"notifications/resources/updated">>, Params, State) ->
    % Handle resource update notifications
    URI = maps:get(<<"uri">>, Params),
    error_logger:info_msg("MCP Resource updated: ~s~n", [URI]),
    {noreply, State};

handle_server_message(<<"notifications/resources/list_changed">>, _Params, State) ->
    % Handle resource list changes
    error_logger:info_msg("MCP Resource list changed~n"),
    {noreply, State};

handle_server_message(<<"notifications/tools/list_changed">>, _Params, State) ->
    % Handle tool list changes
    error_logger:info_msg("MCP Tool list changed~n"),
    {noreply, State};

handle_server_message(<<"notifications/prompts/list_changed">>, _Params, State) ->
    % Handle prompt list changes
    error_logger:info_msg("MCP Prompt list changed~n"),
    {noreply, State};

handle_server_message(Method, Params, State) ->
    error_logger:warning_msg("Unhandled server message: ~s ~p~n", [Method, Params]),
    {noreply, State}.

send_request(Method, Params, #state{request_counter = Counter} = State) ->
    RequestId = integer_to_binary(Counter),
    Request = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => RequestId,
        <<"method">> => Method,
        <<"params">> => Params
    },
    
    case send_message(Request, State) of
        ok ->
            NewState = State#state{request_counter = Counter + 1},
            {ok, RequestId, NewState};
        {error, Reason} ->
            {error, Reason}
    end.

send_notification(Method, Params, State) ->
    Notification = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => Method,
        <<"params">> => Params
    },
    send_message(Notification, State).

send_message(Message, #state{connection_pid = ConnPid, stream_ref = StreamRef, connection_type = websocket}) ->
    try
        Data = jsx:encode(Message),
        gun:ws_send(ConnPid, StreamRef, {text, Data}),
        ok
    catch
        _:Reason ->
            {error, {encode_error, Reason}}
    end;

send_message(Message, #state{connection_pid = ConnPid, connection_type = http_post, url = Url} = State) ->
    % For HTTP POST connections, send directly to the URL
    case uri_string:parse(Url) of
        #{scheme := _Scheme, host := _Host, port := _Port, path := Path} ->
            try
                Data = jsx:encode(Message),
                Headers = [
                    {<<"content-type">>, <<"application/json">>},
                    {<<"accept">>, <<"application/json">>}
                ],
                StreamRef = gun:post(ConnPid, binary_to_list(Path), Headers, Data),
                % For initialization messages, wait for response immediately
                case maps:get(<<"method">>, Message, undefined) of
                    <<"initialize">> ->
                        case gun:await(ConnPid, StreamRef, 10000) of
                            {response, nofin, 200, ResponseHeaders} ->
                                case gun:await_body(ConnPid, StreamRef, 5000) of
                                    {ok, ResponseBody} ->
                                        self() ! {http_response, ResponseBody},
                                        ok;
                                    {error, Reason} ->
                                        {error, Reason}
                                end;
                            {response, fin, 200, ResponseHeaders} ->
                                ok;
                            {response, _Fin, Status, _Headers} ->
                                {error, {http_error, Status}};
                            {error, Reason} ->
                                {error, Reason}
                        end;
                    _ ->
                        % For other messages, don't wait for response
                        ok
                end
            catch
                _:CatchReason ->
                    {error, {encode_error, CatchReason}}
            end;
        _ ->
            {error, invalid_url}
    end;

send_message(Message, #state{connection_pid = ConnPid, connection_type = sse, url = Url}) ->
    % For SSE connections, try different approaches to send messages
    case uri_string:parse(Url) of
        #{scheme := _Scheme, host := _Host, port := _Port, path := Path} ->
            % Try multiple POST endpoint patterns that MCP servers might use
            PostPaths = [
                % Standard MCP JSON-RPC endpoint
                binary_to_list(iolist_to_binary([Path])),
                % Remove /sse suffix and try base path
                case binary:split(Path, <<"/sse">>) of
                    [BasePath, _] -> binary_to_list(BasePath);
                    _ -> binary_to_list(Path)
                end,
                % Try /mcp endpoint
                case binary:split(Path, <<"/sse">>) of
                    [BasePath, _] -> binary_to_list(<<BasePath/binary, "/mcp">>);
                    _ -> binary_to_list(<<Path/binary, "/mcp">>)
                end,
                % Try /jsonrpc endpoint
                case binary:split(Path, <<"/sse">>) of
                    [BasePath, _] -> binary_to_list(<<BasePath/binary, "/jsonrpc">>);
                    _ -> binary_to_list(<<Path/binary, "/jsonrpc">>)
                end
            ],
            try_post_endpoints(ConnPid, PostPaths, Message);
        _ ->
            {error, invalid_url}
    end;

send_message(Message, #state{connection_pid = ConnPid, connection_type = stdio}) ->
    mcp_transport_stdio:send(ConnPid, Message).

try_post_endpoints(_ConnPid, [], _Message) ->
    {error, no_valid_endpoint};
try_post_endpoints(ConnPid, [PostPath | RestPaths], Message) ->
    try
        Data = jsx:encode(Message),
        Headers = [
            {<<"content-type">>, <<"application/json">>},
            {<<"accept">>, <<"application/json">>}
        ],
        StreamRefPost = gun:post(ConnPid, PostPath, Headers, Data),
        case gun:await(ConnPid, StreamRefPost, 5000) of
            {response, nofin, 200, _ResponseHeaders} ->
                ok;
            {response, fin, 200, _ResponseHeaders} ->
                ok;
            {response, _Fin, Status, _Headers} when Status >= 400, Status < 500 ->
                % Client error, try next endpoint
                try_post_endpoints(ConnPid, RestPaths, Message);
            {response, _Fin, Status, _Headers} ->
                {error, {http_error, Status}};
            {error, PostReason} ->
                % Connection error, try next endpoint
                try_post_endpoints(ConnPid, RestPaths, Message)
        end
    catch
        _:CatchReason ->
            {error, {encode_error, CatchReason}}
    end.

cleanup_connection(#state{connection_pid = undefined} = State) ->
    State;
cleanup_connection(#state{connection_pid = ConnPid, connection_type = stdio} = State) ->
    try
        mcp_transport_stdio:disconnect(ConnPid)
    catch
        _:_ -> ok
    end,
    State#state{
        connection_pid = undefined,
        stream_ref = undefined,
        status = disconnected
    };
cleanup_connection(#state{connection_pid = ConnPid} = State) ->
    try
        gun:close(ConnPid)
    catch
        _:_ -> ok
    end,
    State#state{
        connection_pid = undefined,
        stream_ref = undefined,
        status = disconnected
    }.

reply_to_pending_requests(Response, #state{pending_requests = Pending}) ->
    maps:fold(fun
        (init, From, _Acc) ->
            gen_server:reply(From, Response);
        (_RequestId, From, _Acc) ->
            gen_server:reply(From, Response)
    end, ok, Pending).

%% Helper functions for stdio transport

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

start_stdio_connection(Command, Args, From, State) ->
    case mcp_transport_stdio:connect(Command, Args) of
        {ok, StdioPid} ->
            % Send initialization request for stdio
            InitRequest = #{
                <<"jsonrpc">> => <<"2.0">>,
                <<"id">> => <<"init">>,
                <<"method">> => <<"initialize">>,
                <<"params">> => #{
                    <<"protocolVersion">> => ?PROTOCOL_VERSION,
                    <<"capabilities">> => #{
                        <<"roots">> => #{
                            <<"listChanged">> => true
                        },
                        <<"sampling">> => #{},
                        <<"elicitation">> => #{}
                    },
                    <<"clientInfo">> => #{
                        <<"name">> => ?CLIENT_NAME,
                        <<"version">> => ?CLIENT_VERSION
                    }
                }
            },
            case mcp_transport_stdio:send(StdioPid, InitRequest) of
                ok ->
                    NewState = State#state{
                        connection_pid = StdioPid,
                        stream_ref = undefined,
                        connection_type = stdio,
                        status = connecting
                    },
                    {noreply, NewState#state{pending_requests = #{init => From}}};
                {error, Reason} ->
                    mcp_transport_stdio:disconnect(StdioPid),
                    {reply, {error, Reason}, State#state{status = error}}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State#state{status = error}}
    end.

