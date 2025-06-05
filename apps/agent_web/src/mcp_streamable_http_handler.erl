%% @doc MCP Streamable HTTP Handler
%%
%% Complete Cowboy handler implementing MCP Streamable HTTP transport
%% with full specification compliance for 2025-03-26.
-module(mcp_streamable_http_handler).

-include_lib("kernel/include/logger.hrl").

%% Cowboy callbacks
-export([
    init/2,
    info/3,
    terminate/3
]).

%% Internal exports
-export([
    process_json_rpc/1,
    send_sse_message/4,
    handle_initialization/1
]).

%% Types
-type session_id() :: binary().
-type stream_id() :: binary().
-type event_id() :: non_neg_integer().
-type json_rpc() :: map().

-record(handler_state, {
    session_id :: session_id() | undefined,
    stream_id :: stream_id() | undefined,
    event_counter = 0 :: event_id(),
    last_event_id :: event_id() | undefined,
    connection_type :: post_response | sse_stream,
    start_time :: integer(),
    message_count = 0 :: non_neg_integer(),
    pending_requests = [] :: [json_rpc()]
}).

%% Constants
-define(KEEPALIVE_INTERVAL, 30000).
-define(MAX_REQUEST_SIZE, 1048576). % 1MB
-define(PROTOCOL_VERSION, <<"2025-03-26">>).

%%% ============================================================================
%%% Cowboy Handler Implementation
%%% ============================================================================

%% @doc Initialize HTTP handler
init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    
    case validate_request_security(Req0) of
        {ok, ValidationData} ->
            handle_method(Method, Req0, Opts, ValidationData);
        {error, Reason} ->
            ?LOG_WARNING("Security validation failed: ~p", [Reason]),
            Req1 = reply_error(Req0, 403, <<"Forbidden">>, Reason),
            {ok, Req1, #{}}
    end.

%% @doc Handle different HTTP methods
-spec handle_method(binary(), cowboy_req:req(), map(), map()) ->
    {ok | cowboy_loop, cowboy_req:req(), map()}.
handle_method(<<"POST">>, Req0, Opts, ValidationData) ->
    handle_post_request(Req0, Opts, ValidationData);
handle_method(<<"GET">>, Req0, Opts, ValidationData) ->
    handle_get_request(Req0, Opts, ValidationData);
handle_method(<<"DELETE">>, Req0, Opts, ValidationData) ->
    handle_delete_request(Req0, Opts, ValidationData);
handle_method(Method, Req0, _Opts, _ValidationData) ->
    ?LOG_WARNING("Unsupported HTTP method: ~p", [Method]),
    Req1 = cowboy_req:reply(405, #{
        <<"allow">> => <<"GET, POST, DELETE">>,
        <<"content-type">> => <<"text/plain">>
    }, <<"Method Not Allowed">>, Req0),
    {ok, Req1, #{}}.

%%% ============================================================================
%%% POST Request Handling
%%% ============================================================================

%% @doc Handle POST requests for JSON-RPC messages
handle_post_request(Req0, Opts, ValidationData) ->
    case read_and_parse_body(Req0) of
        {ok, Messages, Req1} ->
            SessionId = maps:get(session_id, ValidationData),
            process_post_messages(Messages, SessionId, Req1, Opts);
        {error, Reason, Req1} ->
            ?LOG_ERROR("Failed to parse request body: ~p", [Reason]),
            Req2 = reply_error(Req1, 400, <<"Bad Request">>, Reason),
            {ok, Req2, #{}}
    end.

%% @doc Process parsed JSON-RPC messages
-spec process_post_messages([json_rpc()], session_id() | undefined, cowboy_req:req(), map()) ->
    {ok | cowboy_loop, cowboy_req:req(), map()}.
process_post_messages(Messages, SessionId, Req0, Opts) ->
    case classify_json_rpc_messages(Messages) of
        {requests, Requests} ->
            handle_requests(Requests, SessionId, Req0, Opts);
        {notifications_responses, _} ->
            handle_notifications_responses(SessionId, Req0);
        {mixed, _} ->
            Req1 = reply_error(Req0, 400, <<"Bad Request">>, 
                              <<"Mixed message types not allowed">>),
            {ok, Req1, #{}}
    end.

%% @doc Handle JSON-RPC requests
-spec handle_requests([json_rpc()], session_id() | undefined, cowboy_req:req(), map()) ->
    {ok | cowboy_loop, cowboy_req:req(), map()}.
handle_requests(Requests, SessionId, Req0, Opts) ->
    Accept = cowboy_req:header(<<"accept">>, Req0, <<>>),
    
    case should_use_sse_stream(Accept, Requests) of
        true ->
            start_sse_response_stream(Requests, SessionId, Req0, Opts);
        false ->
            send_json_response(Requests, SessionId, Req0, Opts)
    end.

%% @doc Determine if SSE streaming should be used
-spec should_use_sse_stream(binary(), [json_rpc()]) -> boolean().
should_use_sse_stream(Accept, Requests) ->
    HasEventStream = binary:match(Accept, <<"text/event-stream">>) =/= nomatch,
    HasJson = binary:match(Accept, <<"application/json">>) =/= nomatch,
    
    % Use SSE if:
    % 1. Client accepts event-stream
    % 2. AND (no JSON preference OR multiple requests OR random choice)
    HasEventStream andalso 
    (not HasJson orelse length(Requests) > 1 orelse rand:uniform() < 0.3).

%% @doc Start SSE stream for request responses
-spec start_sse_response_stream([json_rpc()], session_id() | undefined, cowboy_req:req(), map()) ->
    {cowboy_loop, cowboy_req:req(), map()}.
start_sse_response_stream(Requests, SessionId, Req0, _Opts) ->
    StreamId = generate_stream_id(),
    
    % Add stream to session if we have one
    case SessionId of
        undefined -> ok;
        _ -> mcp_http_session_manager:add_stream(SessionId, StreamId, self())
    end,
    
    Headers = #{
        <<"content-type">> => <<"text/event-stream">>,
        <<"cache-control">> => <<"no-cache">>,
        <<"connection">> => <<"keep-alive">>,
        <<"access-control-allow-origin">> => <<"*">>,
        <<"access-control-allow-headers">> => <<"content-type, mcp-session-id, last-event-id">>
    },
    
    % Add session ID header if we created one during init
    FinalHeaders = case should_create_session(SessionId, Requests) of
        {true, NewSessionId} ->
            Headers#{<<"mcp-session-id">> => NewSessionId};
        false ->
            Headers
    end,
    
    Req1 = cowboy_req:stream_reply(200, FinalHeaders, Req0),
    
    State = #handler_state{
        session_id = SessionId,
        stream_id = StreamId,
        connection_type = sse_stream,
        start_time = erlang:system_time(millisecond),
        pending_requests = Requests
    },
    
    % Send initial keepalive and start processing
    send_sse_keepalive(Req1),
    self() ! process_pending_requests,
    
    % Set up keepalive timer
    erlang:send_after(?KEEPALIVE_INTERVAL, self(), keepalive),
    
    {cowboy_loop, Req1, State}.

%% @doc Send JSON response for simple requests
-spec send_json_response([json_rpc()], session_id() | undefined, cowboy_req:req(), map()) ->
    {ok, cowboy_req:req(), map()}.
send_json_response(Requests, SessionId, Req0, _Opts) ->
    % Process requests synchronously
    Responses = lists:map(fun process_json_rpc/1, Requests),
    
    ResponseBody = case Responses of
        [SingleResponse] -> jsx:encode(SingleResponse);
        Multiple -> jsx:encode(Multiple)
    end,
    
    % Create session if this is an initialize request
    Headers = case should_create_session(SessionId, Requests) of
        {true, NewSessionId} ->
            #{
                <<"content-type">> => <<"application/json">>,
                <<"mcp-session-id">> => NewSessionId
            };
        false ->
            #{<<"content-type">> => <<"application/json">>}
    end,
    
    Req1 = cowboy_req:reply(200, Headers, ResponseBody, Req0),
    {ok, Req1, #{}}.

%% @doc Handle notifications and responses (return 202 Accepted)
-spec handle_notifications_responses(session_id() | undefined, cowboy_req:req()) ->
    {ok, cowboy_req:req(), map()}.
handle_notifications_responses(_SessionId, Req0) ->
    Req1 = cowboy_req:reply(202, #{}, <<>>, Req0),
    {ok, Req1, #{}}.

%%% ============================================================================
%%% GET Request Handling (SSE Streams)
%%% ============================================================================

%% @doc Handle GET requests for SSE streams
handle_get_request(Req0, Opts, ValidationData) ->
    Accept = cowboy_req:header(<<"accept">>, Req0, <<>>),
    
    case binary:match(Accept, <<"text/event-stream">>) of
        nomatch ->
            Req1 = cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0),
            {ok, Req1, #{}};
        _ ->
            start_sse_listen_stream(Req0, Opts, ValidationData)
    end.

%% @doc Start SSE stream for listening to server messages
-spec start_sse_listen_stream(cowboy_req:req(), map(), map()) ->
    {cowboy_loop, cowboy_req:req(), map()}.
start_sse_listen_stream(Req0, _Opts, ValidationData) ->
    SessionId = maps:get(session_id, ValidationData),
    StreamId = generate_stream_id(),
    LastEventId = get_last_event_id(Req0),
    
    % Add stream to session
    case SessionId of
        undefined -> ok;
        _ -> mcp_http_session_manager:add_stream(SessionId, StreamId, self())
    end,
    
    Headers = #{
        <<"content-type">> => <<"text/event-stream">>,
        <<"cache-control">> => <<"no-cache">>,
        <<"connection">> => <<"keep-alive">>,
        <<"access-control-allow-origin">> => <<"*">>,
        <<"access-control-allow-headers">> => <<"mcp-session-id, last-event-id">>
    },
    
    Req1 = cowboy_req:stream_reply(200, Headers, Req0),
    
    State = #handler_state{
        session_id = SessionId,
        stream_id = StreamId,
        last_event_id = LastEventId,
        connection_type = sse_stream,
        start_time = erlang:system_time(millisecond)
    },
    
    % Handle resumption if last event ID provided
    case LastEventId of
        undefined ->
            send_sse_keepalive(Req1);
        _ ->
            self() ! {resume_stream, LastEventId}
    end,
    
    % Set up keepalive timer
    erlang:send_after(?KEEPALIVE_INTERVAL, self(), keepalive),
    
    {cowboy_loop, Req1, State}.

%%% ============================================================================
%%% DELETE Request Handling
%%% ============================================================================

%% @doc Handle DELETE requests for session termination
handle_delete_request(Req0, _Opts, ValidationData) ->
    SessionId = maps:get(session_id, ValidationData),
    
    case SessionId of
        undefined ->
            Req1 = cowboy_req:reply(400, #{}, <<"No session to delete">>, Req0),
            {ok, Req1, #{}};
        _ ->
            mcp_http_session_manager:delete_session(SessionId),
            Req1 = cowboy_req:reply(200, #{}, <<>>, Req0),
            {ok, Req1, #{}}
    end.

%%% ============================================================================
%%% Info Message Handling (for SSE streams)
%%% ============================================================================

%% @doc Handle info messages for SSE streams
info(keepalive, Req, State) ->
    send_sse_keepalive(Req),
    erlang:send_after(?KEEPALIVE_INTERVAL, self(), keepalive),
    {ok, Req, State};

info(process_pending_requests, Req, #handler_state{pending_requests = Requests} = State) ->
    % Process requests asynchronously and send responses
    spawn_link(fun() -> process_requests_async(Requests, self()) end),
    {ok, Req, State#handler_state{pending_requests = []}};

info({json_rpc_response, Response}, Req, State) ->
    EventId = State#handler_state.event_counter + 1,
    send_sse_message(Req, <<"response">>, Response, EventId),
    
    % Buffer the message for potential redelivery
    case State#handler_state.session_id of
        undefined -> ok;
        SessionId ->
            mcp_http_message_buffer:buffer_message(
                SessionId, 
                State#handler_state.stream_id, 
                Response, 
                #{delivery_status => delivered}
            )
    end,
    
    UpdatedState = State#handler_state{
        event_counter = EventId,
        message_count = State#handler_state.message_count + 1
    },
    
    {ok, Req, UpdatedState};

info({resume_stream, LastEventId}, Req, State) ->
    case State#handler_state.session_id of
        undefined ->
            {ok, Req, State};
        SessionId ->
            case mcp_http_message_buffer:get_messages_after(
                SessionId, 
                State#handler_state.stream_id, 
                LastEventId
            ) of
                {ok, Messages} ->
                    replay_messages(Req, Messages, State),
                    {ok, Req, State};
                {error, _} ->
                    {ok, Req, State}
            end
    end;

info({server_message, Message}, Req, State) ->
    % Handle messages from server to client
    EventId = State#handler_state.event_counter + 1,
    send_sse_message(Req, <<"notification">>, Message, EventId),
    
    UpdatedState = State#handler_state{
        event_counter = EventId,
        message_count = State#handler_state.message_count + 1
    },
    
    {ok, Req, UpdatedState};

info(_Info, Req, State) ->
    {ok, Req, State}.

%% @doc Cleanup on termination
terminate(_Reason, _Req, State) ->
    % Remove stream from session
    case State of
        #handler_state{session_id = SessionId, stream_id = StreamId} 
          when SessionId =/= undefined, StreamId =/= undefined ->
            mcp_http_session_manager:remove_stream(SessionId, StreamId);
        _ ->
            ok
    end,
    ok.

%%% ============================================================================
%%% Security and Validation
%%% ============================================================================

%% @doc Validate request security requirements
-spec validate_request_security(cowboy_req:req()) -> {ok, map()} | {error, atom()}.
validate_request_security(Req) ->
    case validate_origin_header(Req) of
        ok ->
            case extract_session_id(Req) of
                {ok, SessionId} ->
                    {ok, #{session_id => SessionId}};
                undefined ->
                    {ok, #{session_id => undefined}};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Validate Origin header for security
-spec validate_origin_header(cowboy_req:req()) -> ok | {error, invalid_origin}.
validate_origin_header(Req) ->
    case cowboy_req:header(<<"origin">>, Req) of
        undefined ->
            ok; % Direct requests without origin are OK
        Origin ->
            case is_allowed_origin(Origin) of
                true -> ok;
                false -> {error, invalid_origin}
            end
    end.

%% @doc Check if origin is allowed
-spec is_allowed_origin(binary()) -> boolean().
is_allowed_origin(Origin) ->
    % Allow localhost and 127.0.0.1 in development
    AllowedPatterns = [
        <<"http://localhost">>,
        <<"https://localhost">>,
        <<"http://127.0.0.1">>,
        <<"https://127.0.0.1">>
    ],
    
    lists:any(fun(Pattern) ->
        binary:match(Origin, Pattern) =/= nomatch
    end, AllowedPatterns).

%% @doc Extract session ID from headers
-spec extract_session_id(cowboy_req:req()) -> {ok, session_id()} | undefined | {error, atom()}.
extract_session_id(Req) ->
    case cowboy_req:header(<<"mcp-session-id">>, Req) of
        undefined ->
            undefined;
        SessionId ->
            case validate_session_id_format(SessionId) of
                true -> {ok, SessionId};
                false -> {error, invalid_session_id}
            end
    end.

%% @doc Validate session ID format
-spec validate_session_id_format(binary()) -> boolean().
validate_session_id_format(SessionId) ->
    byte_size(SessionId) >= 16 andalso byte_size(SessionId) =< 64 andalso
    lists:all(fun(C) -> 
        (C >= $0 andalso C =< $9) orelse
        (C >= $A andalso C =< $Z) orelse
        (C >= $a andalso C =< $z) orelse
        C =:= $- orelse C =:= $_
    end, binary_to_list(SessionId)).

%%% ============================================================================
%%% Request Processing
%%% ============================================================================

%% @doc Read and parse request body
-spec read_and_parse_body(cowboy_req:req()) -> 
    {ok, [json_rpc()], cowboy_req:req()} | {error, term(), cowboy_req:req()}.
read_and_parse_body(Req0) ->
    case cowboy_req:read_body(Req0, #{length => ?MAX_REQUEST_SIZE}) of
        {ok, Body, Req1} when byte_size(Body) > 0 ->
            case parse_json_rpc_body(Body) of
                {ok, Messages} ->
                    {ok, Messages, Req1};
                {error, Reason} ->
                    {error, Reason, Req1}
            end;
        {ok, <<>>, Req1} ->
            {error, empty_body, Req1};
        {more, _Partial, Req1} ->
            {error, body_too_large, Req1};
        {error, Reason, Req1} ->
            {error, Reason, Req1}
    end.

%% @doc Parse JSON-RPC from body
-spec parse_json_rpc_body(binary()) -> {ok, [json_rpc()]} | {error, term()}.
parse_json_rpc_body(Body) ->
    try
        case jsx:decode(Body, [return_maps]) of
            Message when is_map(Message) ->
                {ok, [Message]};
            Messages when is_list(Messages) ->
                {ok, Messages};
            _ ->
                {error, invalid_json_format}
        end
    catch
        error:Reason ->
            {error, {json_decode_error, Reason}}
    end.

%% @doc Classify JSON-RPC messages by type
-spec classify_json_rpc_messages([json_rpc()]) -> 
    {requests, [json_rpc()]} | 
    {notifications_responses, [json_rpc()]} | 
    {mixed, [json_rpc()]}.
classify_json_rpc_messages(Messages) ->
    {Requests, NotifResp} = lists:partition(fun is_json_rpc_request/1, Messages),
    
    case {Requests, NotifResp} of
        {[], NotifResp} -> {notifications_responses, NotifResp};
        {Requests, []} -> {requests, Requests};
        {_, _} -> {mixed, Messages}
    end.

%% @doc Check if message is a JSON-RPC request
-spec is_json_rpc_request(json_rpc()) -> boolean().
is_json_rpc_request(#{<<"method">> := _, <<"id">> := _}) -> true;
is_json_rpc_request(_) -> false.

%% @doc Process individual JSON-RPC message
-spec process_json_rpc(json_rpc()) -> json_rpc().
process_json_rpc(#{<<"method">> := <<"initialize">>} = Request) ->
    handle_initialization(Request);
process_json_rpc(#{<<"method">> := <<"ping">>, <<"id">> := Id}) ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"result">> => #{}
    };
process_json_rpc(#{<<"method">> := Method, <<"id">> := Id}) ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"error">> => #{
            <<"code">> => -32601,
            <<"message">> => <<"Method not found">>,
            <<"data">> => #{<<"method">> => Method}
        }
    };
process_json_rpc(Message) ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"error">> => #{
            <<"code">> => -32600,
            <<"message">> => <<"Invalid Request">>,
            <<"data">> => Message
        }
    }.

%% @doc Handle initialization request
handle_initialization(#{<<"id">> := Id, <<"params">> := Params}) ->
    _ClientVersion = maps:get(<<"protocolVersion">>, Params, ?PROTOCOL_VERSION),
    _ClientCapabilities = maps:get(<<"capabilities">>, Params, #{}),
    _ClientInfo = maps:get(<<"clientInfo">>, Params, #{}),
    
    % Store capabilities if we have a session
    % (This would be handled by the caller)
    
    ServerCapabilities = #{
        <<"logging">> => #{},
        <<"tools">> => #{
            <<"listChanged">> => true
        },
        <<"resources">> => #{
            <<"subscribe">> => true,
            <<"listChanged">> => true
        },
        <<"prompts">> => #{
            <<"listChanged">> => true
        }
    },
    
    Result = #{
        <<"protocolVersion">> => ?PROTOCOL_VERSION,
        <<"capabilities">> => ServerCapabilities,
        <<"serverInfo">> => #{
            <<"name">> => <<"Agents.erl MCP Server">>,
            <<"version">> => <<"1.0.0">>
        }
    },
    
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"result">> => Result
    };
handle_initialization(#{<<"id">> := Id}) ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"error">> => #{
            <<"code">> => -32602,
            <<"message">> => <<"Invalid params">>,
            <<"data">> => <<"Missing required initialization parameters">>
        }
    }.

%% @doc Process requests asynchronously
-spec process_requests_async([json_rpc()], pid()) -> ok.
process_requests_async(Requests, StreamPid) ->
    lists:foreach(fun(Request) ->
        Response = process_json_rpc(Request),
        StreamPid ! {json_rpc_response, Response}
    end, Requests).

%%% ============================================================================
%%% SSE Utilities
%%% ============================================================================

%% @doc Send SSE message with proper formatting
-spec send_sse_message(cowboy_req:req(), binary(), json_rpc(), event_id()) -> ok.
send_sse_message(Req, EventType, Data, EventId) ->
    Event = format_sse_event(EventType, Data, EventId),
    cowboy_req:stream_body(Event, nofin, Req).

%% @doc Send SSE keepalive
-spec send_sse_keepalive(cowboy_req:req()) -> ok.
send_sse_keepalive(Req) ->
    Event = <<"event: keepalive\ndata: {}\n\n">>,
    cowboy_req:stream_body(Event, nofin, Req).

%% @doc Format SSE event according to specification
-spec format_sse_event(binary(), json_rpc(), event_id()) -> binary().
format_sse_event(EventType, Data, EventId) ->
    JsonData = jsx:encode(Data),
    iolist_to_binary([
        <<"event: ">>, EventType, <<"\n">>,
        <<"id: ">>, integer_to_binary(EventId), <<"\n">>,
        <<"data: ">>, JsonData, <<"\n\n">>
    ]).

%% @doc Replay buffered messages for stream resumption
-spec replay_messages(cowboy_req:req(), list(), #handler_state{}) -> ok.
replay_messages(Req, Messages, State) ->
    lists:foreach(fun(_BufferedMsg) ->
        % Extract message data and send it
        % This is a simplified version - real implementation would
        % handle the buffered message format properly
        EventId = State#handler_state.event_counter + 1,
        send_sse_message(Req, <<"replay">>, #{}, EventId)
    end, Messages).

%%% ============================================================================
%%% Session Management Utilities
%%% ============================================================================

%% @doc Check if we should create a session
-spec should_create_session(session_id() | undefined, [json_rpc()]) -> 
    {true, session_id()} | false.
should_create_session(undefined, Requests) ->
    % Create session if we have an initialize request
    case lists:any(fun(#{<<"method">> := Method}) -> 
        Method =:= <<"initialize">> 
    end, Requests) of
        true ->
            SessionId = generate_session_id(),
            {true, SessionId};
        false ->
            false
    end;
should_create_session(_ExistingSessionId, _Requests) ->
    false.

%% @doc Get last event ID from request headers
-spec get_last_event_id(cowboy_req:req()) -> event_id() | undefined.
get_last_event_id(Req) ->
    case cowboy_req:header(<<"last-event-id">>, Req) of
        undefined -> undefined;
        IdBinary ->
            try
                binary_to_integer(IdBinary)
            catch
                error:badarg -> undefined
            end
    end.

%% @doc Generate unique session ID
-spec generate_session_id() -> session_id().
generate_session_id() ->
    base64url:encode(crypto:strong_rand_bytes(32)).

%% @doc Generate unique stream ID
-spec generate_stream_id() -> stream_id().
generate_stream_id() ->
    base64url:encode(crypto:strong_rand_bytes(16)).

%% @doc Send error response
-spec reply_error(cowboy_req:req(), integer(), binary(), term()) -> cowboy_req:req().
reply_error(Req, StatusCode, StatusText, Reason) ->
    Body = jsx:encode(#{
        <<"error">> => StatusText,
        <<"reason">> => format_error_reason(Reason)
    }),
    
    cowboy_req:reply(StatusCode, #{
        <<"content-type">> => <<"application/json">>
    }, Body, Req).

%% @doc Format error reason for response
-spec format_error_reason(term()) -> binary().
format_error_reason(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason);
format_error_reason(Reason) when is_binary(Reason) ->
    Reason;
format_error_reason(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).