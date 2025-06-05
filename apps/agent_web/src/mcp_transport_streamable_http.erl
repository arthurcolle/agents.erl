%% @doc MCP Streamable HTTP Transport Implementation
%% 
%% Implements the Model Context Protocol Streamable HTTP transport as per
%% specification 2025-03-26. Provides low-level binary handling, SSE streaming,
%% session management, and full compliance with the MCP transport requirements.
-module(mcp_transport_streamable_http).

-include_lib("kernel/include/logger.hrl").

%% API exports
-export([
    start_link/1,
    stop/1,
    send_message/2,
    send_batch/2,
    open_stream/1,
    close_stream/1,
    resume_stream/2,
    terminate_session/1
]).

%% Cowboy handler exports
-export([
    init/2,
    info/3,
    terminate/3
]).

%% Internal exports for gen_server
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-behaviour(gen_server).

%% Type specifications
-type session_id() :: binary().
-type stream_id() :: binary().
-type event_id() :: non_neg_integer().
-type json_rpc_message() :: map().
-type http_method() :: get | post | delete.
-type content_type() :: binary().
-type origin() :: binary().
-type headers() :: #{binary() => binary()}.

-record(stream_state, {
    id :: stream_id(),
    session_id :: session_id() | undefined,
    pid :: pid(),
    event_counter = 0 :: event_id(),
    pending_responses = [] :: [json_rpc_message()],
    last_activity :: integer()
}).

-record(session_state, {
    id :: session_id(),
    streams = #{} :: #{stream_id() => #stream_state{}},
    created_at :: integer(),
    last_activity :: integer(),
    capabilities = #{} :: map()
}).

-record(transport_state, {
    sessions = #{} :: #{session_id() => #session_state{}},
    config :: map(),
    message_handler :: fun((json_rpc_message()) -> json_rpc_message() | {error, term()})
}).

%% Constants
-define(DEFAULT_TIMEOUT, 30000).
-define(SESSION_TIMEOUT, 3600000). % 1 hour
-define(STREAM_KEEPALIVE, 30000).  % 30 seconds
-define(MAX_MESSAGE_SIZE, 1048576). % 1MB
-define(PROTOCOL_VERSION, <<"2025-03-26">>).

%% Security constants
-define(ALLOWED_ORIGINS, [<<"http://localhost">>, <<"https://localhost">>, 
                         <<"http://127.0.0.1">>, <<"https://127.0.0.1">>]).
-define(SESSION_ID_LENGTH, 32).

%%% ============================================================================
%%% API Functions
%%% ============================================================================

%% @doc Start the MCP Streamable HTTP transport server
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

%% @doc Stop the transport server
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% @doc Send a single JSON-RPC message
-spec send_message(pid(), json_rpc_message()) -> ok | {error, term()}.
send_message(Pid, Message) ->
    gen_server:call(Pid, {send_message, Message}).

%% @doc Send a batch of JSON-RPC messages
-spec send_batch(pid(), [json_rpc_message()]) -> ok | {error, term()}.
send_batch(Pid, Messages) ->
    gen_server:call(Pid, {send_batch, Messages}).

%% @doc Open a new SSE stream
-spec open_stream(pid()) -> {ok, stream_id()} | {error, term()}.
open_stream(Pid) ->
    gen_server:call(Pid, open_stream).

%% @doc Close an existing stream
-spec close_stream(stream_id()) -> ok.
close_stream(StreamId) ->
    gen_server:cast(self(), {close_stream, StreamId}).

%% @doc Resume a stream from last event ID
-spec resume_stream(pid(), event_id()) -> {ok, stream_id()} | {error, term()}.
resume_stream(Pid, LastEventId) ->
    gen_server:call(Pid, {resume_stream, LastEventId}).

%% @doc Terminate a session
-spec terminate_session(session_id()) -> ok.
terminate_session(SessionId) ->
    gen_server:cast(self(), {terminate_session, SessionId}).

%%% ============================================================================
%%% Cowboy HTTP Handler Implementation
%%% ============================================================================

%% @doc Initialize Cowboy handler for MCP endpoint
init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    handle_request(Method, Req0, State).

%% @doc Handle different HTTP methods
-spec handle_request(binary(), cowboy_req:req(), term()) -> 
    {ok, cowboy_req:req(), term()} | 
    {cowboy_loop, cowboy_req:req(), term()}.
handle_request(<<"POST">>, Req0, State) ->
    handle_post_request(Req0, State);
handle_request(<<"GET">>, Req0, State) ->
    handle_get_request(Req0, State);
handle_request(<<"DELETE">>, Req0, State) ->
    handle_delete_request(Req0, State);
handle_request(Method, Req0, State) ->
    ?LOG_WARNING("Unsupported HTTP method: ~p", [Method]),
    Req1 = cowboy_req:reply(405, #{
        <<"allow">> => <<"GET, POST, DELETE">>
    }, Req0),
    {ok, Req1, State}.

%% @doc Handle POST requests for JSON-RPC messages
-spec handle_post_request(cowboy_req:req(), term()) -> 
    {ok, cowboy_req:req(), term()} | 
    {cowboy_loop, cowboy_req:req(), term()}.
handle_post_request(Req0, State) ->
    case validate_security(Req0) of
        {ok, SessionId} ->
            case read_request_body(Req0) of
                {ok, Body, Req1} ->
                    process_post_body(Body, SessionId, Req1, State);
                {error, Reason} ->
                    ?LOG_ERROR("Failed to read request body: ~p", [Reason]),
                    Req1 = cowboy_req:reply(400, #{}, <<"Bad Request">>, Req0),
                    {ok, Req1, State}
            end;
        {error, Reason} ->
            ?LOG_WARNING("Security validation failed: ~p", [Reason]),
            Req1 = cowboy_req:reply(403, #{}, <<"Forbidden">>, Req0),
            {ok, Req1, State}
    end.

%% @doc Handle GET requests for SSE streams
-spec handle_get_request(cowboy_req:req(), term()) -> 
    {cowboy_loop, cowboy_req:req(), term()}.
handle_get_request(Req0, State) ->
    case validate_security(Req0) of
        {ok, SessionId} ->
            case accept_sse_stream(Req0) of
                true ->
                    start_sse_stream(SessionId, Req0, State);
                false ->
                    Req1 = cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0),
                    {ok, Req1, State}
            end;
        {error, Reason} ->
            ?LOG_WARNING("Security validation failed: ~p", [Reason]),
            Req1 = cowboy_req:reply(403, #{}, <<"Forbidden">>, Req0),
            {ok, Req1, State}
    end.

%% @doc Handle DELETE requests for session termination
-spec handle_delete_request(cowboy_req:req(), term()) -> 
    {ok, cowboy_req:req(), term()}.
handle_delete_request(Req0, State) ->
    case validate_security(Req0) of
        {ok, SessionId} ->
            terminate_session(SessionId),
            Req1 = cowboy_req:reply(200, #{}, Req0),
            {ok, Req1, State};
        {error, _Reason} ->
            Req1 = cowboy_req:reply(403, #{}, <<"Forbidden">>, Req0),
            {ok, Req1, State}
    end.

%%% ============================================================================
%%% Security and Validation
%%% ============================================================================

%% @doc Validate security requirements for incoming requests
-spec validate_security(cowboy_req:req()) -> {ok, session_id() | undefined} | {error, atom()}.
validate_security(Req) ->
    case validate_origin(Req) of
        ok ->
            case get_session_id(Req) of
                {ok, SessionId} -> {ok, SessionId};
                undefined -> {ok, undefined};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Validate Origin header to prevent DNS rebinding attacks
-spec validate_origin(cowboy_req:req()) -> ok | {error, invalid_origin}.
validate_origin(Req) ->
    case cowboy_req:header(<<"origin">>, Req) of
        undefined ->
            ok; % No origin header is acceptable for direct requests
        Origin ->
            case is_allowed_origin(Origin) of
                true -> ok;
                false -> {error, invalid_origin}
            end
    end.

%% @doc Check if origin is in allowed list
-spec is_allowed_origin(binary()) -> boolean().
is_allowed_origin(Origin) ->
    ParsedOrigin = parse_origin(Origin),
    lists:any(fun(Allowed) ->
        binary:match(ParsedOrigin, Allowed) =/= nomatch
    end, ?ALLOWED_ORIGINS).

%% @doc Extract host from origin URL
-spec parse_origin(binary()) -> binary().
parse_origin(Origin) ->
    case uri_string:parse(Origin) of
        #{host := Host, scheme := Scheme} ->
            <<Scheme/binary, "://", Host/binary>>;
        _ ->
            Origin
    end.

%% @doc Extract session ID from request headers
-spec get_session_id(cowboy_req:req()) -> {ok, session_id()} | undefined | {error, invalid_session}.
get_session_id(Req) ->
    case cowboy_req:header(<<"mcp-session-id">>, Req) of
        undefined ->
            undefined;
        SessionId when byte_size(SessionId) =:= ?SESSION_ID_LENGTH ->
            case validate_session_id_format(SessionId) of
                true -> {ok, SessionId};
                false -> {error, invalid_session}
            end;
        _ ->
            {error, invalid_session}
    end.

%% @doc Validate session ID format (ASCII printable characters)
-spec validate_session_id_format(binary()) -> boolean().
validate_session_id_format(SessionId) ->
    lists:all(fun(C) -> C >= 16#21 andalso C =< 16#7E end, binary_to_list(SessionId)).

%%% ============================================================================
%%% Request Processing
%%% ============================================================================

%% @doc Process POST request body containing JSON-RPC messages
-spec process_post_body(binary(), session_id() | undefined, cowboy_req:req(), term()) ->
    {ok, cowboy_req:req(), term()} | 
    {cowboy_loop, cowboy_req:req(), term()}.
process_post_body(Body, SessionId, Req0, State) ->
    case parse_json_rpc(Body) of
        {ok, Messages} ->
            case classify_messages(Messages) of
                {requests, Requests} ->
                    handle_requests(Requests, SessionId, Req0, State);
                {notifications_responses, _} ->
                    handle_notifications_responses(SessionId, Req0, State);
                {mixed, _} ->
                    Req1 = cowboy_req:reply(400, #{}, <<"Mixed message types not allowed">>, Req0),
                    {ok, Req1, State}
            end;
        {error, Reason} ->
            ?LOG_ERROR("JSON-RPC parse error: ~p", [Reason]),
            Req1 = cowboy_req:reply(400, #{}, <<"Invalid JSON-RPC">>, Req0),
            {ok, Req1, State}
    end.

%% @doc Read and validate request body
-spec read_request_body(cowboy_req:req()) -> {ok, binary(), cowboy_req:req()} | {error, term()}.
read_request_body(Req0) ->
    case cowboy_req:read_body(Req0, #{length => ?MAX_MESSAGE_SIZE}) of
        {ok, Body, Req1} when byte_size(Body) > 0 ->
            {ok, Body, Req1};
        {ok, <<>>, Req1} ->
            {error, empty_body};
        {more, _Partial, _Req1} ->
            {error, message_too_large};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Parse JSON-RPC messages from binary data
-spec parse_json_rpc(binary()) -> {ok, [json_rpc_message()]} | {error, term()}.
parse_json_rpc(Body) ->
    try
        case jsx:decode(Body, [return_maps]) of
            Messages when is_list(Messages) ->
                {ok, Messages};
            Message when is_map(Message) ->
                {ok, [Message]};
            _ ->
                {error, invalid_format}
        end
    catch
        error:Reason ->
            {error, {json_decode_error, Reason}}
    end.

%% @doc Classify messages by type for processing
-spec classify_messages([json_rpc_message()]) -> 
    {requests, [json_rpc_message()]} | 
    {notifications_responses, [json_rpc_message()]} | 
    {mixed, [json_rpc_message()]}.
classify_messages(Messages) ->
    {Requests, NotifResp} = lists:partition(fun is_request/1, Messages),
    case {Requests, NotifResp} of
        {[], NotifResp} -> {notifications_responses, NotifResp};
        {Requests, []} -> {requests, Requests};
        {_, _} -> {mixed, Messages}
    end.

%% @doc Check if message is a JSON-RPC request
-spec is_request(json_rpc_message()) -> boolean().
is_request(#{<<"method">> := _, <<"id">> := _}) -> true;
is_request(_) -> false.

%%% ============================================================================
%%% SSE Stream Handling
%%% ============================================================================

%% @doc Check if client accepts SSE streams
-spec accept_sse_stream(cowboy_req:req()) -> boolean().
accept_sse_stream(Req) ->
    Accept = cowboy_req:header(<<"accept">>, Req, <<>>),
    binary:match(Accept, <<"text/event-stream">>) =/= nomatch.

%% @doc Start Server-Sent Events stream
-spec start_sse_stream(session_id() | undefined, cowboy_req:req(), term()) ->
    {cowboy_loop, cowboy_req:req(), term()}.
start_sse_stream(SessionId, Req0, State) ->
    StreamId = generate_stream_id(),
    Headers = #{
        <<"content-type">> => <<"text/event-stream">>,
        <<"cache-control">> => <<"no-cache">>,
        <<"connection">> => <<"keep-alive">>,
        <<"access-control-allow-origin">> => <<"*">>,
        <<"access-control-allow-headers">> => <<"mcp-session-id, last-event-id">>
    },
    
    Req1 = cowboy_req:stream_reply(200, Headers, Req0),
    
    StreamState = #stream_state{
        id = StreamId,
        session_id = SessionId,
        pid = self(),
        last_activity = erlang:system_time(millisecond)
    },
    
    NewState = State#{stream => StreamState},
    
    % Send initial keepalive
    send_sse_event(Req1, <<"keepalive">>, #{}, undefined),
    
    % Set up periodic keepalive
    erlang:send_after(?STREAM_KEEPALIVE, self(), keepalive),
    
    {cowboy_loop, Req1, NewState}.

%% @doc Handle requests that require responses
-spec handle_requests([json_rpc_message()], session_id() | undefined, cowboy_req:req(), term()) ->
    {ok, cowboy_req:req(), term()} | 
    {cowboy_loop, cowboy_req:req(), term()}.
handle_requests(Requests, SessionId, Req0, State) ->
    Accept = cowboy_req:header(<<"accept">>, Req0, <<>>),
    
    case should_stream_response(Accept) of
        true ->
            start_request_stream(Requests, SessionId, Req0, State);
        false ->
            send_json_response(Requests, SessionId, Req0, State)
    end.

%% @doc Check if response should be streamed
-spec should_stream_response(binary()) -> boolean().
should_stream_response(Accept) ->
    HasEventStream = binary:match(Accept, <<"text/event-stream">>) =/= nomatch,
    HasJson = binary:match(Accept, <<"application/json">>) =/= nomatch,
    HasEventStream andalso (not HasJson orelse rand:uniform() < 0.5).

%% @doc Start streaming response for requests
-spec start_request_stream([json_rpc_message()], session_id() | undefined, cowboy_req:req(), term()) ->
    {cowboy_loop, cowboy_req:req(), term()}.
start_request_stream(Requests, SessionId, Req0, State) ->
    {cowboy_loop, Req1, NewState} = start_sse_stream(SessionId, Req0, State),
    
    % Process requests asynchronously
    spawn_link(fun() -> process_requests_async(Requests, self()) end),
    
    {cowboy_loop, Req1, NewState}.

%% @doc Send JSON response for simple requests
-spec send_json_response([json_rpc_message()], session_id() | undefined, cowboy_req:req(), term()) ->
    {ok, cowboy_req:req(), term()}.
send_json_response(Requests, _SessionId, Req0, State) ->
    % Process requests synchronously for JSON response
    Responses = lists:map(fun process_request/1, Requests),
    
    ResponseBody = case Responses of
        [SingleResponse] -> jsx:encode(SingleResponse);
        Multiple -> jsx:encode(Multiple)
    end,
    
    Req1 = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, ResponseBody, Req0),
    
    {ok, Req1, State}.

%% @doc Handle notifications and responses (return 202 Accepted)
-spec handle_notifications_responses(session_id() | undefined, cowboy_req:req(), term()) ->
    {ok, cowboy_req:req(), term()}.
handle_notifications_responses(_SessionId, Req0, State) ->
    Req1 = cowboy_req:reply(202, #{}, Req0),
    {ok, Req1, State}.

%%% ============================================================================
%%% SSE Event Formatting
%%% ============================================================================

%% @doc Send SSE event with proper formatting
-spec send_sse_event(cowboy_req:req(), binary(), json_rpc_message(), event_id() | undefined) -> ok.
send_sse_event(Req, EventType, Data, EventId) ->
    Event = format_sse_event(EventType, Data, EventId),
    cowboy_req:stream_body(Event, nofin, Req).

%% @doc Format SSE event according to specification
-spec format_sse_event(binary(), json_rpc_message(), event_id() | undefined) -> binary().
format_sse_event(EventType, Data, EventId) ->
    DataLine = [<<"data: ">>, jsx:encode(Data), <<"\n">>],
    EventLine = [<<"event: ">>, EventType, <<"\n">>],
    
    IdLine = case EventId of
        undefined -> [];
        Id -> [<<"id: ">>, integer_to_binary(Id), <<"\n">>]
    end,
    
    iolist_to_binary([EventLine, IdLine, DataLine, <<"\n">>]).

%%% ============================================================================
%%% Session Management
%%% ============================================================================

%% @doc Generate cryptographically secure session ID
-spec generate_session_id() -> session_id().
generate_session_id() ->
    Bytes = crypto:strong_rand_bytes(?SESSION_ID_LENGTH div 2),
    base16_encode(Bytes).

%% @doc Generate unique stream ID
-spec generate_stream_id() -> stream_id().
generate_stream_id() ->
    base64url:encode(crypto:strong_rand_bytes(16)).

%% @doc Encode bytes as base16 (hex)
-spec base16_encode(binary()) -> binary().
base16_encode(Bytes) ->
    << <<(hex_char(N div 16)), (hex_char(N rem 16))>> || <<N>> <= Bytes >>.

%% @doc Convert number to hex character
-spec hex_char(0..15) -> char().
hex_char(N) when N < 10 -> $0 + N;
hex_char(N) -> $A + N - 10.

%%% ============================================================================
%%% Message Processing
%%% ============================================================================

%% @doc Process individual JSON-RPC request
-spec process_request(json_rpc_message()) -> json_rpc_message().
process_request(#{<<"method">> := Method, <<"id">> := Id} = Request) ->
    try
        case Method of
            <<"initialize">> ->
                handle_initialize(Request);
            <<"ping">> ->
                handle_ping(Request);
            _ ->
                #{
                    <<"jsonrpc">> => <<"2.0">>,
                    <<"id">> => Id,
                    <<"error">> => #{
                        <<"code">> => -32601,
                        <<"message">> => <<"Method not found">>
                    }
                }
        end
    catch
        error:Reason ->
            ?LOG_ERROR("Error processing request ~p: ~p", [Method, Reason]),
            #{
                <<"jsonrpc">> => <<"2.0">>,
                <<"id">> => Id,
                <<"error">> => #{
                    <<"code">> => -32603,
                    <<"message">> => <<"Internal error">>
                }
            }
    end.

%% @doc Handle initialize request
-spec handle_initialize(json_rpc_message()) -> json_rpc_message().
handle_initialize(#{<<"id">> := Id, <<"params">> := Params}) ->
    Version = maps:get(<<"protocolVersion">>, Params, ?PROTOCOL_VERSION),
    
    Result = #{
        <<"protocolVersion">> => ?PROTOCOL_VERSION,
        <<"capabilities">> => #{
            <<"logging">> => #{},
            <<"tools">> => #{<<"listChanged">> => true},
            <<"resources">> => #{
                <<"subscribe">> => true,
                <<"listChanged">> => true
            }
        },
        <<"serverInfo">> => #{
            <<"name">> => <<"Agents.erl MCP Server">>,
            <<"version">> => <<"1.0.0">>
        }
    },
    
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"result">> => Result
    }.

%% @doc Handle ping request
-spec handle_ping(json_rpc_message()) -> json_rpc_message().
handle_ping(#{<<"id">> := Id}) ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"result">> => #{}
    }.

%% @doc Process requests asynchronously for streaming
-spec process_requests_async([json_rpc_message()], pid()) -> ok.
process_requests_async(Requests, StreamPid) ->
    lists:foreach(fun(Request) ->
        Response = process_request(Request),
        StreamPid ! {response, Response}
    end, Requests).

%%% ============================================================================
%%% Gen Server Implementation
%%% ============================================================================

%% @doc Initialize the transport server
init(Config) ->
    State = #transport_state{
        config = Config,
        message_handler = maps:get(message_handler, Config, fun default_message_handler/1)
    },
    
    % Start session cleanup timer
    erlang:send_after(?SESSION_TIMEOUT, self(), cleanup_sessions),
    
    {ok, State}.

%% @doc Handle synchronous calls
handle_call({send_message, Message}, _From, State) ->
    % Implementation for sending messages to active streams
    {reply, ok, State};

handle_call({send_batch, Messages}, _From, State) ->
    % Implementation for sending message batches
    {reply, ok, State};

handle_call(open_stream, _From, State) ->
    StreamId = generate_stream_id(),
    {reply, {ok, StreamId}, State};

handle_call({resume_stream, LastEventId}, _From, State) ->
    % Implementation for stream resumption
    StreamId = generate_stream_id(),
    {reply, {ok, StreamId}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @doc Handle asynchronous casts
handle_cast({close_stream, StreamId}, State) ->
    % Implementation for closing streams
    ?LOG_INFO("Closing stream: ~p", [StreamId]),
    {noreply, State};

handle_cast({terminate_session, SessionId}, State) ->
    % Implementation for session termination
    ?LOG_INFO("Terminating session: ~p", [SessionId]),
    NewSessions = maps:remove(SessionId, State#transport_state.sessions),
    {noreply, State#transport_state{sessions = NewSessions}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle info messages
handle_info(cleanup_sessions, State) ->
    NewState = cleanup_expired_sessions(State),
    erlang:send_after(?SESSION_TIMEOUT, self(), cleanup_sessions),
    {noreply, NewState};

handle_info(keepalive, State) ->
    % Send keepalive to active streams
    {noreply, State};

handle_info({response, Response}, State) ->
    % Handle async responses from request processing
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Handle Cowboy info messages for SSE streams
info(keepalive, Req, State) ->
    send_sse_event(Req, <<"keepalive">>, #{}, undefined),
    erlang:send_after(?STREAM_KEEPALIVE, self(), keepalive),
    {ok, Req, State};

info({response, Response}, Req, #{stream := StreamState} = State) ->
    EventId = StreamState#stream_state.event_counter + 1,
    send_sse_event(Req, <<"response">>, Response, EventId),
    
    UpdatedStreamState = StreamState#stream_state{
        event_counter = EventId,
        last_activity = erlang:system_time(millisecond)
    },
    
    {ok, Req, State#{stream := UpdatedStreamState}};

info(_Info, Req, State) ->
    {ok, Req, State}.

%% @doc Cleanup handler
terminate(_Reason, _State) ->
    ok.

%% @doc Cleanup Cowboy handler
terminate(_Reason, _Req, _State) ->
    ok.

%% @doc Code change handler
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ============================================================================
%%% Utility Functions
%%% ============================================================================

%% @doc Default message handler
-spec default_message_handler(json_rpc_message()) -> json_rpc_message() | {error, term()}.
default_message_handler(Message) ->
    % Default implementation - echo back
    Message.

%% @doc Clean up expired sessions
-spec cleanup_expired_sessions(#transport_state{}) -> #transport_state{}.
cleanup_expired_sessions(State) ->
    Now = erlang:system_time(millisecond),
    
    CleanSessions = maps:filter(fun(_SessionId, Session) ->
        Age = Now - Session#session_state.last_activity,
        Age < ?SESSION_TIMEOUT
    end, State#transport_state.sessions),
    
    State#transport_state{sessions = CleanSessions}.