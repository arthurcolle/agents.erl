%% @doc MCP HTTP Session Manager
%%
%% Advanced session management for MCP Streamable HTTP transport with
%% persistence, resumability, and security features.
-module(mcp_http_session_manager).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

%% API exports
-export([
    start_link/1,
    create_session/2,
    get_session/1,
    update_session/2,
    delete_session/1,
    list_sessions/0,
    add_stream/3,
    remove_stream/2,
    get_stream/2,
    cleanup_expired/0
]).

%% Gen server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% Types
-type session_id() :: binary().
-type stream_id() :: binary().
-type capabilities() :: map().
-type client_info() :: #{
    name := binary(),
    version := binary()
}.

-record(session, {
    id :: session_id(),
    created_at :: integer(),
    last_activity :: integer(),
    capabilities :: capabilities(),
    client_info :: client_info(),
    streams = #{} :: #{stream_id() => stream_state()},
    message_count = 0 :: non_neg_integer(),
    error_count = 0 :: non_neg_integer()
}).

-record(stream_state, {
    id :: stream_id(),
    created_at :: integer(),
    last_activity :: integer(),
    event_counter = 0 :: non_neg_integer(),
    pending_messages = [] :: [map()],
    last_event_id :: non_neg_integer() | undefined,
    connection_pid :: pid() | undefined
}).

-type stream_state() :: #stream_state{}.

-record(state, {
    sessions = #{} :: #{session_id() => #session{}},
    config :: map(),
    storage_module :: atom(),
    cleanup_timer :: reference()
}).

%% Constants
-define(DEFAULT_SESSION_TIMEOUT, 3600000). % 1 hour
-define(DEFAULT_STREAM_TIMEOUT, 300000).   % 5 minutes
-define(CLEANUP_INTERVAL, 60000).          % 1 minute
-define(MAX_PENDING_MESSAGES, 1000).
-define(SESSION_ID_CHARS, "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz").

%%% ============================================================================
%%% API Functions
%%% ============================================================================

%% @doc Start the session manager
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%% @doc Create a new session
-spec create_session(capabilities(), client_info()) -> {ok, session_id()} | {error, term()}.
create_session(Capabilities, ClientInfo) ->
    gen_server:call(?MODULE, {create_session, Capabilities, ClientInfo}).

%% @doc Get session by ID
-spec get_session(session_id()) -> {ok, #session{}} | {error, not_found}.
get_session(SessionId) ->
    gen_server:call(?MODULE, {get_session, SessionId}).

%% @doc Update session last activity
-spec update_session(session_id(), map()) -> ok | {error, not_found}.
update_session(SessionId, Updates) ->
    gen_server:call(?MODULE, {update_session, SessionId, Updates}).

%% @doc Delete session
-spec delete_session(session_id()) -> ok.
delete_session(SessionId) ->
    gen_server:cast(?MODULE, {delete_session, SessionId}).

%% @doc List all active sessions
-spec list_sessions() -> [#session{}].
list_sessions() ->
    gen_server:call(?MODULE, list_sessions).

%% @doc Add stream to session
-spec add_stream(session_id(), stream_id(), pid()) -> ok | {error, term()}.
add_stream(SessionId, StreamId, ConnectionPid) ->
    gen_server:call(?MODULE, {add_stream, SessionId, StreamId, ConnectionPid}).

%% @doc Remove stream from session
-spec remove_stream(session_id(), stream_id()) -> ok.
remove_stream(SessionId, StreamId) ->
    gen_server:cast(?MODULE, {remove_stream, SessionId, StreamId}).

%% @doc Get stream state
-spec get_stream(session_id(), stream_id()) -> {ok, #stream_state{}} | {error, not_found}.
get_stream(SessionId, StreamId) ->
    gen_server:call(?MODULE, {get_stream, SessionId, StreamId}).

%% @doc Cleanup expired sessions and streams
-spec cleanup_expired() -> ok.
cleanup_expired() ->
    gen_server:cast(?MODULE, cleanup_expired).

%%% ============================================================================
%%% Gen Server Implementation
%%% ============================================================================

%% @doc Initialize session manager
init(Config) ->
    SessionTimeout = maps:get(session_timeout, Config, ?DEFAULT_SESSION_TIMEOUT),
    StorageModule = maps:get(storage_module, Config, ets),
    
    State = #state{
        config = Config#{session_timeout => SessionTimeout},
        storage_module = StorageModule
    },
    
    % Initialize storage
    initialize_storage(StorageModule),
    
    % Start cleanup timer
    Timer = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup_expired),
    
    {ok, State#state{cleanup_timer = Timer}}.

%% @doc Handle synchronous calls
handle_call({create_session, Capabilities, ClientInfo}, _From, State) ->
    SessionId = generate_session_id(),
    Now = erlang:system_time(millisecond),
    
    Session = #session{
        id = SessionId,
        created_at = Now,
        last_activity = Now,
        capabilities = Capabilities,
        client_info = ClientInfo
    },
    
    NewSessions = maps:put(SessionId, Session, State#state.sessions),
    store_session(State#state.storage_module, Session),
    
    ?LOG_INFO("Created session ~s for client ~s", 
              [SessionId, maps:get(name, ClientInfo, <<"unknown">>)]),
    
    {reply, {ok, SessionId}, State#state{sessions = NewSessions}};

handle_call({get_session, SessionId}, _From, State) ->
    case maps:get(SessionId, State#state.sessions, undefined) of
        undefined ->
            % Try loading from persistent storage
            case load_session(State#state.storage_module, SessionId) of
                {ok, Session} ->
                    NewSessions = maps:put(SessionId, Session, State#state.sessions),
                    {reply, {ok, Session}, State#state{sessions = NewSessions}};
                {error, not_found} ->
                    {reply, {error, not_found}, State}
            end;
        Session ->
            {reply, {ok, Session}, State}
    end;

handle_call({update_session, SessionId, Updates}, _From, State) ->
    case maps:get(SessionId, State#state.sessions, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Session ->
            UpdatedSession = apply_session_updates(Session, Updates),
            NewSessions = maps:put(SessionId, UpdatedSession, State#state.sessions),
            store_session(State#state.storage_module, UpdatedSession),
            {reply, ok, State#state{sessions = NewSessions}}
    end;

handle_call(list_sessions, _From, State) ->
    Sessions = maps:values(State#state.sessions),
    {reply, Sessions, State};

handle_call({add_stream, SessionId, StreamId, ConnectionPid}, _From, State) ->
    case maps:get(SessionId, State#state.sessions, undefined) of
        undefined ->
            {reply, {error, session_not_found}, State};
        Session ->
            Now = erlang:system_time(millisecond),
            StreamState = #stream_state{
                id = StreamId,
                created_at = Now,
                last_activity = Now,
                connection_pid = ConnectionPid
            },
            
            UpdatedStreams = maps:put(StreamId, StreamState, Session#session.streams),
            UpdatedSession = Session#session{
                streams = UpdatedStreams,
                last_activity = Now
            },
            
            NewSessions = maps:put(SessionId, UpdatedSession, State#state.sessions),
            store_session(State#state.storage_module, UpdatedSession),
            
            % Monitor the connection process
            erlang:monitor(process, ConnectionPid),
            
            ?LOG_DEBUG("Added stream ~s to session ~s", [StreamId, SessionId]),
            {reply, ok, State#state{sessions = NewSessions}}
    end;

handle_call({get_stream, SessionId, StreamId}, _From, State) ->
    case maps:get(SessionId, State#state.sessions, undefined) of
        undefined ->
            {reply, {error, session_not_found}, State};
        Session ->
            case maps:get(StreamId, Session#session.streams, undefined) of
                undefined ->
                    {reply, {error, stream_not_found}, State};
                StreamState ->
                    {reply, {ok, StreamState}, State}
            end
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @doc Handle asynchronous casts
handle_cast({delete_session, SessionId}, State) ->
    case maps:get(SessionId, State#state.sessions, undefined) of
        undefined ->
            {noreply, State};
        Session ->
            % Close all streams
            maps:foreach(fun(_, StreamState) ->
                case StreamState#stream_state.connection_pid of
                    undefined -> ok;
                    Pid when is_pid(Pid) -> 
                        catch erlang:exit(Pid, session_terminated)
                end
            end, Session#session.streams),
            
            NewSessions = maps:remove(SessionId, State#state.sessions),
            delete_session_storage(State#state.storage_module, SessionId),
            
            ?LOG_INFO("Deleted session ~s", [SessionId]),
            {noreply, State#state{sessions = NewSessions}}
    end;

handle_cast({remove_stream, SessionId, StreamId}, State) ->
    case maps:get(SessionId, State#state.sessions, undefined) of
        undefined ->
            {noreply, State};
        Session ->
            UpdatedStreams = maps:remove(StreamId, Session#session.streams),
            UpdatedSession = Session#session{
                streams = UpdatedStreams,
                last_activity = erlang:system_time(millisecond)
            },
            
            NewSessions = maps:put(SessionId, UpdatedSession, State#state.sessions),
            store_session(State#state.storage_module, UpdatedSession),
            
            ?LOG_DEBUG("Removed stream ~s from session ~s", [StreamId, SessionId]),
            {noreply, State#state{sessions = NewSessions}}
    end;

handle_cast(cleanup_expired, State) ->
    NewState = cleanup_expired_sessions(State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle info messages
handle_info(cleanup_expired, State) ->
    NewState = cleanup_expired_sessions(State),
    Timer = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup_expired),
    {noreply, NewState#state{cleanup_timer = Timer}};

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    % Handle connection process termination
    NewState = remove_dead_streams(State, Pid),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Cleanup on termination
terminate(_Reason, State) ->
    case State#state.cleanup_timer of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,
    ok.

%% @doc Handle code changes
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ============================================================================
%%% Storage Functions
%%% ============================================================================

%% @doc Initialize storage backend
-spec initialize_storage(atom()) -> ok.
initialize_storage(ets) ->
    ets:new(mcp_sessions, [named_table, public, {keypos, #session.id}]),
    ok;
initialize_storage(_Module) ->
    ok.

%% @doc Store session in backend
-spec store_session(atom(), #session{}) -> ok.
store_session(ets, Session) ->
    ets:insert(mcp_sessions, Session),
    ok;
store_session(_Module, _Session) ->
    ok.

%% @doc Load session from backend
-spec load_session(atom(), session_id()) -> {ok, #session{}} | {error, not_found}.
load_session(ets, SessionId) ->
    case ets:lookup(mcp_sessions, SessionId) of
        [Session] -> {ok, Session};
        [] -> {error, not_found}
    end;
load_session(_Module, _SessionId) ->
    {error, not_found}.

%% @doc Delete session from storage
-spec delete_session_storage(atom(), session_id()) -> ok.
delete_session_storage(ets, SessionId) ->
    ets:delete(mcp_sessions, SessionId),
    ok;
delete_session_storage(_Module, _SessionId) ->
    ok.

%%% ============================================================================
%%% Utility Functions
%%% ============================================================================

%% @doc Generate cryptographically secure session ID
-spec generate_session_id() -> session_id().
generate_session_id() ->
    Bytes = crypto:strong_rand_bytes(32),
    base64url:encode(Bytes).

%% @doc Apply updates to session record
-spec apply_session_updates(#session{}, map()) -> #session{}.
apply_session_updates(Session, Updates) ->
    Now = erlang:system_time(millisecond),
    
    UpdatedSession = Session#session{last_activity = Now},
    
    maps:fold(fun
        (message_count, Count, S) -> S#session{message_count = Count};
        (error_count, Count, S) -> S#session{error_count = Count};
        (capabilities, Caps, S) -> S#session{capabilities = Caps};
        (_, _, S) -> S
    end, UpdatedSession, Updates).

%% @doc Cleanup expired sessions
-spec cleanup_expired_sessions(#state{}) -> #state{}.
cleanup_expired_sessions(State) ->
    Now = erlang:system_time(millisecond),
    SessionTimeout = maps:get(session_timeout, State#state.config),
    StreamTimeout = maps:get(stream_timeout, State#state.config, ?DEFAULT_STREAM_TIMEOUT),
    
    CleanedSessions = maps:fold(fun(SessionId, Session, Acc) ->
        SessionAge = Now - Session#session.last_activity,
        
        if
            SessionAge > SessionTimeout ->
                ?LOG_INFO("Session ~s expired (age: ~p ms)", [SessionId, SessionAge]),
                delete_session_storage(State#state.storage_module, SessionId),
                Acc;
            true ->
                % Clean expired streams within session
                CleanedStreams = maps:filter(fun(_, StreamState) ->
                    StreamAge = Now - StreamState#stream_state.last_activity,
                    StreamAge =< StreamTimeout
                end, Session#session.streams),
                
                UpdatedSession = Session#session{streams = CleanedStreams},
                maps:put(SessionId, UpdatedSession, Acc)
        end
    end, #{}, State#state.sessions),
    
    State#state{sessions = CleanedSessions}.

%% @doc Remove streams with dead connection processes
-spec remove_dead_streams(#state{}, pid()) -> #state{}.
remove_dead_streams(State, DeadPid) ->
    UpdatedSessions = maps:map(fun(_SessionId, Session) ->
        UpdatedStreams = maps:filter(fun(_, StreamState) ->
            StreamState#stream_state.connection_pid =/= DeadPid
        end, Session#session.streams),
        
        Session#session{streams = UpdatedStreams}
    end, State#state.sessions),
    
    State#state{sessions = UpdatedSessions}.