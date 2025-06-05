%% @doc MCP HTTP Message Buffer
%%
%% Handles message buffering, redelivery, and resumability for MCP Streamable HTTP
%% transport according to the 2025-03-26 specification.
-module(mcp_http_message_buffer).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

%% API exports
-export([
    start_link/1,
    buffer_message/4,
    get_messages_after/3,
    mark_delivered/3,
    cleanup_delivered/2,
    get_buffer_stats/2,
    clear_buffer/2
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
-type event_id() :: non_neg_integer().
-type message() :: map().
-type delivery_status() :: pending | delivered | acknowledged.

-record(buffered_message, {
    event_id :: event_id(),
    session_id :: session_id(),
    stream_id :: stream_id(),
    message :: message(),
    timestamp :: integer(),
    delivery_status = pending :: delivery_status(),
    retry_count = 0 :: non_neg_integer(),
    last_attempt :: integer() | undefined
}).

-record(stream_buffer, {
    session_id :: session_id(),
    stream_id :: stream_id(),
    next_event_id = 1 :: event_id(),
    messages = [] :: [#buffered_message{}],
    last_delivered_id = 0 :: event_id(),
    created_at :: integer(),
    last_activity :: integer()
}).

-record(state, {
    buffers = #{} :: #{{session_id(), stream_id()} => #stream_buffer{}},
    config :: map(),
    cleanup_timer :: reference(),
    delivery_timer :: reference()
}).

%% Constants
-define(MAX_BUFFER_SIZE, 10000).
-define(MAX_MESSAGE_AGE, 3600000).     % 1 hour
-define(CLEANUP_INTERVAL, 60000).      % 1 minute
-define(DELIVERY_RETRY_INTERVAL, 5000). % 5 seconds
-define(MAX_RETRY_COUNT, 5).

%%% ============================================================================
%%% API Functions
%%% ============================================================================

%% @doc Start the message buffer server
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%% @doc Buffer a message for delivery
-spec buffer_message(session_id(), stream_id(), message(), map()) -> 
    {ok, event_id()} | {error, term()}.
buffer_message(SessionId, StreamId, Message, Options) ->
    gen_server:call(?MODULE, {buffer_message, SessionId, StreamId, Message, Options}).

%% @doc Get messages after a specific event ID for resumption
-spec get_messages_after(session_id(), stream_id(), event_id()) -> 
    {ok, [#buffered_message{}]} | {error, term()}.
get_messages_after(SessionId, StreamId, LastEventId) ->
    gen_server:call(?MODULE, {get_messages_after, SessionId, StreamId, LastEventId}).

%% @doc Mark a message as delivered
-spec mark_delivered(session_id(), stream_id(), event_id()) -> ok | {error, term()}.
mark_delivered(SessionId, StreamId, EventId) ->
    gen_server:cast(?MODULE, {mark_delivered, SessionId, StreamId, EventId}).

%% @doc Cleanup delivered messages older than threshold
-spec cleanup_delivered(session_id(), stream_id()) -> ok.
cleanup_delivered(SessionId, StreamId) ->
    gen_server:cast(?MODULE, {cleanup_delivered, SessionId, StreamId}).

%% @doc Get buffer statistics
-spec get_buffer_stats(session_id(), stream_id()) -> 
    {ok, map()} | {error, not_found}.
get_buffer_stats(SessionId, StreamId) ->
    gen_server:call(?MODULE, {get_buffer_stats, SessionId, StreamId}).

%% @doc Clear entire buffer for a stream
-spec clear_buffer(session_id(), stream_id()) -> ok.
clear_buffer(SessionId, StreamId) ->
    gen_server:cast(?MODULE, {clear_buffer, SessionId, StreamId}).

%%% ============================================================================
%%% Gen Server Implementation
%%% ============================================================================

%% @doc Initialize message buffer
init(Config) ->
    State = #state{
        config = Config
    },
    
    % Start cleanup timer
    CleanupTimer = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup),
    
    % Start delivery retry timer
    DeliveryTimer = erlang:send_after(?DELIVERY_RETRY_INTERVAL, self(), retry_deliveries),
    
    {ok, State#state{
        cleanup_timer = CleanupTimer,
        delivery_timer = DeliveryTimer
    }}.

%% @doc Handle synchronous calls
handle_call({buffer_message, SessionId, StreamId, Message, Options}, _From, State) ->
    BufferKey = {SessionId, StreamId},
    
    case maps:get(BufferKey, State#state.buffers, undefined) of
        undefined ->
            % Create new buffer
            Buffer = create_new_buffer(SessionId, StreamId),
            {EventId, UpdatedBuffer} = add_message_to_buffer(Buffer, Message, Options),
            NewBuffers = maps:put(BufferKey, UpdatedBuffer, State#state.buffers),
            {reply, {ok, EventId}, State#state{buffers = NewBuffers}};
        Buffer ->
            case check_buffer_capacity(Buffer) of
                ok ->
                    {EventId, UpdatedBuffer} = add_message_to_buffer(Buffer, Message, Options),
                    NewBuffers = maps:put(BufferKey, UpdatedBuffer, State#state.buffers),
                    {reply, {ok, EventId}, State#state{buffers = NewBuffers}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({get_messages_after, SessionId, StreamId, LastEventId}, _From, State) ->
    BufferKey = {SessionId, StreamId},
    
    case maps:get(BufferKey, State#state.buffers, undefined) of
        undefined ->
            {reply, {ok, []}, State};
        Buffer ->
            Messages = get_messages_after_id(Buffer, LastEventId),
            {reply, {ok, Messages}, State}
    end;

handle_call({get_buffer_stats, SessionId, StreamId}, _From, State) ->
    BufferKey = {SessionId, StreamId},
    
    case maps:get(BufferKey, State#state.buffers, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Buffer ->
            Stats = calculate_buffer_stats(Buffer),
            {reply, {ok, Stats}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @doc Handle asynchronous casts
handle_cast({mark_delivered, SessionId, StreamId, EventId}, State) ->
    BufferKey = {SessionId, StreamId},
    
    case maps:get(BufferKey, State#state.buffers, undefined) of
        undefined ->
            {noreply, State};
        Buffer ->
            UpdatedBuffer = mark_message_delivered(Buffer, EventId),
            NewBuffers = maps:put(BufferKey, UpdatedBuffer, State#state.buffers),
            {noreply, State#state{buffers = NewBuffers}}
    end;

handle_cast({cleanup_delivered, SessionId, StreamId}, State) ->
    BufferKey = {SessionId, StreamId},
    
    case maps:get(BufferKey, State#state.buffers, undefined) of
        undefined ->
            {noreply, State};
        Buffer ->
            UpdatedBuffer = cleanup_delivered_messages(Buffer),
            NewBuffers = maps:put(BufferKey, UpdatedBuffer, State#state.buffers),
            {noreply, State#state{buffers = NewBuffers}}
    end;

handle_cast({clear_buffer, SessionId, StreamId}, State) ->
    BufferKey = {SessionId, StreamId},
    NewBuffers = maps:remove(BufferKey, State#state.buffers),
    ?LOG_INFO("Cleared buffer for session ~s, stream ~s", [SessionId, StreamId]),
    {noreply, State#state{buffers = NewBuffers}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle info messages
handle_info(cleanup, State) ->
    NewState = cleanup_expired_buffers(State),
    Timer = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup),
    {noreply, NewState#state{cleanup_timer = Timer}};

handle_info(retry_deliveries, State) ->
    NewState = retry_failed_deliveries(State),
    Timer = erlang:send_after(?DELIVERY_RETRY_INTERVAL, self(), retry_deliveries),
    {noreply, NewState#state{delivery_timer = Timer}};

handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Cleanup on termination
terminate(_Reason, State) ->
    cleanup_timers(State),
    ok.

%% @doc Handle code changes
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ============================================================================
%%% Buffer Management Functions
%%% ============================================================================

%% @doc Create a new stream buffer
-spec create_new_buffer(session_id(), stream_id()) -> #stream_buffer{}.
create_new_buffer(SessionId, StreamId) ->
    Now = erlang:system_time(millisecond),
    #stream_buffer{
        session_id = SessionId,
        stream_id = StreamId,
        created_at = Now,
        last_activity = Now
    }.

%% @doc Add message to buffer
-spec add_message_to_buffer(#stream_buffer{}, message(), map()) -> 
    {event_id(), #stream_buffer{}}.
add_message_to_buffer(Buffer, Message, Options) ->
    EventId = Buffer#stream_buffer.next_event_id,
    Now = erlang:system_time(millisecond),
    
    BufferedMessage = #buffered_message{
        event_id = EventId,
        session_id = Buffer#stream_buffer.session_id,
        stream_id = Buffer#stream_buffer.stream_id,
        message = Message,
        timestamp = Now,
        delivery_status = maps:get(delivery_status, Options, pending)
    },
    
    UpdatedMessages = Buffer#stream_buffer.messages ++ [BufferedMessage],
    
    UpdatedBuffer = Buffer#stream_buffer{
        next_event_id = EventId + 1,
        messages = UpdatedMessages,
        last_activity = Now
    },
    
    ?LOG_DEBUG("Buffered message ~p for session ~s, stream ~s", 
               [EventId, Buffer#stream_buffer.session_id, Buffer#stream_buffer.stream_id]),
    
    {EventId, UpdatedBuffer}.

%% @doc Check if buffer has capacity for new messages
-spec check_buffer_capacity(#stream_buffer{}) -> ok | {error, atom()}.
check_buffer_capacity(Buffer) ->
    MessageCount = length(Buffer#stream_buffer.messages),
    
    if
        MessageCount >= ?MAX_BUFFER_SIZE ->
            {error, buffer_full};
        true ->
            ok
    end.

%% @doc Get messages after a specific event ID
-spec get_messages_after_id(#stream_buffer{}, event_id()) -> [#buffered_message{}].
get_messages_after_id(Buffer, LastEventId) ->
    lists:filter(fun(Msg) ->
        Msg#buffered_message.event_id > LastEventId
    end, Buffer#stream_buffer.messages).

%% @doc Mark a message as delivered
-spec mark_message_delivered(#stream_buffer{}, event_id()) -> #stream_buffer{}.
mark_message_delivered(Buffer, EventId) ->
    Now = erlang:system_time(millisecond),
    
    UpdatedMessages = lists:map(fun(Msg) ->
        case Msg#buffered_message.event_id of
            EventId ->
                Msg#buffered_message{
                    delivery_status = delivered,
                    last_attempt = Now
                };
            _ ->
                Msg
        end
    end, Buffer#stream_buffer.messages),
    
    NewLastDelivered = max(Buffer#stream_buffer.last_delivered_id, EventId),
    
    Buffer#stream_buffer{
        messages = UpdatedMessages,
        last_delivered_id = NewLastDelivered,
        last_activity = Now
    }.

%% @doc Remove delivered messages older than threshold
-spec cleanup_delivered_messages(#stream_buffer{}) -> #stream_buffer{}.
cleanup_delivered_messages(Buffer) ->
    Now = erlang:system_time(millisecond),
    Threshold = Now - ?MAX_MESSAGE_AGE,
    
    RemainingMessages = lists:filter(fun(Msg) ->
        case Msg#buffered_message.delivery_status of
            delivered ->
                Msg#buffered_message.timestamp > Threshold;
            _ ->
                true
        end
    end, Buffer#stream_buffer.messages),
    
    Buffer#stream_buffer{messages = RemainingMessages}.

%% @doc Calculate buffer statistics
-spec calculate_buffer_stats(#stream_buffer{}) -> map().
calculate_buffer_stats(Buffer) ->
    Messages = Buffer#stream_buffer.messages,
    
    {Pending, Delivered, Failed} = lists:foldl(fun(Msg, {P, D, F}) ->
        case Msg#buffered_message.delivery_status of
            pending -> {P + 1, D, F};
            delivered -> {P, D + 1, F};
            _ -> {P, D, F + 1}
        end
    end, {0, 0, 0}, Messages),
    
    #{
        total_messages => length(Messages),
        pending_messages => Pending,
        delivered_messages => Delivered,
        failed_messages => Failed,
        next_event_id => Buffer#stream_buffer.next_event_id,
        last_delivered_id => Buffer#stream_buffer.last_delivered_id,
        created_at => Buffer#stream_buffer.created_at,
        last_activity => Buffer#stream_buffer.last_activity
    }.

%%% ============================================================================
%%% Cleanup and Maintenance Functions
%%% ============================================================================

%% @doc Cleanup expired buffers
-spec cleanup_expired_buffers(#state{}) -> #state{}.
cleanup_expired_buffers(State) ->
    Now = erlang:system_time(millisecond),
    MaxAge = maps:get(max_buffer_age, State#state.config, ?MAX_MESSAGE_AGE),
    
    ActiveBuffers = maps:filter(fun(_Key, Buffer) ->
        Age = Now - Buffer#stream_buffer.last_activity,
        Age =< MaxAge
    end, State#state.buffers),
    
    % Clean up messages within remaining buffers
    CleanedBuffers = maps:map(fun(_Key, Buffer) ->
        cleanup_delivered_messages(Buffer)
    end, ActiveBuffers),
    
    RemovedCount = maps:size(State#state.buffers) - maps:size(CleanedBuffers),
    
    if
        RemovedCount > 0 ->
            ?LOG_INFO("Cleaned up ~p expired buffers", [RemovedCount]);
        true ->
            ok
    end,
    
    State#state{buffers = CleanedBuffers}.

%% @doc Retry failed message deliveries
-spec retry_failed_deliveries(#state{}) -> #state{}.
retry_failed_deliveries(State) ->
    Now = erlang:system_time(millisecond),
    
    UpdatedBuffers = maps:map(fun(_Key, Buffer) ->
        retry_buffer_deliveries(Buffer, Now)
    end, State#state.buffers),
    
    State#state{buffers = UpdatedBuffers}.

%% @doc Retry deliveries for a specific buffer
-spec retry_buffer_deliveries(#stream_buffer{}, integer()) -> #stream_buffer{}.
retry_buffer_deliveries(Buffer, Now) ->
    UpdatedMessages = lists:map(fun(Msg) ->
        case should_retry_message(Msg, Now) of
            true ->
                ?LOG_DEBUG("Retrying delivery for message ~p", 
                          [Msg#buffered_message.event_id]),
                Msg#buffered_message{
                    retry_count = Msg#buffered_message.retry_count + 1,
                    last_attempt = Now
                };
            false ->
                Msg
        end
    end, Buffer#stream_buffer.messages),
    
    Buffer#stream_buffer{messages = UpdatedMessages}.

%% @doc Check if message should be retried
-spec should_retry_message(#buffered_message{}, integer()) -> boolean().
should_retry_message(Msg, Now) ->
    case Msg#buffered_message.delivery_status of
        pending ->
            case Msg#buffered_message.last_attempt of
                undefined ->
                    true; % First attempt
                LastAttempt ->
                    TimeSinceAttempt = Now - LastAttempt,
                    RetryInterval = calculate_retry_interval(Msg#buffered_message.retry_count),
                    TimeSinceAttempt >= RetryInterval andalso 
                    Msg#buffered_message.retry_count < ?MAX_RETRY_COUNT
            end;
        _ ->
            false
    end.

%% @doc Calculate retry interval with exponential backoff
-spec calculate_retry_interval(non_neg_integer()) -> integer().
calculate_retry_interval(RetryCount) ->
    BaseInterval = ?DELIVERY_RETRY_INTERVAL,
    BackoffFactor = math:pow(2, min(RetryCount, 5)),
    trunc(BaseInterval * BackoffFactor).

%% @doc Cleanup timers on shutdown
-spec cleanup_timers(#state{}) -> ok.
cleanup_timers(State) ->
    case State#state.cleanup_timer of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,
    
    case State#state.delivery_timer of
        undefined -> ok;
        Timer2 -> erlang:cancel_timer(Timer2)
    end,
    
    ok.