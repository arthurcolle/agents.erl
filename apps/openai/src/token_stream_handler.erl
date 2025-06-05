%% token_stream_handler.erl
%% Token stream handler that works with token_stream_fsm
%% Provides higher-level token processing capabilities

-module(token_stream_handler).
-behaviour(gen_server).

%% Public API
-export([
    start_link/1,
    start_link/2,
    stop/1,
    create_token_stream/2,
    create_token_stream/3,
    subscribe/2,
    unsubscribe/2,
    get_active_streams/1,
    get_stream_info/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% Internal exports
-export([
    format_status/2
]).

%% Records
-record(state, {
    streams = #{} :: #{term() => #{stream_pid => pid(), subscribers => [pid()]}},
    subscribers = #{} :: #{pid() => [term()]},
    options = #{} :: map(),
    statistics = #{} :: map()
}).

%% Token processing record
-record(token_event, {
    stream_id :: term(),
    tokens :: [binary()],
    timestamp :: non_neg_integer(),
    metadata = #{} :: map()
}).

%% Logging macros
-define(LOG_INFO(Msg), log_safe(info, Msg)).
-define(LOG_INFO(Msg, Args), log_safe(info, io_lib:format(Msg, Args))).
-define(LOG_ERROR(Msg), log_safe(error, Msg)).
-define(LOG_ERROR(Msg, Args), log_safe(error, io_lib:format(Msg, Args))).
-define(LOG_DEBUG(Msg), log_safe(debug, Msg)).
-define(LOG_DEBUG(Msg, Args), log_safe(debug, io_lib:format(Msg, Args))).

%% Public API

%% Start token stream handler with default options
start_link(Name) ->
    start_link(Name, #{}).

%% Start token stream handler with custom options
start_link(Name, Options) ->
    gen_server:start_link({local, Name}, ?MODULE, Options, []).

%% Stop the token stream handler
stop(HandlerRef) ->
    gen_server:stop(HandlerRef).

%% Create a new token stream
create_token_stream(HandlerRef, StreamId) ->
    create_token_stream(HandlerRef, StreamId, #{}).

create_token_stream(HandlerRef, StreamId, Options) ->
    gen_server:call(HandlerRef, {create_stream, StreamId, Options}).

%% Subscribe to token events from a stream
subscribe(HandlerRef, StreamId) ->
    gen_server:call(HandlerRef, {subscribe, StreamId, self()}).

%% Unsubscribe from token events
unsubscribe(HandlerRef, StreamId) ->
    gen_server:call(HandlerRef, {unsubscribe, StreamId, self()}).

%% Get list of active streams
get_active_streams(HandlerRef) ->
    gen_server:call(HandlerRef, get_active_streams).

%% Get information about a specific stream
get_stream_info(HandlerRef, StreamId) ->
    gen_server:call(HandlerRef, {get_stream_info, StreamId}).

%% gen_server callbacks

%% Initialize the handler
init(Options) ->
    ?LOG_INFO("[TOKEN_HANDLER] 🚀 Initializing token stream handler"),
    
    % Monitor process exits to clean up subscriptions
    process_flag(trap_exit, true),
    
    State = #state{
        options = Options,
        statistics = init_statistics()
    },
    
    ?LOG_INFO("[TOKEN_HANDLER] ✅ Token stream handler initialized"),
    {ok, State}.

%% Handle synchronous calls
handle_call({create_stream, StreamId, Options}, _From, State) ->
    case maps:is_key(StreamId, State#state.streams) of
        true ->
            ?LOG_ERROR("[TOKEN_HANDLER] ❌ Stream already exists: ~p", [StreamId]),
            {reply, {error, stream_exists}, State};
        false ->
            case create_stream_process(StreamId, Options) of
                {ok, StreamPid} ->
                    ?LOG_INFO("[TOKEN_HANDLER] ✅ Created stream: ~p (PID: ~p)", [StreamId, StreamPid]),
                    
                    % Monitor the stream process
                    monitor(process, StreamPid),
                    
                    % Add to streams map
                    StreamInfo = #{
                        stream_pid => StreamPid,
                        subscribers => [],
                        created_at => erlang:system_time(millisecond),
                        options => Options
                    },
                    NewStreams = maps:put(StreamId, StreamInfo, State#state.streams),
                    NewState = State#state{streams = NewStreams},
                    
                    {reply, {ok, StreamPid}, update_statistics(NewState, streams_created, 1)};
                {error, Reason} ->
                    ?LOG_ERROR("[TOKEN_HANDLER] ❌ Failed to create stream ~p: ~p", [StreamId, Reason]),
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({subscribe, StreamId, SubscriberPid}, _From, State) ->
    case maps:get(StreamId, State#state.streams, undefined) of
        undefined ->
            {reply, {error, stream_not_found}, State};
        StreamInfo ->
            % Monitor the subscriber
            monitor(process, SubscriberPid),
            
            % Add subscriber to stream
            Subscribers = maps:get(subscribers, StreamInfo, []),
            NewSubscribers = [SubscriberPid | lists:delete(SubscriberPid, Subscribers)],
            NewStreamInfo = StreamInfo#{subscribers => NewSubscribers},
            NewStreams = maps:put(StreamId, NewStreamInfo, State#state.streams),
            
            % Update subscriber map
            SubStreams = maps:get(SubscriberPid, State#state.subscribers, []),
            NewSubStreams = [StreamId | lists:delete(StreamId, SubStreams)],
            NewSubscribers = maps:put(SubscriberPid, NewSubStreams, State#state.subscribers),
            
            NewState = State#state{
                streams = NewStreams,
                subscribers = NewSubscribers
            },
            
            ?LOG_INFO("[TOKEN_HANDLER] 📡 Subscribed ~p to stream ~p", [SubscriberPid, StreamId]),
            {reply, ok, update_statistics(NewState, subscriptions, 1)}
    end;

handle_call({unsubscribe, StreamId, SubscriberPid}, _From, State) ->
    case maps:get(StreamId, State#state.streams, undefined) of
        undefined ->
            {reply, {error, stream_not_found}, State};
        StreamInfo ->
            % Remove subscriber from stream
            Subscribers = maps:get(subscribers, StreamInfo, []),
            NewSubscribers = lists:delete(SubscriberPid, Subscribers),
            NewStreamInfo = StreamInfo#{subscribers => NewSubscribers},
            NewStreams = maps:put(StreamId, NewStreamInfo, State#state.streams),
            
            % Update subscriber map
            SubStreams = maps:get(SubscriberPid, State#state.subscribers, []),
            NewSubStreams = lists:delete(StreamId, SubStreams),
            NewSubscribersMap = case NewSubStreams of
                [] -> maps:remove(SubscriberPid, State#state.subscribers);
                _ -> maps:put(SubscriberPid, NewSubStreams, State#state.subscribers)
            end,
            
            NewState = State#state{
                streams = NewStreams,
                subscribers = NewSubscribersMap
            },
            
            ?LOG_INFO("[TOKEN_HANDLER] 📡 Unsubscribed ~p from stream ~p", [SubscriberPid, StreamId]),
            {reply, ok, NewState}
    end;

handle_call(get_active_streams, _From, State) ->
    StreamList = maps:fold(fun(StreamId, StreamInfo, Acc) ->
        Info = #{
            stream_id => StreamId,
            stream_pid => maps:get(stream_pid, StreamInfo),
            subscribers => length(maps:get(subscribers, StreamInfo, [])),
            created_at => maps:get(created_at, StreamInfo, 0),
            options => maps:get(options, StreamInfo, #{})
        },
        [Info | Acc]
    end, [], State#state.streams),
    {reply, {ok, StreamList}, State};

handle_call({get_stream_info, StreamId}, _From, State) ->
    case maps:get(StreamId, State#state.streams, undefined) of
        undefined ->
            {reply, {error, stream_not_found}, State};
        StreamInfo ->
            % Get statistics from the stream FSM
            StreamPid = maps:get(stream_pid, StreamInfo),
            StreamStats = case token_stream_fsm:get_statistics(StreamPid) of
                {ok, Stats} -> Stats;
                _ -> #{}
            end,
            
            Info = #{
                stream_id => StreamId,
                stream_pid => StreamPid,
                subscribers => maps:get(subscribers, StreamInfo, []),
                created_at => maps:get(created_at, StreamInfo, 0),
                options => maps:get(options, StreamInfo, #{}),
                statistics => StreamStats
            },
            {reply, {ok, Info}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% Handle asynchronous casts
handle_cast(_Msg, State) ->
    {noreply, State}.

%% Handle info messages
handle_info({token_batch, Tokens}, State) ->
    % This is called by token_stream_fsm when tokens are flushed
    ?LOG_DEBUG("[TOKEN_HANDLER] 📥 Received token batch: ~p tokens", [length(Tokens)]),
    
    % Find which stream this came from (we need to track this better)
    % For now, broadcast to all subscribers
    broadcast_tokens(Tokens, State),
    
    {noreply, update_statistics(State, tokens_processed, length(Tokens))};

handle_info({'DOWN', _Ref, process, Pid, Reason}, State) ->
    case find_stream_by_pid(Pid, State#state.streams) of
        {ok, StreamId} ->
            ?LOG_INFO("[TOKEN_HANDLER] 🛑 Stream ~p (PID: ~p) terminated: ~p", [StreamId, Pid, Reason]),
            NewStreams = maps:remove(StreamId, State#state.streams),
            NewState = State#state{streams = NewStreams},
            {noreply, update_statistics(NewState, streams_terminated, 1)};
        not_found ->
            % Check if it's a subscriber
            case maps:get(Pid, State#state.subscribers, undefined) of
                undefined ->
                    ?LOG_DEBUG("[TOKEN_HANDLER] 🛑 Unknown process terminated: ~p", [Pid]),
                    {noreply, State};
                StreamIds ->
                    ?LOG_INFO("[TOKEN_HANDLER] 🛑 Subscriber ~p terminated, cleaning up ~p streams", 
                             [Pid, length(StreamIds)]),
                    NewState = cleanup_subscriber(Pid, StreamIds, State),
                    {noreply, NewState}
            end
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%% Terminate callback
terminate(Reason, State) ->
    ?LOG_INFO("[TOKEN_HANDLER] 🛑 Terminating token stream handler: ~p", [Reason]),
    
    % Stop all active streams
    maps:fold(fun(_StreamId, StreamInfo, _Acc) ->
        StreamPid = maps:get(stream_pid, StreamInfo),
        token_stream_fsm:stop(StreamPid)
    end, ok, State#state.streams),
    
    ?LOG_INFO("[TOKEN_HANDLER] 📊 Final statistics: ~p", [State#state.statistics]),
    ok.

%% Code change callback
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Format status for debugging
format_status(Opt, [_PDict, State]) ->
    FilteredState = State#state{
        streams = maps:size(State#state.streams),
        subscribers = maps:size(State#state.subscribers)
    },
    case Opt of
        terminate ->
            FilteredState;
        normal ->
            [{data, [{"State", FilteredState}]}]
    end.

%% Internal helper functions

%% Create a stream process
create_stream_process(StreamId, Options) ->
    % Create a token stream FSM with this handler as the callback
    case token_stream_fsm:start_link(self(), Options) of
        {ok, StreamPid} ->
            ?LOG_DEBUG("[TOKEN_HANDLER] ✅ Created stream FSM for ~p", [StreamId]),
            {ok, StreamPid};
        {error, Reason} ->
            {error, Reason}
    end.

%% Find stream by PID
find_stream_by_pid(Pid, Streams) ->
    case maps:fold(fun(StreamId, StreamInfo, Acc) ->
        case maps:get(stream_pid, StreamInfo) of
            Pid -> {found, StreamId};
            _ -> Acc
        end
    end, not_found, Streams) of
        {found, StreamId} -> {ok, StreamId};
        not_found -> not_found
    end.

%% Cleanup subscriber from all streams
cleanup_subscriber(SubscriberPid, StreamIds, State) ->
    % Remove subscriber from all their streams
    NewStreams = lists:foldl(fun(StreamId, StreamsAcc) ->
        case maps:get(StreamId, StreamsAcc, undefined) of
            undefined ->
                StreamsAcc;
            StreamInfo ->
                Subscribers = maps:get(subscribers, StreamInfo, []),
                NewSubscribers = lists:delete(SubscriberPid, Subscribers),
                NewStreamInfo = StreamInfo#{subscribers => NewSubscribers},
                maps:put(StreamId, NewStreamInfo, StreamsAcc)
        end
    end, State#state.streams, StreamIds),
    
    % Remove subscriber from subscribers map
    NewSubscribers = maps:remove(SubscriberPid, State#state.subscribers),
    
    State#state{
        streams = NewStreams,
        subscribers = NewSubscribers
    }.

%% Broadcast tokens to subscribers
broadcast_tokens(Tokens, State) ->
    TokenEvent = #token_event{
        stream_id = unknown, % We need to improve this
        tokens = Tokens,
        timestamp = erlang:system_time(millisecond),
        metadata = #{}
    },
    
    % Broadcast to all subscribers of all streams (simplified for now)
    AllSubscribers = maps:fold(fun(_StreamId, StreamInfo, Acc) ->
        Subscribers = maps:get(subscribers, StreamInfo, []),
        Subscribers ++ Acc
    end, [], State#state.streams),
    
    UniqueSubscribers = lists:usort(AllSubscribers),
    
    lists:foreach(fun(SubscriberPid) ->
        try
            SubscriberPid ! {token_stream_event, TokenEvent}
        catch
            _:_ ->
                ?LOG_DEBUG("[TOKEN_HANDLER] ⚠️  Failed to send event to subscriber: ~p", [SubscriberPid])
        end
    end, UniqueSubscribers).

%% Initialize statistics
init_statistics() ->
    #{
        streams_created => 0,
        streams_terminated => 0,
        tokens_processed => 0,
        subscriptions => 0,
        start_time => erlang:system_time(millisecond)
    }.

%% Update statistics
update_statistics(State, Type, Value) ->
    Stats = State#state.statistics,
    NewStats = case Type of
        streams_created ->
            Stats#{streams_created => maps:get(streams_created, Stats, 0) + Value};
        streams_terminated ->
            Stats#{streams_terminated => maps:get(streams_terminated, Stats, 0) + Value};
        tokens_processed ->
            Stats#{tokens_processed => maps:get(tokens_processed, Stats, 0) + Value};
        subscriptions ->
            Stats#{subscriptions => maps:get(subscriptions, Stats, 0) + Value};
        _ ->
            Stats
    end,
    State#state{statistics = NewStats}.

%% Safe logging function with fallback
log_safe(Level, Msg) ->
    try
        case Level of
            info -> colored_logger:data(processed, Msg);
            error -> colored_logger:fire(inferno, Msg);
            debug -> colored_logger:debug(general, Msg);
            _ -> colored_logger:info(general, Msg)
        end
    catch
        _:_ ->
            % Fallback to standard logging
            Prefix = case Level of
                error -> "[ERROR] ";
                debug -> "[DEBUG] ";
                _ -> "[INFO] "
            end,
            io:format("~s~s~n", [Prefix, Msg])
    end.