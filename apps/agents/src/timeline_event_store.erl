-module(timeline_event_store).
-behaviour(gen_server).

%% API
-export([start_link/0,
         store/1,
         store_sync/1,
         retrieve/2,
         retrieve_range/3,
         retrieve_by_agent/2,
         retrieve_by_type/2,
         stream_events/2,
         unsubscribe/1,
         get_event_count/0,
         clear_old_events/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TABLE, timeline_events).
-define(MAX_EVENTS, 1000000). % Keep last 1M events
-define(CLEANUP_INTERVAL, 3600000). % Cleanup every hour
-define(MAX_PAGE_SIZE, 1000).
-define(CALL_TIMEOUT, 5000).
-define(SUBSCRIBER_TABLE, timeline_subscribers).

-record(timeline_event, {
    id :: binary(),
    timestamp :: erlang:timestamp(),
    type :: atom(),
    agent_id :: binary() | undefined,
    data :: map(),
    indexed_fields :: map()
}).

-record(state, {
    event_count :: non_neg_integer(),
    cleanup_timer :: reference()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Store a new timeline event
store(Event) when is_map(Event) ->
    gen_server:cast(?SERVER, {store, Event}).

%% Store an event and wait until it has been durably appended.  Internal,
%% high-volume producers can continue to use store/1; request/response paths
%% should use this API when they must not report success before persistence.
store_sync(Event) when is_map(Event) ->
    gen_server:call(?SERVER, {store_sync, Event}, ?CALL_TIMEOUT);
store_sync(_) ->
    {error, invalid_event}.

%% Retrieve events with pagination
retrieve(Limit, Offset) ->
    gen_server:call(?SERVER, {retrieve, Limit, Offset}, ?CALL_TIMEOUT).

%% Retrieve events within a time range
retrieve_range(StartTime, EndTime, Options) ->
    gen_server:call(?SERVER, {retrieve_range, StartTime, EndTime, Options}, ?CALL_TIMEOUT).

%% Retrieve events for a specific agent
retrieve_by_agent(AgentId, Options) ->
    gen_server:call(?SERVER, {retrieve_by_agent, AgentId, Options}, ?CALL_TIMEOUT).

%% Retrieve events by type
retrieve_by_type(Type, Options) ->
    gen_server:call(?SERVER, {retrieve_by_type, Type, Options}, ?CALL_TIMEOUT).

%% Stream events in real-time
stream_events(FilterFun, Pid) ->
    gen_server:call(?SERVER, {stream_events, FilterFun, Pid}, ?CALL_TIMEOUT).

unsubscribe(Pid) when is_pid(Pid) ->
    gen_server:call(?SERVER, {unsubscribe, Pid}, ?CALL_TIMEOUT);
unsubscribe(_) ->
    {error, invalid_subscriber}.

%% Get total event count
get_event_count() ->
    gen_server:call(?SERVER, get_event_count, ?CALL_TIMEOUT).

%% Clear events older than specified time
clear_old_events(Age) ->
    gen_server:call(?SERVER, {clear_old_events, Age}, ?CALL_TIMEOUT).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% Create ETS table for timeline events
    ensure_table(?TABLE, [
        ordered_set,
        public,
        {keypos, #timeline_event.id},
        {read_concurrency, true},
        {write_concurrency, true}
    ]),
    
    %% Create indices
    ensure_table(timeline_by_timestamp, [
        ordered_set,
        public,
        {read_concurrency, true}
    ]),
    
    ensure_table(timeline_by_agent, [
        bag,
        public,
        {read_concurrency, true}
    ]),
    
    ensure_table(timeline_by_type, [
        bag,
        public,
        {read_concurrency, true}
    ]),

    %% Subscriber state belongs to this process and is removed with it.  ETS
    %% avoids putting an unbounded subscriber list into the gen_server state.
    ensure_table(?SUBSCRIBER_TABLE, [
        set,
        protected,
        {keypos, 1}
    ]),
    
    %% Start cleanup timer
    TimerRef = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup),
    
    %% Restore persisted events directly into memory. Replay must not travel
    %% through store/1 because that path appends new events to the same file.
    LoadedEventCount = load_persisted_events(),
    
    {ok, #state{
        event_count = LoadedEventCount,
        cleanup_timer = TimerRef
    }}.

handle_call({retrieve, Limit, Offset}, _From, State) ->
    case valid_page(Limit, Offset) of
        true ->
            Events = retrieve_events(Limit, Offset),
            {reply, {ok, Events}, State};
        false ->
            {reply, {error, invalid_pagination}, State}
    end;

handle_call({retrieve_range, StartTime, EndTime, Options}, _From, State) ->
    case valid_range(StartTime, EndTime, Options) of
        true ->
            Events = retrieve_events_by_range(StartTime, EndTime, Options),
            {reply, {ok, Events}, State};
        false ->
            {reply, {error, invalid_range}, State}
    end;

handle_call({retrieve_by_agent, AgentId, Options}, _From, State) ->
    case valid_options(Options) of
        true ->
            Events = retrieve_events_by_agent(AgentId, Options),
            {reply, {ok, Events}, State};
        false ->
            {reply, {error, invalid_options}, State}
    end;

handle_call({retrieve_by_type, Type, Options}, _From, State) ->
    case valid_options(Options) of
        true ->
            Events = retrieve_events_by_type(Type, Options),
            {reply, {ok, Events}, State};
        false ->
            {reply, {error, invalid_options}, State}
    end;

handle_call({store_sync, Event}, _From, State) when is_map(Event) ->
    case store_event(Event, State) of
        {{ok, EventId}, NewState} -> {reply, {ok, EventId}, NewState};
        {{error, Reason}, NewState} -> {reply, {error, Reason}, NewState}
    end;

handle_call({stream_events, FilterFun, Pid}, _From, State)
        when is_function(FilterFun, 1), is_pid(Pid) ->
    case is_process_alive(Pid) of
        false ->
            {reply, {error, subscriber_not_alive}, State};
        true ->
            Existing = ets:lookup(?SUBSCRIBER_TABLE, Pid),
            SubscriberRef = case Existing of
                [{Pid, _OldFilter, Ref}] when is_reference(Ref) ->
                    erlang:demonitor(Ref, [flush]),
                    erlang:monitor(process, Pid);
                _ ->
                    erlang:monitor(process, Pid)
            end,
            ets:insert(?SUBSCRIBER_TABLE, {Pid, FilterFun, SubscriberRef}),
            {reply, ok, State}
    end;

handle_call({stream_events, _FilterFun, _Pid}, _From, State) ->
    {reply, {error, invalid_subscriber}, State};

handle_call({unsubscribe, Pid}, _From, State) when is_pid(Pid) ->
    remove_subscriber(Pid),
    {reply, ok, State};

handle_call(get_event_count, _From, State) ->
    {reply, State#state.event_count, State};

handle_call({clear_old_events, Age}, _From, State) when is_integer(Age), Age >= 0 ->
    DeletedCount = clear_old_events_internal(Age),
    NewCount = max(0, State#state.event_count - DeletedCount),
    {reply, {ok, DeletedCount}, State#state{event_count = NewCount}};

handle_call({clear_old_events, _Age}, _From, State) ->
    {reply, {error, invalid_age}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({store, Event}, State) when is_map(Event) ->
    case store_event(Event, State) of
        {{ok, _EventId}, NewState} -> {noreply, NewState};
        {{error, Reason}, NewState} ->
            logger:error("Timeline event was not stored: ~p", [Reason]),
            {noreply, NewState}
    end;

handle_cast({store, _Event}, State) ->
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup, State) ->
    %% Clean up old events if we exceed max count
    if
        State#state.event_count > ?MAX_EVENTS ->
            Deleted = cleanup_old_events(),
            NewCount = max(0, State#state.event_count - Deleted),
            State1 = State#state{event_count = NewCount},
            erlang:cancel_timer(State#state.cleanup_timer),
            NewTimerRef = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup),
            {noreply, State1#state{cleanup_timer = NewTimerRef}};
        true ->
            erlang:cancel_timer(State#state.cleanup_timer),
            NewTimerRef = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup),
            {noreply, State#state{cleanup_timer = NewTimerRef}}
    end;

handle_info({'DOWN', MonitorRef, process, Pid, _Reason}, State) ->
    %% Subscriber monitors are owned by this server.  Remove only the exact
    %% registration that produced the DOWN message; a process may have
    %% unsubscribed and subscribed again before an old notification arrives.
    case ets:lookup(?SUBSCRIBER_TABLE, Pid) of
        [{Pid, _FilterFun, MonitorRef}] ->
            ets:delete(?SUBSCRIBER_TABLE, Pid);
        _ ->
            ok
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    erlang:cancel_timer(State#state.cleanup_timer),
    lists:foreach(
        fun({Pid, _FilterFun, Ref}) ->
            erlang:demonitor(Ref, [flush]),
            Pid ! {timeline_subscription, closed}
        end,
        ets:tab2list(?SUBSCRIBER_TABLE)
    ),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

store_event(Event, State) ->
    %% Build and persist the event before publishing it through ETS or to
    %% subscribers.  A synchronous caller therefore never receives success
    %% for an event that was only held in memory.
    EventId = normalize_event_id(map_value(Event, id, <<"id">>, generate_event_id())),
    Timestamp = erlang:timestamp(),
    TimelineEvent = #timeline_event{
        id = EventId,
        timestamp = Timestamp,
        type = maps:get(type, Event, unknown),
        agent_id = maps:get(agent_id, Event, undefined),
        data = Event,
        indexed_fields = extract_indexed_fields(Event)
    },
    case persist_event(TimelineEvent) of
        ok ->
            Existing = ets:lookup(?TABLE, EventId),
            lists:foreach(fun remove_indices/1, Existing),
            ets:insert(?TABLE, TimelineEvent),
            update_indices(TimelineEvent),
            notify_stream_clients(TimelineEvent),
            EventCount = case Existing of
                [] -> State#state.event_count + 1;
                _ -> State#state.event_count
            end,
            {{ok, EventId}, State#state{event_count = EventCount}};
        {error, Reason} ->
            {{error, {persistence_failed, Reason}}, State}
    end.

normalize_event_id(Id) when is_binary(Id), byte_size(Id) > 0 -> Id;
normalize_event_id(Id) when is_list(Id) -> list_to_binary(Id);
normalize_event_id(Id) when is_atom(Id) -> atom_to_binary(Id, utf8);
normalize_event_id(_) -> generate_event_id().

ensure_table(Name, Options) ->
    case ets:info(Name) of
        undefined -> ets:new(Name, [named_table | Options]);
        _ -> Name
    end.

valid_page(Limit, Offset) ->
    is_integer(Limit) andalso Limit >= 0 andalso Limit =< ?MAX_PAGE_SIZE andalso
    is_integer(Offset) andalso Offset >= 0.

valid_options(Options) when is_map(Options) ->
    valid_limit(maps:get(limit, Options, 100));
valid_options(_) -> false.

valid_limit(Limit) -> is_integer(Limit) andalso Limit >= 0 andalso Limit =< ?MAX_PAGE_SIZE.

valid_range(StartTime, EndTime, Options) ->
    valid_timestamp(StartTime) andalso valid_timestamp(EndTime) andalso
    timestamp_greater_or_equal(EndTime, StartTime) andalso valid_options(Options).

valid_timestamp({Mega, Sec, Micro}) ->
    is_integer(Mega) andalso is_integer(Sec) andalso is_integer(Micro) andalso
    Mega >= 0 andalso Sec >= 0 andalso Micro >= 0;
valid_timestamp(_) -> false.

timestamp_greater_or_equal(A, B) -> A =:= B orelse timestamp_greater_than(A, B).

drop_events(0, Events) -> Events;
drop_events(_N, []) -> [];
drop_events(N, [_ | Rest]) when N > 0 -> drop_events(N - 1, Rest).

paginate_events(Events, Limit, Offset) ->
    lists:sublist(drop_events(Offset, Events), Limit).

sort_event_ids(EventIds) ->
    Pairs = lists:foldl(fun(EventId, Acc) ->
        case ets:lookup(?TABLE, EventId) of
            [Event] -> [{Event#timeline_event.timestamp, EventId} | Acc];
            [] -> Acc
        end
    end, [], lists:usort(EventIds)),
    [EventId || {_Timestamp, EventId} <-
        lists:sort(fun({TimestampA, IdA}, {TimestampB, IdB}) ->
            {TimestampA, IdA} > {TimestampB, IdB}
        end, Pairs)].

remove_subscriber(Pid) ->
    case ets:take(?SUBSCRIBER_TABLE, Pid) of
        [{Pid, _FilterFun, Ref}] when is_reference(Ref) ->
            erlang:demonitor(Ref, [flush]),
            ok;
        _ ->
            ok
    end.

generate_event_id() ->
    %% The event store is persisted as JSON and exposed through the web API.
    %% Raw UUID bytes are not valid UTF-8, so retain the UUID's canonical text
    %% representation at this boundary.
    list_to_binary(agent_uuid:to_string(agent_uuid:generate())).

extract_indexed_fields(Event) ->
    %% Extract fields that should be indexed for fast retrieval
    #{
        task_id => map_value(Event, task_id, <<"task_id">>, undefined),
        conversation_id => map_value(
            Event,
            conversation_id,
            <<"conversation_id">>,
            undefined
        ),
        tool_name => map_value(Event, tool_name, <<"tool_name">>, undefined),
        error_type => map_value(Event, error_type, <<"error_type">>, undefined)
    }.

update_indices(#timeline_event{} = Event) ->
    %% Update timestamp index
    ets:insert(timeline_by_timestamp, {{Event#timeline_event.timestamp, Event#timeline_event.id}}),
    
    %% Update agent index
    case Event#timeline_event.agent_id of
        undefined -> ok;
        AgentId -> ets:insert(timeline_by_agent, {AgentId, Event#timeline_event.id})
    end,
    
    %% Update type index
    ets:insert(timeline_by_type, {Event#timeline_event.type, Event#timeline_event.id}).

remove_indices(#timeline_event{} = Event) ->
    ets:delete(timeline_by_timestamp, {Event#timeline_event.timestamp, Event#timeline_event.id}),
    case Event#timeline_event.agent_id of
        undefined -> ok;
        AgentId -> ets:delete_object(timeline_by_agent, {AgentId, Event#timeline_event.id})
    end,
    ets:delete_object(timeline_by_type, {Event#timeline_event.type, Event#timeline_event.id}).

retrieve_events(Limit, Offset) ->
    %% Get events ordered by timestamp (newest first)
    AllEvents = ets:select(timeline_by_timestamp, [{{'$1'}, [], ['$1']}]),
    SortedEvents = lists:reverse(AllEvents),
    
    %% Apply pagination
    EventIds = paginate_events([Id || {_Timestamp, Id} <- SortedEvents], Limit, Offset),
    
    %% Fetch full events
    [begin
        case ets:lookup(?TABLE, EventId) of
            [Event] -> event_to_map(Event);
            [] -> undefined
        end
     end || EventId <- EventIds, EventId =/= undefined].

retrieve_events_by_range(StartTime, EndTime, Options) ->
    Limit = maps:get(limit, Options, 100),
    
    %% Find events within time range
    MatchingEvents = ets:select(timeline_by_timestamp, [
        {{{{'$1', '$2', '$3'}, '$4'}}, 
         [{'>=', {'$1', '$2', '$3'}, StartTime}, 
          {'=<', {'$1', '$2', '$3'}, EndTime}], 
         ['$4']}
    ]),
    
    %% Apply limit and fetch full events
    EventIds = lists:sublist(MatchingEvents, Limit),
    [begin
        case ets:lookup(?TABLE, EventId) of
            [Event] -> event_to_map(Event);
            [] -> undefined
        end
     end || EventId <- EventIds, EventId =/= undefined].

retrieve_events_by_agent(AgentId, Options) ->
    Limit = maps:get(limit, Options, 100),
    
    %% Get event IDs for agent
    EventIds = case ets:lookup(timeline_by_agent, AgentId) of
        [] -> [];
        Results -> [EventId || {_AgentId, EventId} <- Results]
    end,
    
    %% Sort by a strict timestamp/id key and apply the limit.
    Events = lists:sublist(sort_event_ids(EventIds), Limit),
    
    %% Fetch full events
    [begin
        case ets:lookup(?TABLE, EventId) of
            [Event] -> event_to_map(Event);
            [] -> undefined
        end
     end || EventId <- Events, EventId =/= undefined].

retrieve_events_by_type(Type, Options) ->
    Limit = maps:get(limit, Options, 100),
    
    %% Get event IDs for type
    EventIds = case ets:lookup(timeline_by_type, Type) of
        [] -> [];
        Results -> [EventId || {_Type, EventId} <- Results]
    end,
    
    %% Sort by a strict timestamp/id key and apply the limit.
    Events = lists:sublist(sort_event_ids(EventIds), Limit),
    
    %% Fetch full events
    [begin
        case ets:lookup(?TABLE, EventId) of
            [Event] -> event_to_map(Event);
            [] -> undefined
        end
     end || EventId <- Events, EventId =/= undefined].

event_to_map(#timeline_event{} = Event) ->
    #{
        id => Event#timeline_event.id,
        timestamp => Event#timeline_event.timestamp,
        type => Event#timeline_event.type,
        agent_id => Event#timeline_event.agent_id,
        data => Event#timeline_event.data
    }.

notify_stream_clients(Event) ->
    %% Notify all registered streaming clients.  Filters are extension points
    %% supplied by websocket processes, so a bad filter must not take down the
    %% durable event store.
    Subscribers = ets:tab2list(?SUBSCRIBER_TABLE),
    lists:foreach(
        fun({Pid, FilterFun, _MonitorRef}) ->
            case safe_filter(FilterFun, Event) of
                true -> Pid ! {timeline_event, event_to_map(Event)};
                false -> ok;
                _ -> ok
            end
        end,
        Subscribers
    ).

safe_filter(FilterFun, Event) ->
    try FilterFun(Event) catch _:_ -> false end.

persist_event(Event) ->
    %% Persist to disk (using NDJSON format)
    Path = timeline_event_path(),
    case filelib:ensure_dir(Path) of
        ok -> persist_event_to_path(Event, Path);
        {error, Reason} -> {error, {ensure_dir_failed, Reason}}
    end.

persist_event_to_path(Event, Path) ->
    case file:open(Path, [append, binary, {encoding, utf8}]) of
        {ok, File} ->
            Result = case file:write(File, [jsx:encode(persisted_event_to_map(Event)), <<"\n">>]) of
                ok -> file:sync(File);
                {error, WriteReason} -> {error, {write_failed, WriteReason}}
            end,
            CloseResult = file:close(File),
            case {Result, CloseResult} of
                {ok, ok} -> ok;
                {{error, _} = Error, _} -> Error;
                {ok, {error, CloseReason}} -> {error, {close_failed, CloseReason}}
            end;
        {error, Reason} ->
            {error, {open_failed, Reason}}
    end.

load_persisted_events() ->
    %% Stream the file instead of loading the entire history into one binary.
    %% This is important for long-running installations with large timelines.
    case file:open(timeline_event_path(), [read, binary, {read_ahead, 65536}]) of
        {ok, File} ->
            try load_persisted_lines(File)
            after
                file:close(File)
            end;
        {error, enoent} ->
            0;
        {error, _Reason} ->
            0
    end.

load_persisted_lines(File) ->
    case io:get_line(File, "") of
        eof ->
            ets:info(?TABLE, size);
        {error, _Reason} ->
            ets:info(?TABLE, size);
        Line when is_binary(Line) ->
            load_persisted_line(Line),
            load_persisted_lines(File)
    end.

load_persisted_line(<<"\n">>) ->
    ok;
load_persisted_line(<<"\r\n">>) ->
    ok;
load_persisted_line(Line) ->
    try
        EventMap = jsx:decode(Line, [return_maps]),
        Event = persisted_event_from_map(EventMap),
        insert_event_in_memory(Event)
    catch
        _:_ -> ok
    end.

insert_event_in_memory(#timeline_event{id = EventId} = Event) ->
    Existing = ets:lookup(?TABLE, EventId),
    lists:foreach(fun remove_indices/1, Existing),
    ets:insert(?TABLE, Event),
    update_indices(Event),
    ok.

persisted_event_from_map(EventMap0) ->
    EventMap = unwrap_replayed_event(EventMap0),
    Id = map_value(EventMap, id, <<"id">>, generate_event_id()),
    Timestamp = normalize_persisted_timestamp(map_value(
        EventMap,
        timestamp,
        <<"timestamp">>,
        erlang:timestamp()
    )),
    Type = normalize_event_type(
        map_value(EventMap, type, <<"type">>, unknown)
    ),
    AgentId = map_value(
        EventMap,
        agent_id,
        <<"agent_id">>,
        map_value(EventMap, agentId, <<"agentId">>, undefined)
    ),
    Data = case maps:is_key(data, EventMap) orelse
                maps:is_key(<<"data">>, EventMap) of
        true -> map_value(EventMap, data, <<"data">>, EventMap);
        false -> EventMap
    end,
    #timeline_event{
        id = Id,
        timestamp = Timestamp,
        type = Type,
        agent_id = normalize_optional_value(AgentId),
        data = Data,
        indexed_fields = extract_indexed_fields(Data)
    }.

persisted_event_to_map(#timeline_event{} = Event) ->
    %% JSON has no tuple type.  Persist an explicit Unix-microsecond scalar so
    %% replay can reconstruct the exact Erlang timestamp used for indexing and
    %% the web view does not lose its fractional second.
    (event_to_map(Event))#{
        timestamp => timestamp_to_unix_us(Event#timeline_event.timestamp)
    }.

timestamp_to_unix_us({Mega, Sec, Micro}) ->
    (Mega * 1000000 + Sec) * 1000000 + Micro;
timestamp_to_unix_us(Value) ->
    Value.

normalize_persisted_timestamp({Mega, Sec, Micro}) ->
    {Mega, Sec, Micro};
normalize_persisted_timestamp(UnixMicroseconds) when is_integer(UnixMicroseconds) ->
    unix_us_to_timestamp(UnixMicroseconds);
normalize_persisted_timestamp(Rfc3339) when is_binary(Rfc3339) ->
    try unix_us_to_timestamp(calendar:rfc3339_to_system_time(
        binary_to_list(Rfc3339),
        [{unit, microsecond}]
    ))
    catch
        _:_ -> erlang:timestamp()
    end;
normalize_persisted_timestamp(Rfc3339) when is_list(Rfc3339) ->
    try unix_us_to_timestamp(calendar:rfc3339_to_system_time(
        Rfc3339,
        [{unit, microsecond}]
    ))
    catch
        _:_ -> erlang:timestamp()
    end;
normalize_persisted_timestamp(_) ->
    erlang:timestamp().

unix_us_to_timestamp(UnixMicroseconds) ->
    Mega = UnixMicroseconds div 1000000000000,
    Remaining = UnixMicroseconds rem 1000000000000,
    Sec = Remaining div 1000000,
    Micro = Remaining rem 1000000,
    {Mega, Sec, Micro}.

%% Older releases replayed persisted records through store/1. That generated a
%% new "unknown" envelope on every boot and appended it to the same file.
%% Unwrap only the exact five-field envelope emitted by event_to_map/1.
unwrap_replayed_event(EventMap) when is_map(EventMap) ->
    case {
        lists:sort(maps:keys(EventMap)),
        maps:get(<<"type">>, EventMap, undefined),
        maps:get(<<"agent_id">>, EventMap, undefined),
        maps:get(<<"data">>, EventMap, undefined)
    } of
        {
            [<<"agent_id">>, <<"data">>, <<"id">>, <<"timestamp">>, <<"type">>],
            <<"unknown">>,
            <<"undefined">>,
            Data
        } when is_map(Data) ->
            unwrap_replayed_event(Data);
        _ ->
            EventMap
    end.

normalize_event_type(Type) when is_atom(Type) ->
    Type;
normalize_event_type(<<"message">>) ->
    message;
normalize_event_type(<<"system">>) ->
    system;
normalize_event_type(<<"unknown">>) ->
    unknown;
normalize_event_type(Type) ->
    Type.

normalize_optional_value(null) -> undefined;
normalize_optional_value(<<"undefined">>) -> undefined;
normalize_optional_value(Value) -> Value.

map_value(Map, AtomKey, BinaryKey, Default) ->
    case maps:find(AtomKey, Map) of
        {ok, Value} -> Value;
        error -> maps:get(BinaryKey, Map, Default)
    end.

timeline_event_path() ->
    application:get_env(
        agents,
        timeline_event_path,
        "apps/agent_web/priv/data/timeline_events.ndjson"
    ).

cleanup_old_events() ->
    %% Remove oldest events if we exceed max count
    EventCount = ets:info(?TABLE, size),
    if
        EventCount > ?MAX_EVENTS ->
            ToDelete = EventCount - ?MAX_EVENTS,
            {OldestEvents, _Continuation} = ets:select(
                timeline_by_timestamp,
                [{{'$1'}, [], ['$1']}],
                ToDelete
            ),
            lists:foldl(
                fun({_Timestamp, EventId}, Acc) ->
                    case ets:lookup(?TABLE, EventId) of
                        [Event] ->
                            remove_indices(Event),
                            ets:delete(?TABLE, EventId),
                            Acc + 1;
                        [] ->
                            Acc
                    end
                end,
                0,
                OldestEvents
            );
        true ->
            0
    end.

clear_old_events_internal(Age) ->
    Now = erlang:timestamp(),
    CutoffTime = subtract_seconds_from_timestamp(Now, Age),
    
    %% Find old events
    OldEvents = ets:select(timeline_by_timestamp, [
        {{{{'$1', '$2', '$3'}, '$4'}}, 
         [{'<', {'$1', '$2', '$3'}, CutoffTime}], 
         ['$4']}
    ]),
    
    %% Delete them
    lists:foreach(
        fun(EventId) ->
            case ets:lookup(?TABLE, EventId) of
                [Event] ->
                    ets:delete(?TABLE, EventId),
                    ets:delete(timeline_by_timestamp, {Event#timeline_event.timestamp, EventId}),
                    case Event#timeline_event.agent_id of
                        undefined -> ok;
                        AgentId -> ets:delete_object(timeline_by_agent, {AgentId, EventId})
                    end,
                    ets:delete_object(timeline_by_type, {Event#timeline_event.type, EventId});
                [] ->
                    ok
            end
        end,
        OldEvents
    ),
    
    length(OldEvents).

timestamp_greater_than({M1, S1, U1}, {M2, S2, U2}) ->
    (M1 > M2) orelse 
    (M1 =:= M2 andalso S1 > S2) orelse
    (M1 =:= M2 andalso S1 =:= S2 andalso U1 > U2).

subtract_seconds_from_timestamp({Mega, Sec, Micro}, Seconds) ->
    TotalMicro = (Mega * 1000000 + Sec) * 1000000 + Micro - (Seconds * 1000000),
    NewMega = TotalMicro div 1000000000000,
    NewSec = (TotalMicro rem 1000000000000) div 1000000,
    NewMicro = TotalMicro rem 1000000,
    {NewMega, NewSec, NewMicro}.
