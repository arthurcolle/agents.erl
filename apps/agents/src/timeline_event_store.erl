-module(timeline_event_store).
-behaviour(gen_server).

%% API
-export([start_link/0,
         store/1,
         retrieve/2,
         retrieve_range/3,
         retrieve_by_agent/2,
         retrieve_by_type/2,
         stream_events/2,
         get_event_count/0,
         clear_old_events/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TABLE, timeline_events).
-define(MAX_EVENTS, 1000000). % Keep last 1M events
-define(CLEANUP_INTERVAL, 3600000). % Cleanup every hour

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

%% Retrieve events with pagination
retrieve(Limit, Offset) ->
    gen_server:call(?SERVER, {retrieve, Limit, Offset}).

%% Retrieve events within a time range
retrieve_range(StartTime, EndTime, Options) ->
    gen_server:call(?SERVER, {retrieve_range, StartTime, EndTime, Options}).

%% Retrieve events for a specific agent
retrieve_by_agent(AgentId, Options) ->
    gen_server:call(?SERVER, {retrieve_by_agent, AgentId, Options}).

%% Retrieve events by type
retrieve_by_type(Type, Options) ->
    gen_server:call(?SERVER, {retrieve_by_type, Type, Options}).

%% Stream events in real-time
stream_events(FilterFun, Pid) ->
    gen_server:call(?SERVER, {stream_events, FilterFun, Pid}).

%% Get total event count
get_event_count() ->
    gen_server:call(?SERVER, get_event_count).

%% Clear events older than specified time
clear_old_events(Age) ->
    gen_server:call(?SERVER, {clear_old_events, Age}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% Create ETS table for timeline events
    ets:new(?TABLE, [
        named_table,
        ordered_set,
        public,
        {keypos, #timeline_event.id},
        {read_concurrency, true},
        {write_concurrency, true}
    ]),
    
    %% Create indices
    ets:new(timeline_by_timestamp, [
        named_table,
        ordered_set,
        public,
        {read_concurrency, true}
    ]),
    
    ets:new(timeline_by_agent, [
        named_table,
        bag,
        public,
        {read_concurrency, true}
    ]),
    
    ets:new(timeline_by_type, [
        named_table,
        bag,
        public,
        {read_concurrency, true}
    ]),
    
    %% Start cleanup timer
    TimerRef = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup),
    
    %% Load persisted events if any
    load_persisted_events(),
    
    {ok, #state{event_count = 0, cleanup_timer = TimerRef}}.

handle_call({retrieve, Limit, Offset}, _From, State) ->
    Events = retrieve_events(Limit, Offset),
    {reply, {ok, Events}, State};

handle_call({retrieve_range, StartTime, EndTime, Options}, _From, State) ->
    Events = retrieve_events_by_range(StartTime, EndTime, Options),
    {reply, {ok, Events}, State};

handle_call({retrieve_by_agent, AgentId, Options}, _From, State) ->
    Events = retrieve_events_by_agent(AgentId, Options),
    {reply, {ok, Events}, State};

handle_call({retrieve_by_type, Type, Options}, _From, State) ->
    Events = retrieve_events_by_type(Type, Options),
    {reply, {ok, Events}, State};

handle_call({stream_events, FilterFun, Pid}, _From, State) ->
    %% Register the streaming client
    gproc:reg({p, l, {timeline_stream, FilterFun}}),
    {reply, ok, State};

handle_call(get_event_count, _From, State) ->
    {reply, State#state.event_count, State};

handle_call({clear_old_events, Age}, _From, State) ->
    DeletedCount = clear_old_events_internal(Age),
    {reply, {ok, DeletedCount}, State#state{event_count = State#state.event_count - DeletedCount}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({store, Event}, State) ->
    %% Generate event ID and timestamp
    EventId = generate_event_id(),
    Timestamp = erlang:timestamp(),
    
    %% Create timeline event record
    TimelineEvent = #timeline_event{
        id = EventId,
        timestamp = Timestamp,
        type = maps:get(type, Event, unknown),
        agent_id = maps:get(agent_id, Event, undefined),
        data = Event,
        indexed_fields = extract_indexed_fields(Event)
    },
    
    %% Store in main table
    ets:insert(?TABLE, TimelineEvent),
    
    %% Update indices
    update_indices(TimelineEvent),
    
    %% Notify streaming clients
    notify_stream_clients(TimelineEvent),
    
    %% Persist to disk asynchronously
    spawn(fun() -> persist_event(TimelineEvent) end),
    
    {noreply, State#state{event_count = State#state.event_count + 1}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup, State) ->
    %% Clean up old events if we exceed max count
    if
        State#state.event_count > ?MAX_EVENTS ->
            spawn(fun() -> cleanup_old_events() end);
        true ->
            ok
    end,
    
    %% Schedule next cleanup
    erlang:cancel_timer(State#state.cleanup_timer),
    NewTimerRef = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup),
    
    {noreply, State#state{cleanup_timer = NewTimerRef}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    erlang:cancel_timer(State#state.cleanup_timer),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

generate_event_id() ->
    agent_uuid:generate().

extract_indexed_fields(Event) ->
    %% Extract fields that should be indexed for fast retrieval
    #{
        task_id => maps:get(task_id, Event, undefined),
        conversation_id => maps:get(conversation_id, Event, undefined),
        tool_name => maps:get(tool_name, Event, undefined),
        error_type => maps:get(error_type, Event, undefined)
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

retrieve_events(Limit, Offset) ->
    %% Get events ordered by timestamp (newest first)
    AllEvents = ets:select(timeline_by_timestamp, [{{'$1'}, [], ['$1']}]),
    SortedEvents = lists:reverse(AllEvents),
    
    %% Apply pagination
    EventIds = lists:sublist(lists:nthtail(Offset, [Id || {_Timestamp, Id} <- SortedEvents]), Limit),
    
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
    
    %% Sort by timestamp and apply limit
    Events = lists:sublist(
        lists:sort(
            fun(A, B) ->
                case {ets:lookup(?TABLE, A), ets:lookup(?TABLE, B)} of
                    {[EventA], [EventB]} ->
                        timestamp_greater_than(
                            EventA#timeline_event.timestamp,
                            EventB#timeline_event.timestamp
                        );
                    _ -> false
                end
            end,
            EventIds
        ),
        Limit
    ),
    
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
    
    %% Sort by timestamp and apply limit
    Events = lists:sublist(
        lists:sort(
            fun(A, B) ->
                case {ets:lookup(?TABLE, A), ets:lookup(?TABLE, B)} of
                    {[EventA], [EventB]} ->
                        timestamp_greater_than(
                            EventA#timeline_event.timestamp,
                            EventB#timeline_event.timestamp
                        );
                    _ -> false
                end
            end,
            EventIds
        ),
        Limit
    ),
    
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
    %% Notify all registered streaming clients
    Subscribers = gproc:lookup_pids({p, l, timeline_stream}),
    lists:foreach(
        fun({Pid, FilterFun}) ->
            case FilterFun(Event) of
                true -> Pid ! {timeline_event, event_to_map(Event)};
                false -> ok
            end
        end,
        Subscribers
    ).

persist_event(Event) ->
    %% Persist to disk (using NDJSON format)
    case file:open("apps/agent_web/priv/data/timeline_events.ndjson", [append, {encoding, utf8}]) of
        {ok, File} ->
            Json = jsx:encode(event_to_map(Event)),
            file:write(File, [Json, "\n"]),
            file:close(File);
        {error, _Reason} ->
            ok
    end.

load_persisted_events() ->
    %% Load events from disk on startup
    case file:read_file("apps/agent_web/priv/data/timeline_events.ndjson") of
        {ok, Data} ->
            Lines = binary:split(Data, <<"\n">>, [global]),
            lists:foreach(
                fun(Line) ->
                    case Line of
                        <<>> -> ok;
                        _ ->
                            try
                                Event = jsx:decode(Line, [return_maps]),
                                gen_server:cast(?SERVER, {store, Event})
                            catch
                                _:_ -> ok
                            end
                    end
                end,
                Lines
            );
        {error, _} ->
            ok
    end.

cleanup_old_events() ->
    %% Remove oldest events if we exceed max count
    EventCount = ets:info(?TABLE, size),
    if
        EventCount > ?MAX_EVENTS ->
            ToDelete = EventCount - ?MAX_EVENTS,
            OldestEvents = ets:select(timeline_by_timestamp, [{{'$1'}, [], ['$1']}], ToDelete),
            lists:foreach(
                fun({_Timestamp, EventId}) ->
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
                OldestEvents
            );
        true ->
            ok
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