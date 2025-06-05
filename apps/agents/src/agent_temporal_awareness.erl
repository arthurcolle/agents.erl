-module(agent_temporal_awareness).
-behaviour(gen_server).

%% API
-export([start_link/0,
         enable_temporal_awareness/1,
         disable_temporal_awareness/1,
         get_temporal_context/1,
         remember_event/2,
         recall_events/2,
         predict_future_events/2,
         analyze_temporal_patterns/1,
         set_temporal_window/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_WINDOW, 3600). % 1 hour default temporal window

-record(temporal_context, {
    agent_id :: binary(),
    past_events :: [map()],
    current_state :: map(),
    predicted_futures :: [map()],
    temporal_patterns :: map(),
    awareness_window :: integer()
}).

-record(state, {
    contexts :: #{binary() => #temporal_context{}},
    pattern_analyzer :: pid()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Enable temporal awareness for an agent
enable_temporal_awareness(AgentId) ->
    gen_server:call(?SERVER, {enable_awareness, AgentId}).

%% Disable temporal awareness for an agent
disable_temporal_awareness(AgentId) ->
    gen_server:call(?SERVER, {disable_awareness, AgentId}).

%% Get the full temporal context for an agent
get_temporal_context(AgentId) ->
    gen_server:call(?SERVER, {get_context, AgentId}).

%% Remember a significant event
remember_event(AgentId, Event) ->
    gen_server:cast(?SERVER, {remember, AgentId, Event}).

%% Recall events based on criteria
recall_events(AgentId, Criteria) ->
    gen_server:call(?SERVER, {recall, AgentId, Criteria}).

%% Predict future events based on patterns
predict_future_events(AgentId, TimeHorizon) ->
    gen_server:call(?SERVER, {predict, AgentId, TimeHorizon}).

%% Analyze temporal patterns for an agent
analyze_temporal_patterns(AgentId) ->
    gen_server:call(?SERVER, {analyze_patterns, AgentId}).

%% Set the temporal awareness window
set_temporal_window(AgentId, WindowSize) ->
    gen_server:call(?SERVER, {set_window, AgentId, WindowSize}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% Start pattern analyzer process
    PatternAnalyzer = spawn_link(fun pattern_analyzer_loop/0),
    
    %% Subscribe to timeline events
    gproc:reg({p, l, timeline_events}),
    
    {ok, #state{contexts = #{}, pattern_analyzer = PatternAnalyzer}}.

handle_call({enable_awareness, AgentId}, _From, State) ->
    Context = #temporal_context{
        agent_id = AgentId,
        past_events = [],
        current_state = #{},
        predicted_futures = [],
        temporal_patterns = #{},
        awareness_window = ?DEFAULT_WINDOW
    },
    
    %% Load historical events for the agent
    spawn(fun() -> load_historical_events(AgentId) end),
    
    NewContexts = maps:put(AgentId, Context, State#state.contexts),
    {reply, ok, State#state{contexts = NewContexts}};

handle_call({disable_awareness, AgentId}, _From, State) ->
    NewContexts = maps:remove(AgentId, State#state.contexts),
    {reply, ok, State#state{contexts = NewContexts}};

handle_call({get_context, AgentId}, _From, State) ->
    case maps:find(AgentId, State#state.contexts) of
        {ok, Context} ->
            ContextMap = #{
                agent_id => Context#temporal_context.agent_id,
                past_events => Context#temporal_context.past_events,
                current_state => Context#temporal_context.current_state,
                predicted_futures => Context#temporal_context.predicted_futures,
                temporal_patterns => Context#temporal_context.temporal_patterns,
                awareness_window => Context#temporal_context.awareness_window,
                temporal_summary => generate_temporal_summary(Context)
            },
            {reply, {ok, ContextMap}, State};
        error ->
            {reply, {error, not_enabled}, State}
    end;

handle_call({recall, AgentId, Criteria}, _From, State) ->
    case maps:find(AgentId, State#state.contexts) of
        {ok, Context} ->
            Events = recall_events_by_criteria(Context, Criteria),
            {reply, {ok, Events}, State};
        error ->
            {reply, {error, not_enabled}, State}
    end;

handle_call({predict, AgentId, TimeHorizon}, _From, State) ->
    case maps:find(AgentId, State#state.contexts) of
        {ok, Context} ->
            Predictions = generate_predictions(Context, TimeHorizon),
            UpdatedContext = Context#temporal_context{predicted_futures = Predictions},
            NewContexts = maps:put(AgentId, UpdatedContext, State#state.contexts),
            {reply, {ok, Predictions}, State#state{contexts = NewContexts}};
        error ->
            {reply, {error, not_enabled}, State}
    end;

handle_call({analyze_patterns, AgentId}, _From, State) ->
    case maps:find(AgentId, State#state.contexts) of
        {ok, Context} ->
            State#state.pattern_analyzer ! {analyze, self(), Context},
            receive
                {patterns, Patterns} ->
                    UpdatedContext = Context#temporal_context{temporal_patterns = Patterns},
                    NewContexts = maps:put(AgentId, UpdatedContext, State#state.contexts),
                    {reply, {ok, Patterns}, State#state{contexts = NewContexts}}
            after 5000 ->
                {reply, {error, timeout}, State}
            end;
        error ->
            {reply, {error, not_enabled}, State}
    end;

handle_call({set_window, AgentId, WindowSize}, _From, State) ->
    case maps:find(AgentId, State#state.contexts) of
        {ok, Context} ->
            UpdatedContext = Context#temporal_context{awareness_window = WindowSize},
            NewContexts = maps:put(AgentId, UpdatedContext, State#state.contexts),
            {reply, ok, State#state{contexts = NewContexts}};
        error ->
            {reply, {error, not_enabled}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({remember, AgentId, Event}, State) ->
    case maps:find(AgentId, State#state.contexts) of
        {ok, Context} ->
            %% Add timestamp if not present
            TimestampedEvent = case maps:get(timestamp, Event, undefined) of
                undefined -> Event#{timestamp => erlang:timestamp()};
                _ -> Event
            end,
            
            %% Update past events (keep limited history)
            NewPastEvents = [TimestampedEvent | Context#temporal_context.past_events],
            TrimmedEvents = lists:sublist(NewPastEvents, 1000), % Keep last 1000 events
            
            %% Update current state based on event type
            NewCurrentState = update_current_state(Context#temporal_context.current_state, TimestampedEvent),
            
            UpdatedContext = Context#temporal_context{
                past_events = TrimmedEvents,
                current_state = NewCurrentState
            },
            
            NewContexts = maps:put(AgentId, UpdatedContext, State#state.contexts),
            {noreply, State#state{contexts = NewContexts}};
        error ->
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeline_event, Event}, State) ->
    %% Process timeline events for temporal awareness
    case maps:get(agent_id, Event, undefined) of
        undefined ->
            {noreply, State};
        AgentId ->
            case maps:find(AgentId, State#state.contexts) of
                {ok, _Context} ->
                    gen_server:cast(self(), {remember, AgentId, Event}),
                    {noreply, State};
                error ->
                    {noreply, State}
            end
    end;

handle_info({gproc_ps_event, timeline_events, Event}, State) ->
    %% Handle gproc published events
    handle_info({timeline_event, Event}, State);

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

load_historical_events(AgentId) ->
    %% Load recent events from timeline store
    case timeline_event_store:retrieve_by_agent(AgentId, #{limit => 100}) of
        {ok, Events} ->
            lists:foreach(
                fun(Event) ->
                    gen_server:cast(?SERVER, {remember, AgentId, Event})
                end,
                lists:reverse(Events)
            );
        _ ->
            ok
    end.

recall_events_by_criteria(Context, Criteria) ->
    %% Filter past events based on criteria
    Events = Context#temporal_context.past_events,
    
    case Criteria of
        #{type := Type} ->
            lists:filter(
                fun(E) -> maps:get(type, E, undefined) =:= Type end,
                Events
            );
        #{time_range := {Start, End}} ->
            lists:filter(
                fun(E) ->
                    T = maps:get(timestamp, E, {0,0,0}),
                    timestamp_in_range(T, Start, End)
                end,
                Events
            );
        #{pattern := Pattern} ->
            lists:filter(
                fun(E) -> matches_pattern(E, Pattern) end,
                Events
            );
        _ ->
            Events
    end.

generate_predictions(Context, TimeHorizon) ->
    %% Generate predictions based on temporal patterns
    Patterns = Context#temporal_context.temporal_patterns,
    CurrentState = Context#temporal_context.current_state,
    
    %% Simple prediction based on recurring patterns
    RecurringEvents = maps:get(recurring_events, Patterns, []),
    
    lists:map(
        fun({EventType, Interval}) ->
            #{
                type => predicted_event,
                event_type => EventType,
                predicted_time => add_seconds_to_timestamp(erlang:timestamp(), Interval),
                confidence => calculate_confidence(EventType, Context),
                based_on => recurring_pattern
            }
        end,
        RecurringEvents
    ).

generate_temporal_summary(Context) ->
    %% Generate a human-readable summary of temporal context
    EventCount = length(Context#temporal_context.past_events),
    PatternCount = maps:size(Context#temporal_context.temporal_patterns),
    PredictionCount = length(Context#temporal_context.predicted_futures),
    
    RecentEvents = lists:sublist(Context#temporal_context.past_events, 5),
    EventTypes = lists:usort([maps:get(type, E, unknown) || E <- RecentEvents]),
    
    #{
        total_events => EventCount,
        pattern_count => PatternCount,
        prediction_count => PredictionCount,
        recent_event_types => EventTypes,
        temporal_state => analyze_temporal_state(Context)
    }.

update_current_state(CurrentState, Event) ->
    %% Update current state based on event
    Type = maps:get(type, Event, unknown),
    
    %% Track event counts
    EventCounts = maps:get(event_counts, CurrentState, #{}),
    NewCount = maps:get(Type, EventCounts, 0) + 1,
    NewEventCounts = maps:put(Type, NewCount, EventCounts),
    
    %% Update last occurrence times
    LastOccurrences = maps:get(last_occurrences, CurrentState, #{}),
    NewLastOccurrences = maps:put(Type, maps:get(timestamp, Event, erlang:timestamp()), LastOccurrences),
    
    %% Update state
    CurrentState#{
        event_counts => NewEventCounts,
        last_occurrences => NewLastOccurrences,
        last_event => Event,
        last_update => erlang:timestamp()
    }.

pattern_analyzer_loop() ->
    receive
        {analyze, From, Context} ->
            Patterns = analyze_patterns(Context),
            From ! {patterns, Patterns},
            pattern_analyzer_loop()
    end.

analyze_patterns(Context) ->
    Events = Context#temporal_context.past_events,
    
    %% Analyze recurring patterns
    RecurringPatterns = find_recurring_patterns(Events),
    
    %% Analyze sequences
    SequencePatterns = find_sequence_patterns(Events),
    
    %% Analyze temporal correlations
    Correlations = find_temporal_correlations(Events),
    
    #{
        recurring_events => RecurringPatterns,
        sequences => SequencePatterns,
        correlations => Correlations,
        analysis_time => erlang:timestamp()
    }.

find_recurring_patterns(Events) ->
    %% Group events by type
    TypeGroups = lists:foldl(
        fun(Event, Acc) ->
            Type = maps:get(type, Event, unknown),
            maps:update_with(Type, fun(L) -> [Event | L] end, [Event], Acc)
        end,
        #{},
        Events
    ),
    
    %% Find average intervals for each type
    maps:fold(
        fun(Type, TypeEvents, Acc) when length(TypeEvents) > 2 ->
            Intervals = calculate_intervals(TypeEvents),
            case analyze_intervals(Intervals) of
                {recurring, AvgInterval} ->
                    [{Type, AvgInterval} | Acc];
                _ ->
                    Acc
            end;
           (_, _, Acc) ->
            Acc
        end,
        [],
        TypeGroups
    ).

find_sequence_patterns(Events) ->
    %% Find common event sequences
    %% Simplified implementation - looks for pairs
    Pairs = find_event_pairs(Events),
    
    %% Count occurrences
    PairCounts = lists:foldl(
        fun(Pair, Acc) ->
            maps:update_with(Pair, fun(C) -> C + 1 end, 1, Acc)
        end,
        #{},
        Pairs
    ),
    
    %% Filter significant patterns
    maps:filter(fun(_, Count) -> Count > 2 end, PairCounts).

find_temporal_correlations(_Events) ->
    %% Placeholder for correlation analysis
    #{}.

calculate_intervals(Events) ->
    %% Calculate time intervals between events
    SortedEvents = lists:sort(
        fun(A, B) ->
            timestamp_less_than(
                maps:get(timestamp, A, {0,0,0}),
                maps:get(timestamp, B, {0,0,0})
            )
        end,
        Events
    ),
    
    case SortedEvents of
        [_] -> [];
        _ ->
            lists:zipwith(
                fun(A, B) ->
                    timestamp_diff(
                        maps:get(timestamp, B, {0,0,0}),
                        maps:get(timestamp, A, {0,0,0})
                    )
                end,
                lists:droplast(SortedEvents),
                tl(SortedEvents)
            )
    end.

analyze_intervals(Intervals) when length(Intervals) < 2 ->
    non_recurring;
analyze_intervals(Intervals) ->
    Avg = lists:sum(Intervals) / length(Intervals),
    StdDev = math:sqrt(lists:sum([math:pow(I - Avg, 2) || I <- Intervals]) / length(Intervals)),
    
    %% If standard deviation is less than 20% of average, consider it recurring
    if
        StdDev < (Avg * 0.2) ->
            {recurring, round(Avg)};
        true ->
            non_recurring
    end.

find_event_pairs(Events) when length(Events) < 2 ->
    [];
find_event_pairs([E1, E2 | Rest]) ->
    Type1 = maps:get(type, E1, unknown),
    Type2 = maps:get(type, E2, unknown),
    [{Type1, Type2} | find_event_pairs([E2 | Rest])].

calculate_confidence(EventType, Context) ->
    %% Calculate prediction confidence based on historical accuracy
    %% Simplified implementation
    EventCounts = maps:get(event_counts, maps:get(current_state, Context, #{}), #{}),
    Count = maps:get(EventType, EventCounts, 0),
    
    if
        Count > 10 -> 0.9;
        Count > 5 -> 0.7;
        Count > 2 -> 0.5;
        true -> 0.3
    end.

analyze_temporal_state(Context) ->
    %% Analyze the current temporal state
    Events = Context#temporal_context.past_events,
    
    case Events of
        [] -> idle;
        [LastEvent | _] ->
            TimeSinceLastEvent = timestamp_diff(
                erlang:timestamp(),
                maps:get(timestamp, LastEvent, erlang:timestamp())
            ),
            
            if
                TimeSinceLastEvent < 60 -> active;
                TimeSinceLastEvent < 300 -> recent;
                TimeSinceLastEvent < 3600 -> quiet;
                true -> dormant
            end
    end.

matches_pattern(_Event, _Pattern) ->
    %% Placeholder for pattern matching
    false.

timestamp_in_range(Timestamp, Start, End) ->
    timestamp_greater_than(Timestamp, Start) andalso
    timestamp_less_than(Timestamp, End).

timestamp_less_than({M1, S1, U1}, {M2, S2, U2}) ->
    (M1 < M2) orelse 
    (M1 =:= M2 andalso S1 < S2) orelse
    (M1 =:= M2 andalso S1 =:= S2 andalso U1 < U2).

timestamp_greater_than({M1, S1, U1}, {M2, S2, U2}) ->
    (M1 > M2) orelse 
    (M1 =:= M2 andalso S1 > S2) orelse
    (M1 =:= M2 andalso S1 =:= S2 andalso U1 > U2).

timestamp_diff({M2, S2, U2}, {M1, S1, U1}) ->
    ((M2 - M1) * 1000000 + (S2 - S1)) * 1000000 + (U2 - U1).

add_seconds_to_timestamp({Mega, Sec, Micro}, Seconds) ->
    TotalMicro = (Mega * 1000000 + Sec) * 1000000 + Micro + (Seconds * 1000000),
    NewMega = TotalMicro div 1000000000000,
    NewSec = (TotalMicro rem 1000000000000) div 1000000,
    NewMicro = TotalMicro rem 1000000,
    {NewMega, NewSec, NewMicro}.