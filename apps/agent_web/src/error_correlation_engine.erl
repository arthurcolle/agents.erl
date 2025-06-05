-module(error_correlation_engine).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([correlate_error/2, get_error_patterns/0, get_correlation_stats/0]).

-define(SERVER, ?MODULE).
-define(MAX_ERROR_HISTORY, 1000).
-define(CORRELATION_WINDOW, 30000). % 30 seconds

-record(state, {
    error_history = [],
    patterns = #{},
    correlations = #{},
    stats = #{total_errors => 0, patterns_found => 0}
}).

start_link() ->
    colored_logger:ocean(deep, "[ERR_CORR] Starting Error Correlation Engine"),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    colored_logger:success("[ERR_CORR] Error Correlation Engine initialized", []),
    {ok, #state{}}.

handle_call(get_error_patterns, _From, State) ->
    {reply, State#state.patterns, State};

handle_call(get_correlation_stats, _From, State) ->
    {reply, State#state.stats, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({correlate_error, Error, Context}, State) ->
    NewState = process_error_correlation(Error, Context, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    colored_logger:warning("[ERR_CORR] Error Correlation Engine terminating", []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Public API
correlate_error(Error, Context) ->
    gen_server:cast(?SERVER, {correlate_error, Error, Context}).

get_error_patterns() ->
    gen_server:call(?SERVER, get_error_patterns).

get_correlation_stats() ->
    gen_server:call(?SERVER, get_correlation_stats).

%% Process error correlation
process_error_correlation(Error, Context, State) ->
    Timestamp = erlang:system_time(millisecond),
    
    %% Create error record
    ErrorRecord = #{
        error => Error,
        context => Context,
        timestamp => Timestamp,
        normalized => normalize_error(Error)
    },
    
    %% Add to history (with size limit)
    NewHistory = add_to_history(ErrorRecord, State#state.error_history),
    
    %% Look for patterns
    {NewPatterns, PatternFound} = detect_patterns(ErrorRecord, NewHistory, State#state.patterns),
    
    %% Update correlations
    NewCorrelations = update_correlations(ErrorRecord, NewHistory, State#state.correlations),
    
    %% Update stats
    NewStats = update_stats(State#state.stats, PatternFound),
    
    %% Log interesting findings
    log_correlation_findings(ErrorRecord, PatternFound, NewCorrelations),
    
    State#state{
        error_history = NewHistory,
        patterns = NewPatterns,
        correlations = NewCorrelations,
        stats = NewStats
    }.

%% Add error to history with size limit
add_to_history(ErrorRecord, History) ->
    NewHistory = [ErrorRecord | History],
    case length(NewHistory) > ?MAX_ERROR_HISTORY of
        true -> lists:sublist(NewHistory, ?MAX_ERROR_HISTORY);
        false -> NewHistory
    end.

%% Normalize error for pattern matching
normalize_error({Format, _Args}) when is_list(Format) ->
    %% Extract format pattern
    format_to_pattern(Format);
normalize_error(Error) when is_binary(Error) ->
    binary_to_list(Error);
normalize_error(Error) when is_atom(Error) ->
    atom_to_list(Error);
normalize_error(Error) ->
    io_lib:format("~p", [Error]).

%% Convert format string to pattern
format_to_pattern(Format) ->
    %% Replace format specifiers with wildcards
    re:replace(Format, "~[wpnski]+", "*", [global, {return, list}]).

%% Detect error patterns
detect_patterns(ErrorRecord, History, Patterns) ->
    NormalizedError = maps:get(normalized, ErrorRecord),
    Timestamp = maps:get(timestamp, ErrorRecord),
    
    %% Count recent occurrences of this error pattern
    RecentCount = count_recent_occurrences(NormalizedError, Timestamp, History),
    
    PatternFound = case RecentCount of
        Count when Count >= 3 ->
            colored_logger:warning("[ERR_CORR] Pattern detected: '~s' occurred ~p times recently", 
                                 [NormalizedError, Count]),
            true;
        Count when Count >= 5 ->
            colored_logger:fire(bright, "[ERR_CORR] High frequency pattern: '~s' occurred ~p times!", 
                              [NormalizedError, Count]),
            true;
        _ ->
            false
    end,
    
    %% Update pattern tracking
    NewPatterns = case PatternFound of
        true ->
            Pattern = maps:get(NormalizedError, Patterns, #{count => 0, first_seen => Timestamp}),
            UpdatedPattern = Pattern#{
                count => maps:get(count, Pattern, 0) + 1,
                last_seen => Timestamp
            },
            Patterns#{NormalizedError => UpdatedPattern};
        false ->
            Patterns
    end,
    
    {NewPatterns, PatternFound}.

%% Count recent occurrences of an error pattern
count_recent_occurrences(Pattern, CurrentTime, History) ->
    WindowStart = CurrentTime - ?CORRELATION_WINDOW,
    lists:foldl(fun(#{normalized := Normalized, timestamp := Time}, Count) ->
        case Normalized =:= Pattern andalso Time >= WindowStart of
            true -> Count + 1;
            false -> Count
        end
    end, 0, History).

%% Update error correlations
update_correlations(ErrorRecord, History, Correlations) ->
    Context = maps:get(context, ErrorRecord),
    Timestamp = maps:get(timestamp, ErrorRecord),
    
    %% Look for errors that happened around the same time
    WindowStart = Timestamp - 5000, % 5 second window
    WindowEnd = Timestamp + 5000,
    
    CooccurringErrors = lists:filter(fun(#{timestamp := Time}) ->
        Time >= WindowStart andalso Time =< WindowEnd
    end, History),
    
    %% Update correlation counts
    case length(CooccurringErrors) > 1 of
        true ->
            colored_logger:info("[ERR_CORR] Found ~p correlated errors in 5s window", 
                              [length(CooccurringErrors)]),
            %% Simple correlation tracking - could be more sophisticated
            CorrelationKey = lists:sort([maps:get(normalized, E) || E <- CooccurringErrors]),
            Count = maps:get(CorrelationKey, Correlations, 0),
            Correlations#{CorrelationKey => Count + 1};
        false ->
            Correlations
    end.

%% Update statistics
update_stats(Stats, PatternFound) ->
    NewTotal = maps:get(total_errors, Stats, 0) + 1,
    NewPatterns = case PatternFound of
        true -> maps:get(patterns_found, Stats, 0) + 1;
        false -> maps:get(patterns_found, Stats, 0)
    end,
    Stats#{
        total_errors => NewTotal,
        patterns_found => NewPatterns
    }.

%% Log interesting correlation findings
log_correlation_findings(ErrorRecord, PatternFound, Correlations) ->
    case PatternFound of
        true ->
            Error = maps:get(error, ErrorRecord),
            Context = maps:get(context, ErrorRecord),
            colored_logger:warning("[ERR_CORR] Error pattern analysis: ~p in context ~p", [Error, Context]);
        false ->
            ok
    end,
    
    %% Log if we have strong correlations
    StrongCorrelations = maps:filter(fun(_, Count) -> Count >= 3 end, Correlations),
    case maps:size(StrongCorrelations) > 0 of
        true ->
            colored_logger:fire(bright, "[ERR_CORR] Strong correlations detected: ~p", 
                              [maps:keys(StrongCorrelations)]);
        false ->
            ok
    end.