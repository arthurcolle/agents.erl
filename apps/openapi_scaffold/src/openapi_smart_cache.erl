%%%-------------------------------------------------------------------
%%% @doc Intelligent Caching System
%%% Smart caching with TTL from headers, request deduplication,
%%% and predictive prefetching.
%%% @end
%%%-------------------------------------------------------------------
-module(openapi_smart_cache).
-behaviour(gen_server).

-export([
    start_link/0,
    get/2,
    put/4,
    invalidate/1,
    invalidate_pattern/1,
    get_stats/0,
    configure/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    cache :: ets:tid(),
    dedup_queue :: ets:tid(),
    stats :: ets:tid(),
    prefetch_predictor :: pid(),
    config :: map()
}).

-record(cache_entry, {
    key :: term(),
    value :: term(),
    ttl :: integer(),
    expires_at :: integer(),
    headers :: map(),
    hit_count :: integer(),
    last_access :: integer(),
    size :: integer()
}).

-record(dedup_entry, {
    key :: term(),
    waiters :: [pid()],
    started_at :: integer()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Get value from cache with smart features
get(Key, Options) ->
    gen_server:call(?MODULE, {get, Key, Options}).

%% @doc Put value in cache with TTL extraction
put(Key, Value, Headers, Options) ->
    gen_server:cast(?MODULE, {put, Key, Value, Headers, Options}).

%% @doc Invalidate specific cache key
invalidate(Key) ->
    gen_server:cast(?MODULE, {invalidate, Key}).

%% @doc Invalidate keys matching pattern
invalidate_pattern(Pattern) ->
    gen_server:cast(?MODULE, {invalidate_pattern, Pattern}).

%% @doc Get cache statistics
get_stats() ->
    gen_server:call(?MODULE, get_stats).

%% @doc Configure cache behavior
configure(Config) ->
    gen_server:call(?MODULE, {configure, Config}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Create ETS tables for cache storage
    Cache = ets:new(smart_cache, [
        set, 
        protected, 
        {keypos, #cache_entry.key},
        {read_concurrency, true}
    ]),
    
    DedupQueue = ets:new(dedup_queue, [
        set,
        protected,
        {keypos, #dedup_entry.key}
    ]),
    
    Stats = ets:new(cache_stats, [
        set,
        public,
        {write_concurrency, true}
    ]),
    
    %% Initialize stats
    ets:insert(Stats, [
        {hits, 0},
        {misses, 0},
        {evictions, 0},
        {dedup_savings, 0}
    ]),
    
    %% Start prefetch predictor
    {ok, Predictor} = start_prefetch_predictor(),
    
    %% Start cleanup timer
    timer:send_interval(60000, cleanup_expired),
    
    %% Default configuration
    Config = #{
        max_size => 100 * 1024 * 1024, % 100MB
        max_entries => 10000,
        default_ttl => 300, % 5 minutes
        enable_compression => true,
        enable_deduplication => true,
        enable_prefetch => true,
        eviction_policy => lru
    },
    
    State = #state{
        cache = Cache,
        dedup_queue = DedupQueue,
        stats = Stats,
        prefetch_predictor = Predictor,
        config = Config
    },
    
    {ok, State}.

handle_call({get, Key, Options}, From, State) ->
    %% Check if request is being deduplicated
    case should_deduplicate(Key, Options, State) of
        {wait, _} ->
            %% Add to waiters list
            add_dedup_waiter(Key, From, State),
            {noreply, State};
        proceed ->
            %% Normal cache lookup
            Result = cache_lookup(Key, State),
            handle_cache_result(Result, Key, From, State)
    end;

handle_call(get_stats, _From, State) ->
    Stats = collect_stats(State),
    {reply, {ok, Stats}, State};

handle_call({configure, NewConfig}, _From, State) ->
    UpdatedConfig = maps:merge(State#state.config, NewConfig),
    {reply, ok, State#state{config = UpdatedConfig}}.

handle_cast({put, Key, Value, Headers, Options}, State) ->
    %% Extract TTL from headers or use default
    TTL = extract_ttl(Headers, State#state.config),
    
    %% Compress if enabled and beneficial
    {StoredValue, Size} = maybe_compress(Value, State#state.config),
    
    %% Create cache entry
    Entry = #cache_entry{
        key = Key,
        value = StoredValue,
        ttl = TTL,
        expires_at = erlang:system_time(second) + TTL,
        headers = Headers,
        hit_count = 0,
        last_access = erlang:system_time(second),
        size = Size
    },
    
    %% Check cache size limits
    State2 = ensure_cache_limits(Entry, State),
    
    %% Store in cache
    ets:insert(State2#state.cache, Entry),
    
    %% Notify dedup waiters if any
    notify_dedup_waiters(Key, {ok, Value}, State2),
    
    %% Update prefetch predictor
    update_prefetch_predictor(Key, State2),
    
    {noreply, State2};

handle_cast({invalidate, Key}, State) ->
    ets:delete(State#state.cache, Key),
    increment_stat(State#state.stats, evictions),
    {noreply, State};

handle_cast({invalidate_pattern, Pattern}, State) ->
    %% Find and delete all keys matching pattern
    MatchSpec = [{#cache_entry{key = '$1', _ = '_'}, 
                  [{'=:=', {pattern_match, Pattern, '$1'}, true}], 
                  ['$1']}],
    Keys = ets:select(State#state.cache, MatchSpec),
    [ets:delete(State#state.cache, K) || K <- Keys],
    increment_stat(State#state.stats, evictions, length(Keys)),
    {noreply, State}.

handle_info(cleanup_expired, State) ->
    %% Remove expired entries
    Now = erlang:system_time(second),
    MatchSpec = [{#cache_entry{expires_at = '$1', _ = '_'}, 
                  [{'<', '$1', Now}], 
                  [true]}],
    NumDeleted = ets:select_delete(State#state.cache, MatchSpec),
    increment_stat(State#state.stats, evictions, NumDeleted),
    {noreply, State};

handle_info({prefetch_suggestion, Keys}, State) ->
    %% Prefetch suggested keys
    case maps:get(enable_prefetch, State#state.config) of
        true -> prefetch_keys(Keys, State);
        false -> ok
    end,
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions - Cache Operations
%%====================================================================

cache_lookup(Key, State) ->
    case ets:lookup(State#state.cache, Key) of
        [#cache_entry{expires_at = ExpiresAt} = Entry] ->
            Now = erlang:system_time(second),
            if
                ExpiresAt > Now ->
                    %% Valid entry, update stats
                    increment_stat(State#state.stats, hits),
                    update_access_stats(Entry, State),
                    decompress_value(Entry, State);
                true ->
                    %% Expired entry
                    ets:delete(State#state.cache, Key),
                    increment_stat(State#state.stats, misses),
                    miss
            end;
        [] ->
            increment_stat(State#state.stats, misses),
            miss
    end.

handle_cache_result(miss, Key, From, State) ->
    %% Start deduplication for this key
    case start_deduplication(Key, From, State) of
        {started, State2} ->
            {noreply, State2};
        {exists, _} ->
            %% Already being fetched
            add_dedup_waiter(Key, From, State),
            {noreply, State}
    end;

handle_cache_result({ok, Value}, _Key, _From, State) ->
    {reply, {ok, Value}, State}.

%%====================================================================
%% Internal functions - Deduplication
%%====================================================================

should_deduplicate(Key, Options, State) ->
    case maps:get(enable_deduplication, State#state.config) of
        false -> proceed;
        true ->
            case ets:lookup(State#state.dedup_queue, Key) of
                [#dedup_entry{started_at = StartedAt}] ->
                    %% Check if request is still in flight (timeout after 30s)
                    Now = erlang:system_time(second),
                    if
                        Now - StartedAt < 30 -> {wait, Key};
                        true -> 
                            %% Timeout, remove stale entry
                            ets:delete(State#state.dedup_queue, Key),
                            proceed
                    end;
                [] ->
                    proceed
            end
    end.

start_deduplication(Key, From, State) ->
    Entry = #dedup_entry{
        key = Key,
        waiters = [From],
        started_at = erlang:system_time(second)
    },
    case ets:insert_new(State#state.dedup_queue, Entry) of
        true -> {started, State};
        false -> {exists, State}
    end.

add_dedup_waiter(Key, From, State) ->
    case ets:lookup(State#state.dedup_queue, Key) of
        [#dedup_entry{waiters = Waiters} = Entry] ->
            UpdatedEntry = Entry#dedup_entry{waiters = [From | Waiters]},
            ets:insert(State#state.dedup_queue, UpdatedEntry);
        [] ->
            %% Race condition, entry was removed
            gen_server:reply(From, miss)
    end.

notify_dedup_waiters(Key, Result, State) ->
    case ets:lookup(State#state.dedup_queue, Key) of
        [#dedup_entry{waiters = Waiters}] ->
            %% Reply to all waiters
            [gen_server:reply(From, Result) || From <- Waiters],
            %% Update dedup savings stat
            increment_stat(State#state.stats, dedup_savings, length(Waiters)),
            %% Remove dedup entry
            ets:delete(State#state.dedup_queue, Key);
        [] ->
            ok
    end.

%%====================================================================
%% Internal functions - TTL Extraction
%%====================================================================

extract_ttl(Headers, Config) ->
    %% Try to extract TTL from various cache headers
    CacheControl = maps:get(<<"cache-control">>, Headers, <<>>),
    
    case parse_cache_control(CacheControl) of
        {ok, TTL} -> TTL;
        error ->
            %% Try Expires header
            case maps:get(<<"expires">>, Headers, undefined) of
                undefined -> 
                    maps:get(default_ttl, Config);
                ExpiresStr ->
                    parse_expires_header(ExpiresStr, Config)
            end
    end.

parse_cache_control(<<>>) -> error;
parse_cache_control(CacheControl) ->
    %% Parse Cache-Control header for max-age
    case re:run(CacheControl, "max-age=([0-9]+)", [{capture, [1], binary}]) of
        {match, [MaxAgeStr]} ->
            {ok, binary_to_integer(MaxAgeStr)};
        nomatch ->
            %% Check for no-cache directive
            case re:run(CacheControl, "no-cache|no-store", []) of
                {match, _} -> {ok, 0}; % Don't cache
                nomatch -> error
            end
    end.

parse_expires_header(ExpiresStr, Config) ->
    %% Parse HTTP date and calculate TTL
    case httpd_util:convert_request_date(binary_to_list(ExpiresStr)) of
        {ok, ExpiresTime} ->
            Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
            TTL = max(0, ExpiresTime - Now),
            TTL;
        {error, _} ->
            maps:get(default_ttl, Config)
    end.

%%====================================================================
%% Internal functions - Compression
%%====================================================================

maybe_compress(Value, Config) ->
    case maps:get(enable_compression, Config) of
        false -> 
            Size = byte_size(term_to_binary(Value)),
            {Value, Size};
        true ->
            Binary = term_to_binary(Value),
            Size = byte_size(Binary),
            %% Only compress if beneficial (> 1KB)
            if
                Size > 1024 ->
                    Compressed = zlib:compress(Binary),
                    CompressedSize = byte_size(Compressed),
                    if
                        CompressedSize < Size * 0.9 ->
                            {{compressed, Compressed}, CompressedSize};
                        true ->
                            {Value, Size}
                    end;
                true ->
                    {Value, Size}
            end
    end.

decompress_value(#cache_entry{value = {compressed, Compressed}}, _State) ->
    Binary = zlib:uncompress(Compressed),
    {ok, binary_to_term(Binary)};
decompress_value(#cache_entry{value = Value}, _State) ->
    {ok, Value}.

%%====================================================================
%% Internal functions - Cache Limits
%%====================================================================

ensure_cache_limits(NewEntry, State) ->
    %% Check entry count limit
    CurrentCount = ets:info(State#state.cache, size),
    MaxEntries = maps:get(max_entries, State#state.config),
    
    State2 = if
        CurrentCount >= MaxEntries ->
            evict_entries(1, State);
        true ->
            State
    end,
    
    %% Check size limit
    CurrentSize = calculate_cache_size(State2),
    MaxSize = maps:get(max_size, State2#state.config),
    NewSize = NewEntry#cache_entry.size,
    
    if
        CurrentSize + NewSize > MaxSize ->
            %% Need to evict entries to make room
            BytesToEvict = (CurrentSize + NewSize) - MaxSize,
            evict_by_size(BytesToEvict, State2);
        true ->
            State2
    end.

calculate_cache_size(State) ->
    ets:foldl(fun(#cache_entry{size = Size}, Acc) -> 
        Acc + Size 
    end, 0, State#state.cache).

evict_entries(Count, State) ->
    %% Evict based on configured policy
    Policy = maps:get(eviction_policy, State#state.config),
    
    Candidates = case Policy of
        lru -> get_lru_candidates(Count, State);
        lfu -> get_lfu_candidates(Count, State);
        fifo -> get_fifo_candidates(Count, State)
    end,
    
    [ets:delete(State#state.cache, Key) || Key <- Candidates],
    increment_stat(State#state.stats, evictions, length(Candidates)),
    State.

evict_by_size(BytesToEvict, State) ->
    %% Evict entries until we've freed enough space
    Policy = maps:get(eviction_policy, State#state.config),
    
    AllEntries = ets:tab2list(State#state.cache),
    SortedEntries = sort_for_eviction(AllEntries, Policy),
    
    {ToEvict, _} = lists:foldl(fun(Entry, {Evict, BytesFreed}) ->
        if
            BytesFreed >= BytesToEvict -> {Evict, BytesFreed};
            true -> {[Entry#cache_entry.key | Evict], BytesFreed + Entry#cache_entry.size}
        end
    end, {[], 0}, SortedEntries),
    
    [ets:delete(State#state.cache, Key) || Key <- ToEvict],
    increment_stat(State#state.stats, evictions, length(ToEvict)),
    State.

get_lru_candidates(Count, State) ->
    %% Get least recently used entries
    AllEntries = ets:tab2list(State#state.cache),
    Sorted = lists:sort(fun(#cache_entry{last_access = A}, #cache_entry{last_access = B}) ->
        A < B
    end, AllEntries),
    [E#cache_entry.key || E <- lists:sublist(Sorted, Count)].

get_lfu_candidates(Count, State) ->
    %% Get least frequently used entries
    AllEntries = ets:tab2list(State#state.cache),
    Sorted = lists:sort(fun(#cache_entry{hit_count = A}, #cache_entry{hit_count = B}) ->
        A < B
    end, AllEntries),
    [E#cache_entry.key || E <- lists:sublist(Sorted, Count)].

get_fifo_candidates(Count, State) ->
    %% Get oldest entries (could track insertion time)
    AllEntries = ets:tab2list(State#state.cache),
    [E#cache_entry.key || E <- lists:sublist(AllEntries, Count)].

sort_for_eviction(Entries, lru) ->
    lists:sort(fun(#cache_entry{last_access = A}, #cache_entry{last_access = B}) ->
        A < B
    end, Entries);
sort_for_eviction(Entries, lfu) ->
    lists:sort(fun(#cache_entry{hit_count = A}, #cache_entry{hit_count = B}) ->
        A < B
    end, Entries);
sort_for_eviction(Entries, fifo) ->
    Entries.

%%====================================================================
%% Internal functions - Statistics
%%====================================================================

increment_stat(Stats, Key) ->
    increment_stat(Stats, Key, 1).

increment_stat(Stats, Key, Value) ->
    ets:update_counter(Stats, Key, Value).

update_access_stats(#cache_entry{key = Key} = Entry, State) ->
    %% Update hit count and last access time
    UpdatedEntry = Entry#cache_entry{
        hit_count = Entry#cache_entry.hit_count + 1,
        last_access = erlang:system_time(second)
    },
    ets:insert(State#state.cache, UpdatedEntry).

collect_stats(State) ->
    Stats = ets:tab2list(State#state.stats),
    CacheInfo = #{
        entry_count => ets:info(State#state.cache, size),
        memory_size => ets:info(State#state.cache, memory),
        total_size => calculate_cache_size(State)
    },
    
    maps:from_list(Stats ++ [{cache_info, CacheInfo}]).

%%====================================================================
%% Internal functions - Prefetching
%%====================================================================

start_prefetch_predictor() ->
    %% Start a simple prefetch predictor process
    spawn_link(fun prefetch_predictor_loop/0).

prefetch_predictor_loop() ->
    %% Simple predictive prefetching based on access patterns
    PatternTable = ets:new(prefetch_patterns, [set, private]),
    prefetch_predictor_loop(PatternTable).

prefetch_predictor_loop(PatternTable) ->
    receive
        {access_pattern, Key} ->
            %% Update access patterns
            update_access_pattern(PatternTable, Key),
            %% Generate prefetch suggestions
            Suggestions = generate_prefetch_suggestions(PatternTable, Key),
            case Suggestions of
                [] -> ok;
                _ -> self() ! {prefetch_suggestion, Suggestions}
            end,
            prefetch_predictor_loop(PatternTable);
        _ ->
            prefetch_predictor_loop(PatternTable)
    end.

update_prefetch_predictor(Key, State) ->
    case maps:get(enable_prefetch, State#state.config) of
        true ->
            State#state.prefetch_predictor ! {access_pattern, Key};
        false ->
            ok
    end.

update_access_pattern(_PatternTable, _Key) ->
    %% Simplified - in production would track sequential patterns, 
    %% co-occurrence, time-based patterns, etc.
    ok.

generate_prefetch_suggestions(_PatternTable, _Key) ->
    %% Simplified - would analyze patterns and suggest keys to prefetch
    [].

prefetch_keys(Keys, State) ->
    %% Asynchronously fetch and cache suggested keys
    [spawn(fun() -> prefetch_single_key(K, State) end) || K <- Keys].

prefetch_single_key(_Key, _State) ->
    %% Would implement actual prefetching logic
    ok.