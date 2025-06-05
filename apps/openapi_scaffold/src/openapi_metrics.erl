%%%-------------------------------------------------------------------
%%% @doc Advanced Metrics and Monitoring System
%%% Real-time metrics with Prometheus integration, OpenTelemetry tracing,
%%% performance profiling, cost tracking, and anomaly detection.
%%% @end
%%%-------------------------------------------------------------------
-module(openapi_metrics).
-behaviour(gen_server).

-export([
    start_link/0,
    record_request/4,
    record_response/5,
    record_error/4,
    record_cache_hit/2,
    record_cache_miss/2,
    record_transformation_time/3,
    record_api_cost/3,
    get_endpoint_metrics/1,
    get_anomalies/0,
    get_sla_report/1,
    register_transformer/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    metrics_table :: ets:tid(),
    anomaly_detector :: pid(),
    cost_tracker :: pid(),
    sla_monitor :: pid(),
    trace_config :: map()
}).

-record(endpoint_metrics, {
    endpoint :: binary(),
    request_count :: integer(),
    error_count :: integer(),
    total_latency :: integer(),
    min_latency :: integer(),
    max_latency :: integer(),
    percentiles :: map(),
    status_codes :: map(),
    cache_hits :: integer(),
    cache_misses :: integer(),
    total_cost :: float(),
    last_updated :: integer()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Record incoming request
record_request(Endpoint, Method, Headers, Body) ->
    gen_server:cast(?MODULE, {record_request, Endpoint, Method, Headers, Body}).

%% @doc Record response details
record_response(Endpoint, StatusCode, Latency, ResponseSize, Headers) ->
    gen_server:cast(?MODULE, {record_response, Endpoint, StatusCode, Latency, ResponseSize, Headers}).

%% @doc Record error occurrence
record_error(Endpoint, ErrorType, ErrorDetails, Context) ->
    gen_server:cast(?MODULE, {record_error, Endpoint, ErrorType, ErrorDetails, Context}).

%% @doc Record cache hit
record_cache_hit(Transformer, Operation) ->
    gen_server:cast(?MODULE, {cache_hit, Transformer, Operation}).

%% @doc Record cache miss
record_cache_miss(Transformer, Operation) ->
    gen_server:cast(?MODULE, {cache_miss, Transformer, Operation}).

%% @doc Record transformation time
record_transformation_time(Transformer, Operation, Duration) ->
    gen_server:cast(?MODULE, {transformation_time, Transformer, Operation, Duration}).

%% @doc Record API call cost
record_api_cost(Endpoint, Provider, Cost) ->
    gen_server:cast(?MODULE, {api_cost, Endpoint, Provider, Cost}).

%% @doc Get metrics for specific endpoint
get_endpoint_metrics(Endpoint) ->
    gen_server:call(?MODULE, {get_endpoint_metrics, Endpoint}).

%% @doc Get detected anomalies
get_anomalies() ->
    gen_server:call(?MODULE, get_anomalies).

%% @doc Get SLA compliance report
get_sla_report(Period) ->
    gen_server:call(?MODULE, {get_sla_report, Period}).

%% @doc Register a transformer for metrics
register_transformer(Name) ->
    gen_server:call(?MODULE, {register_transformer, Name}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Initialize ETS table for metrics
    MetricsTable = ets:new(api_metrics, [
        set,
        public,
        {keypos, #endpoint_metrics.endpoint},
        {write_concurrency, true}
    ]),
    
    %% Initialize Prometheus metrics
    init_prometheus_metrics(),
    
    %% Initialize OpenTelemetry
    init_opentelemetry(),
    
    %% Start anomaly detector
    {ok, AnomalyDetector} = start_anomaly_detector(),
    
    %% Start cost tracker
    {ok, CostTracker} = start_cost_tracker(),
    
    %% Start SLA monitor
    {ok, SLAMonitor} = start_sla_monitor(),
    
    %% Schedule periodic aggregation
    timer:send_interval(60000, aggregate_metrics),
    
    State = #state{
        metrics_table = MetricsTable,
        anomaly_detector = AnomalyDetector,
        cost_tracker = CostTracker,
        sla_monitor = SLAMonitor,
        trace_config = #{}
    },
    
    {ok, State}.

handle_call({get_endpoint_metrics, Endpoint}, _From, State) ->
    Metrics = case ets:lookup(State#state.metrics_table, Endpoint) of
        [M] -> {ok, format_endpoint_metrics(M)};
        [] -> {error, not_found}
    end,
    {reply, Metrics, State};

handle_call(get_anomalies, _From, State) ->
    Anomalies = get_current_anomalies(State#state.anomaly_detector),
    {reply, {ok, Anomalies}, State};

handle_call({get_sla_report, Period}, _From, State) ->
    Report = generate_sla_report(State#state.sla_monitor, Period),
    {reply, {ok, Report}, State};

handle_call({register_transformer, Name}, _From, State) ->
    %% Register transformer-specific metrics
    prometheus_counter:new([
        {name, transform_cache_hits},
        {help, "Number of cache hits in transformers"},
        {labels, [transformer, operation]}
    ]),
    prometheus_counter:new([
        {name, transform_cache_misses},
        {help, "Number of cache misses in transformers"},
        {labels, [transformer, operation]}
    ]),
    prometheus_histogram:new([
        {name, transformation_duration_microseconds},
        {help, "Time spent in transformations"},
        {labels, [transformer, operation]},
        {buckets, [100, 500, 1000, 5000, 10000, 50000, 100000]}
    ]),
    {reply, Name, State}.

handle_cast({record_request, Endpoint, Method, Headers, Body}, State) ->
    %% Start trace span
    SpanCtx = start_trace_span(Endpoint, Method),
    
    %% Update Prometheus metrics
    prometheus_counter:inc(api_requests_total, [Endpoint, Method]),
    
    %% Record request size
    BodySize = byte_size(term_to_binary(Body)),
    prometheus_histogram:observe(request_size_bytes, [Endpoint], BodySize),
    
    %% Store trace context
    put({trace_ctx, Endpoint}, SpanCtx),
    
    {noreply, State};

handle_cast({record_response, Endpoint, StatusCode, Latency, ResponseSize, Headers}, State) ->
    %% Update ETS metrics
    update_endpoint_metrics(State#state.metrics_table, Endpoint, 
                          StatusCode, Latency, ResponseSize),
    
    %% Update Prometheus metrics
    prometheus_histogram:observe(api_latency_milliseconds, [Endpoint, integer_to_list(StatusCode)], Latency),
    prometheus_histogram:observe(response_size_bytes, [Endpoint], ResponseSize),
    prometheus_counter:inc(api_responses_total, [Endpoint, integer_to_list(StatusCode)]),
    
    %% End trace span
    case get({trace_ctx, Endpoint}) of
        undefined -> ok;
        SpanCtx -> 
            end_trace_span(SpanCtx, StatusCode),
            erase({trace_ctx, Endpoint})
    end,
    
    %% Check for anomalies
    check_latency_anomaly(State#state.anomaly_detector, Endpoint, Latency),
    
    %% Update SLA monitoring
    update_sla_metrics(State#state.sla_monitor, Endpoint, StatusCode, Latency),
    
    {noreply, State};

handle_cast({record_error, Endpoint, ErrorType, ErrorDetails, Context}, State) ->
    %% Update error metrics
    prometheus_counter:inc(api_errors_total, [Endpoint, atom_to_list(ErrorType)]),
    
    %% Log to anomaly detector
    report_error_anomaly(State#state.anomaly_detector, Endpoint, ErrorType, ErrorDetails),
    
    %% Record error in trace
    case get({trace_ctx, Endpoint}) of
        undefined -> ok;
        SpanCtx -> add_trace_error(SpanCtx, ErrorType, ErrorDetails)
    end,
    
    {noreply, State};

handle_cast({cache_hit, Transformer, Operation}, State) ->
    prometheus_counter:inc(transform_cache_hits, [atom_to_list(Transformer), Operation]),
    {noreply, State};

handle_cast({cache_miss, Transformer, Operation}, State) ->
    prometheus_counter:inc(transform_cache_misses, [atom_to_list(Transformer), Operation]),
    {noreply, State};

handle_cast({transformation_time, Transformer, Operation, Duration}, State) ->
    prometheus_histogram:observe(transformation_duration_microseconds, 
                               [atom_to_list(Transformer), Operation], Duration),
    {noreply, State};

handle_cast({api_cost, Endpoint, Provider, Cost}, State) ->
    %% Record API usage cost
    update_cost_metrics(State#state.cost_tracker, Endpoint, Provider, Cost),
    prometheus_counter:inc(api_cost_dollars, [Endpoint, Provider], Cost),
    {noreply, State}.

handle_info(aggregate_metrics, State) ->
    %% Periodic metrics aggregation
    aggregate_all_metrics(State#state.metrics_table),
    
    %% Export to external systems if configured
    export_metrics(State),
    
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions - Prometheus
%%====================================================================

init_prometheus_metrics() ->
    %% Request/Response metrics
    prometheus_counter:new([
        {name, api_requests_total},
        {help, "Total number of API requests"},
        {labels, [endpoint, method]}
    ]),
    
    prometheus_counter:new([
        {name, api_responses_total},
        {help, "Total number of API responses"},
        {labels, [endpoint, status_code]}
    ]),
    
    prometheus_histogram:new([
        {name, api_latency_milliseconds},
        {help, "API request latency in milliseconds"},
        {labels, [endpoint, status_code]},
        {buckets, [10, 50, 100, 250, 500, 1000, 2500, 5000, 10000]}
    ]),
    
    prometheus_histogram:new([
        {name, request_size_bytes},
        {help, "Request body size in bytes"},
        {labels, [endpoint]},
        {buckets, [100, 1000, 10000, 100000, 1000000]}
    ]),
    
    prometheus_histogram:new([
        {name, response_size_bytes},
        {help, "Response body size in bytes"},
        {labels, [endpoint]},
        {buckets, [100, 1000, 10000, 100000, 1000000]}
    ]),
    
    %% Error metrics
    prometheus_counter:new([
        {name, api_errors_total},
        {help, "Total number of API errors"},
        {labels, [endpoint, error_type]}
    ]),
    
    %% Cost metrics
    prometheus_counter:new([
        {name, api_cost_dollars},
        {help, "Cumulative API cost in dollars"},
        {labels, [endpoint, provider]}
    ]).

%%====================================================================
%% Internal functions - OpenTelemetry
%%====================================================================

init_opentelemetry() ->
    %% Configure OpenTelemetry
    opentelemetry:register_tracer(openapi_scaffold, "1.0.0"),
    
    %% Set up exporters
    otel_batch_processor:set_exporter(otel_exporter_jaeger, #{
        endpoints => ["http://localhost:14268/api/traces"]
    }).

start_trace_span(Endpoint, Method) ->
    %% Start a new trace span
    SpanName = iolist_to_binary([Method, " ", Endpoint]),
    Ctx = otel_tracer:start_span(openapi_scaffold, SpanName, #{}),
    
    %% Add span attributes
    otel_span:set_attributes(Ctx, [
        {"http.method", Method},
        {"http.target", Endpoint},
        {"http.scheme", "https"},
        {"span.kind", "server"}
    ]),
    
    Ctx.

end_trace_span(SpanCtx, StatusCode) ->
    %% Set final span attributes
    otel_span:set_attributes(SpanCtx, [
        {"http.status_code", StatusCode}
    ]),
    
    %% Set span status based on HTTP status
    Status = if
        StatusCode >= 200, StatusCode < 300 -> ok;
        StatusCode >= 400, StatusCode < 500 -> {error, "Client error"};
        StatusCode >= 500 -> {error, "Server error"};
        true -> unset
    end,
    otel_span:set_status(SpanCtx, Status),
    
    %% End the span
    otel_span:finish(SpanCtx).

add_trace_error(SpanCtx, ErrorType, ErrorDetails) ->
    otel_span:record_exception(SpanCtx, ErrorType, ErrorDetails, []).

%%====================================================================
%% Internal functions - Metrics Updates
%%====================================================================

update_endpoint_metrics(Table, Endpoint, StatusCode, Latency, ResponseSize) ->
    Default = #endpoint_metrics{
        endpoint = Endpoint,
        request_count = 0,
        error_count = 0,
        total_latency = 0,
        min_latency = infinity,
        max_latency = 0,
        percentiles = #{},
        status_codes = #{},
        cache_hits = 0,
        cache_misses = 0,
        total_cost = 0.0,
        last_updated = erlang:system_time(second)
    },
    
    %% Update metrics atomically
    ets:update_counter(Table, Endpoint, 
        [{#endpoint_metrics.request_count, 1},
         {#endpoint_metrics.total_latency, Latency}], Default),
    
    %% Update min/max latency and status codes
    case ets:lookup(Table, Endpoint) of
        [Metrics] ->
            Updated = Metrics#endpoint_metrics{
                min_latency = min(Metrics#endpoint_metrics.min_latency, Latency),
                max_latency = max(Metrics#endpoint_metrics.max_latency, Latency),
                status_codes = update_status_count(Metrics#endpoint_metrics.status_codes, StatusCode),
                error_count = case StatusCode >= 400 of
                    true -> Metrics#endpoint_metrics.error_count + 1;
                    false -> Metrics#endpoint_metrics.error_count
                end,
                last_updated = erlang:system_time(second)
            },
            ets:insert(Table, Updated);
        [] ->
            ok
    end.

update_status_count(StatusCodes, StatusCode) ->
    Key = integer_to_binary(StatusCode),
    maps:update_with(Key, fun(V) -> V + 1 end, 1, StatusCodes).

%%====================================================================
%% Internal functions - Anomaly Detection
%%====================================================================

start_anomaly_detector() ->
    spawn_link(fun() -> anomaly_detector_loop(#{}) end).

anomaly_detector_loop(State) ->
    receive
        {check_latency, Endpoint, Latency} ->
            NewState = check_latency_anomaly_internal(State, Endpoint, Latency),
            anomaly_detector_loop(NewState);
        {report_error, Endpoint, ErrorType, Details} ->
            NewState = track_error_pattern(State, Endpoint, ErrorType, Details),
            anomaly_detector_loop(NewState);
        {get_anomalies, From} ->
            Anomalies = detect_current_anomalies(State),
            From ! {anomalies, Anomalies},
            anomaly_detector_loop(State);
        _ ->
            anomaly_detector_loop(State)
    end.

check_latency_anomaly(Detector, Endpoint, Latency) ->
    Detector ! {check_latency, Endpoint, Latency}.

check_latency_anomaly_internal(State, Endpoint, Latency) ->
    %% Simple anomaly detection using moving average and standard deviation
    History = maps:get(Endpoint, State, {[], 0, 0}),
    {Samples, Sum, SumSq} = History,
    
    %% Keep last 100 samples
    NewSamples = lists:sublist([Latency | Samples], 100),
    N = length(NewSamples),
    NewSum = lists:sum(NewSamples),
    NewSumSq = lists:sum([X*X || X <- NewSamples]),
    
    %% Calculate mean and standard deviation
    Mean = NewSum / N,
    Variance = (NewSumSq / N) - (Mean * Mean),
    StdDev = math:sqrt(max(0, Variance)),
    
    %% Check if current latency is anomalous (> 3 std devs)
    if
        N > 10, Latency > Mean + 3 * StdDev ->
            report_anomaly(latency_spike, #{
                endpoint => Endpoint,
                latency => Latency,
                mean => Mean,
                std_dev => StdDev
            });
        true ->
            ok
    end,
    
    maps:put(Endpoint, {NewSamples, NewSum, NewSumSq}, State).

report_error_anomaly(Detector, Endpoint, ErrorType, Details) ->
    Detector ! {report_error, Endpoint, ErrorType, Details}.

track_error_pattern(State, Endpoint, ErrorType, _Details) ->
    %% Track error rates over time windows
    Now = erlang:system_time(second),
    Window = 300, % 5 minutes
    
    Key = {Endpoint, ErrorType},
    ErrorHistory = maps:get(Key, State, []),
    
    %% Remove old entries
    RecentErrors = [T || T <- ErrorHistory, Now - T < Window],
    UpdatedErrors = [Now | RecentErrors],
    
    %% Check for error spike
    ErrorRate = length(UpdatedErrors) / Window,
    if
        ErrorRate > 0.1 -> % More than 10% error rate
            report_anomaly(error_spike, #{
                endpoint => Endpoint,
                error_type => ErrorType,
                error_rate => ErrorRate,
                window_seconds => Window
            });
        true ->
            ok
    end,
    
    maps:put(Key, UpdatedErrors, State).

get_current_anomalies(Detector) ->
    Detector ! {get_anomalies, self()},
    receive
        {anomalies, Anomalies} -> Anomalies
    after 5000 ->
        []
    end.

detect_current_anomalies(_State) ->
    %% Return currently active anomalies
    %% In production, would maintain a list of active anomalies
    [].

report_anomaly(Type, Details) ->
    %% Log anomaly and trigger alerts
    error_logger:warning_msg("API Anomaly Detected: ~p - ~p~n", [Type, Details]),
    
    %% Send to monitoring system
    prometheus_counter:inc(api_anomalies_total, [atom_to_list(Type)]).

%%====================================================================
%% Internal functions - Cost Tracking
%%====================================================================

start_cost_tracker() ->
    spawn_link(fun() -> cost_tracker_loop(#{}) end).

cost_tracker_loop(State) ->
    receive
        {update_cost, Endpoint, Provider, Cost} ->
            NewState = update_cost_internal(State, Endpoint, Provider, Cost),
            cost_tracker_loop(NewState);
        {get_costs, From, Period} ->
            Costs = calculate_costs(State, Period),
            From ! {costs, Costs},
            cost_tracker_loop(State);
        _ ->
            cost_tracker_loop(State)
    end.

update_cost_metrics(Tracker, Endpoint, Provider, Cost) ->
    Tracker ! {update_cost, Endpoint, Provider, Cost}.

update_cost_internal(State, Endpoint, Provider, Cost) ->
    Now = erlang:system_time(second),
    Key = {Endpoint, Provider},
    
    Entry = maps:get(Key, State, []),
    UpdatedEntry = [{Now, Cost} | Entry],
    
    %% Keep last 30 days of cost data
    CutoffTime = Now - (30 * 24 * 60 * 60),
    TrimmedEntry = [{T, C} || {T, C} <- UpdatedEntry, T > CutoffTime],
    
    maps:put(Key, TrimmedEntry, State).

calculate_costs(State, Period) ->
    Now = erlang:system_time(second),
    StartTime = Now - Period,
    
    maps:fold(fun({Endpoint, Provider}, Entries, Acc) ->
        PeriodCosts = [C || {T, C} <- Entries, T >= StartTime],
        TotalCost = lists:sum(PeriodCosts),
        [{Endpoint, Provider, TotalCost} | Acc]
    end, [], State).

%%====================================================================
%% Internal functions - SLA Monitoring
%%====================================================================

start_sla_monitor() ->
    spawn_link(fun() -> sla_monitor_loop(#{}) end).

sla_monitor_loop(State) ->
    receive
        {update_sla, Endpoint, StatusCode, Latency} ->
            NewState = update_sla_internal(State, Endpoint, StatusCode, Latency),
            sla_monitor_loop(NewState);
        {get_report, From, Period} ->
            Report = generate_sla_report_internal(State, Period),
            From ! {sla_report, Report},
            sla_monitor_loop(State);
        _ ->
            sla_monitor_loop(State)
    end.

update_sla_metrics(Monitor, Endpoint, StatusCode, Latency) ->
    Monitor ! {update_sla, Endpoint, StatusCode, Latency}.

update_sla_internal(State, Endpoint, StatusCode, Latency) ->
    Now = erlang:system_time(second),
    
    Entry = maps:get(Endpoint, State, {[], []}),
    {LatencyHistory, StatusHistory} = Entry,
    
    %% Keep hourly granularity for SLA calculation
    Hour = Now div 3600,
    
    UpdatedLatency = [{Hour, Latency} | LatencyHistory],
    UpdatedStatus = [{Hour, StatusCode} | StatusHistory],
    
    %% Keep last 30 days
    CutoffHour = Hour - (30 * 24),
    TrimmedLatency = [{H, L} || {H, L} <- UpdatedLatency, H > CutoffHour],
    TrimmedStatus = [{H, S} || {H, S} <- UpdatedStatus, H > CutoffHour],
    
    maps:put(Endpoint, {TrimmedLatency, TrimmedStatus}, State).

generate_sla_report(Monitor, Period) ->
    Monitor ! {get_report, self(), Period},
    receive
        {sla_report, Report} -> Report
    after 5000 ->
        #{error => timeout}
    end.

generate_sla_report_internal(State, Period) ->
    Now = erlang:system_time(second),
    StartTime = Now - Period,
    StartHour = StartTime div 3600,
    
    maps:fold(fun(Endpoint, {LatencyHistory, StatusHistory}, Acc) ->
        %% Calculate SLA metrics for period
        PeriodLatencies = [L || {H, L} <- LatencyHistory, H >= StartHour],
        PeriodStatuses = [S || {H, S} <- StatusHistory, H >= StartHour],
        
        %% Calculate availability (non-5xx responses)
        TotalRequests = length(PeriodStatuses),
        SuccessfulRequests = length([S || S <- PeriodStatuses, S < 500]),
        Availability = case TotalRequests of
            0 -> 100.0;
            _ -> (SuccessfulRequests / TotalRequests) * 100
        end,
        
        %% Calculate latency percentiles
        SortedLatencies = lists:sort(PeriodLatencies),
        P50 = percentile(SortedLatencies, 0.5),
        P95 = percentile(SortedLatencies, 0.95),
        P99 = percentile(SortedLatencies, 0.99),
        
        SLAMetrics = #{
            endpoint => Endpoint,
            availability_percent => Availability,
            total_requests => TotalRequests,
            successful_requests => SuccessfulRequests,
            latency_p50 => P50,
            latency_p95 => P95,
            latency_p99 => P99
        },
        
        [SLAMetrics | Acc]
    end, [], State).

percentile([], _) -> 0;
percentile(List, P) ->
    N = length(List),
    K = erlang:round(P * N),
    lists:nth(max(1, K), List).

%%====================================================================
%% Internal functions - Utilities
%%====================================================================

format_endpoint_metrics(#endpoint_metrics{} = M) ->
    #{
        endpoint => M#endpoint_metrics.endpoint,
        request_count => M#endpoint_metrics.request_count,
        error_count => M#endpoint_metrics.error_count,
        error_rate => calculate_error_rate(M),
        average_latency => calculate_average_latency(M),
        min_latency => format_latency(M#endpoint_metrics.min_latency),
        max_latency => M#endpoint_metrics.max_latency,
        status_codes => M#endpoint_metrics.status_codes,
        cache_hit_rate => calculate_cache_hit_rate(M),
        total_cost => M#endpoint_metrics.total_cost,
        last_updated => M#endpoint_metrics.last_updated
    }.

calculate_error_rate(#endpoint_metrics{request_count = 0}) -> 0.0;
calculate_error_rate(#endpoint_metrics{error_count = E, request_count = R}) ->
    (E / R) * 100.

calculate_average_latency(#endpoint_metrics{request_count = 0}) -> 0;
calculate_average_latency(#endpoint_metrics{total_latency = T, request_count = R}) ->
    T div R.

format_latency(infinity) -> 0;
format_latency(L) -> L.

calculate_cache_hit_rate(#endpoint_metrics{cache_hits = H, cache_misses = M}) ->
    Total = H + M,
    case Total of
        0 -> 0.0;
        _ -> (H / Total) * 100
    end.

aggregate_all_metrics(Table) ->
    %% Perform periodic aggregations
    AllMetrics = ets:tab2list(Table),
    
    %% Calculate and store percentiles
    lists:foreach(fun(M) ->
        %% In production, would maintain a proper histogram
        %% and calculate accurate percentiles
        ets:insert(Table, M)
    end, AllMetrics).

export_metrics(State) ->
    %% Export metrics to external systems if configured
    %% Could send to Graphite, InfluxDB, CloudWatch, etc.
    ok.