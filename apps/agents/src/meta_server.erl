%%%-------------------------------------------------------------------
%%% @doc Meta-Server
%%% Separate server for viewing and interacting with meta-operations.
%%% Provides HTTP endpoints and WebSocket connections for monitoring
%%% meta-system activities, error feedback, and self-modification processes.
%%% @end
%%%-------------------------------------------------------------------
-module(meta_server).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([get_meta_status/0,
         get_error_dashboard/0,
         get_feedback_summary/0,
         get_self_modification_log/0,
         get_system_health/0,
         execute_meta_command/2,
         subscribe_to_updates/1,
         unsubscribe_from_updates/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% HTTP handler exports
-export([handle_http_request/2]).

-define(SERVER, ?MODULE).
-define(HTTP_PORT, 8083).

-record(state, {
    %% Server state
    http_listener,               % HTTP listener reference
    subscribers = [],            % WebSocket subscribers
    
    %% Cached data for fast access
    meta_status_cache = #{},     % Cached meta-system status
    error_cache = [],            % Recent errors
    feedback_cache = [],         % Recent feedback
    modification_cache = [],     % Recent modifications
    
    %% Update intervals and timers
    cache_update_timer,          % Timer for cache updates
    status_broadcast_timer       % Timer for status broadcasts
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Get overall meta-system status
get_meta_status() ->
    gen_server:call(?SERVER, get_meta_status).

%% @doc Get error dashboard data
get_error_dashboard() ->
    gen_server:call(?SERVER, get_error_dashboard).

%% @doc Get feedback summary
get_feedback_summary() ->
    gen_server:call(?SERVER, get_feedback_summary).

%% @doc Get self-modification log
get_self_modification_log() ->
    gen_server:call(?SERVER, get_self_modification_log).

%% @doc Get system health report
get_system_health() ->
    gen_server:call(?SERVER, get_system_health).

%% @doc Execute a meta-command
execute_meta_command(Command, Parameters) ->
    gen_server:call(?SERVER, {execute_command, Command, Parameters}).

%% @doc Subscribe to real-time updates
subscribe_to_updates(SubscriberPid) ->
    gen_server:cast(?SERVER, {subscribe, SubscriberPid}).

%% @doc Unsubscribe from real-time updates
unsubscribe_from_updates(SubscriberPid) ->
    gen_server:cast(?SERVER, {unsubscribe, SubscriberPid}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Start HTTP server for meta-operations viewing
    case start_http_server() of
        {ok, Listener} ->
            %% Set up periodic cache updates
            {ok, UpdateTimer} = timer:send_interval(5000, self(), update_caches),
            
            %% Set up periodic status broadcasts
            {ok, BroadcastTimer} = timer:send_interval(10000, self(), broadcast_status),
            
            %% Register with meta-layer coordinator
            self() ! register_with_coordinator,
            
            %% Initial cache population
            self() ! update_caches,
            
            {ok, #state{
                http_listener = Listener,
                cache_update_timer = UpdateTimer,
                status_broadcast_timer = BroadcastTimer
            }};
        {error, Reason} ->
            %% Could not start HTTP server, but continue without it
            io:format("Meta-Server running without HTTP interface: ~p~n", [Reason]),
            
            %% Set up periodic cache updates
            {ok, UpdateTimer} = timer:send_interval(5000, self(), update_caches),
            
            %% Set up periodic status broadcasts
            {ok, BroadcastTimer} = timer:send_interval(10000, self(), broadcast_status),
            
            %% Register with meta-layer coordinator
            self() ! register_with_coordinator,
            
            %% Initial cache population
            self() ! update_caches,
            
            {ok, #state{
                http_listener = undefined,
                cache_update_timer = UpdateTimer,
                status_broadcast_timer = BroadcastTimer
            }}
    end.

handle_call(get_meta_status, _From, State) ->
    Status = compile_meta_status(State),
    {reply, {ok, Status}, State};

handle_call(get_error_dashboard, _From, State) ->
    Dashboard = compile_error_dashboard(State),
    {reply, {ok, Dashboard}, State};

handle_call(get_feedback_summary, _From, State) ->
    Summary = compile_feedback_summary(State),
    {reply, {ok, Summary}, State};

handle_call(get_self_modification_log, _From, State) ->
    Log = compile_modification_log(State),
    {reply, {ok, Log}, State};

handle_call(get_system_health, _From, State) ->
    Health = compile_system_health(State),
    {reply, {ok, Health}, State};

handle_call({execute_command, Command, Parameters}, _From, State) ->
    Result = execute_meta_command_impl(Command, Parameters),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({subscribe, SubscriberPid}, State) ->
    %% Monitor the subscriber
    erlang:monitor(process, SubscriberPid),
    
    %% Add to subscriber list
    NewSubscribers = [SubscriberPid | State#state.subscribers],
    
    %% Send current status to new subscriber
    send_status_update(SubscriberPid, State),
    
    {noreply, State#state{subscribers = NewSubscribers}};

handle_cast({unsubscribe, SubscriberPid}, State) ->
    NewSubscribers = lists:delete(SubscriberPid, State#state.subscribers),
    {noreply, State#state{subscribers = NewSubscribers}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(register_with_coordinator, State) ->
    case erlang:whereis(meta_layer_coordinator) of
        undefined ->
            erlang:send_after(5000, self(), register_with_coordinator);
        _Pid ->
            meta_layer_coordinator:register_meta_system(meta_server, self())
    end,
    {noreply, State};

handle_info(update_caches, State) ->
    NewState = update_all_caches(State),
    {noreply, NewState};

handle_info(broadcast_status, State) ->
    broadcast_to_subscribers(State),
    {noreply, State};

handle_info({'DOWN', _MonitorRef, process, Pid, _Reason}, State) ->
    %% Remove crashed subscriber
    NewSubscribers = lists:delete(Pid, State#state.subscribers),
    {noreply, State#state{subscribers = NewSubscribers}};

handle_info({meta_event, EventType, EventData}, State) ->
    %% Handle meta-events and broadcast to subscribers
    NewState = handle_meta_event(EventType, EventData, State),
    broadcast_meta_event(EventType, EventData, State),
    {noreply, NewState};

handle_info({http_request, Ref, Method, Path, Headers, Body}, State) ->
    %% Handle HTTP requests
    Response = handle_http_request(Method, Path, Headers, Body),
    Ref ! {http_response, Response},
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Clean up timers
    timer:cancel(State#state.cache_update_timer),
    timer:cancel(State#state.status_broadcast_timer),
    
    %% Stop HTTP listener if it exists
    case State#state.http_listener of
        undefined -> ok;
        Listener -> stop_http_server(Listener)
    end,
    
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

start_http_server() ->
    %% Start simple HTTP server for meta-operations
    %% In production, would use Cowboy or similar
    case gen_tcp:listen(?HTTP_PORT, [
        binary,
        {packet, http_bin},
        {active, false},
        {reuseaddr, true}
    ]) of
        {ok, ListenSocket} ->
            %% Spawn acceptor process (use spawn instead of spawn_link to avoid cascading failures)
            AcceptorPid = spawn(fun() -> acceptor_loop(ListenSocket) end),
            
            io:format("Meta-Server HTTP interface started on port ~p~n", [?HTTP_PORT]),
            
            {ok, {ListenSocket, AcceptorPid}};
        {error, eaddrinuse} ->
            %% Port already in use, try a different port
            AltPort = ?HTTP_PORT + 1000,
            io:format("Port ~p already in use, trying ~p~n", [?HTTP_PORT, AltPort]),
            case gen_tcp:listen(AltPort, [
                binary,
                {packet, http_bin},
                {active, false},
                {reuseaddr, true}
            ]) of
                {ok, ListenSocket} ->
                    AcceptorPid = spawn(fun() -> acceptor_loop(ListenSocket) end),
                    io:format("Meta-Server HTTP interface started on port ~p~n", [AltPort]),
                    {ok, {ListenSocket, AcceptorPid}};
                {error, Reason} ->
                    io:format("Failed to start Meta-Server HTTP interface: ~p~n", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            io:format("Failed to start Meta-Server HTTP interface: ~p~n", [Reason]),
            {error, Reason}
    end.

stop_http_server({ListenSocket, AcceptorPid}) ->
    exit(AcceptorPid, shutdown),
    gen_tcp:close(ListenSocket).

acceptor_loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            %% Spawn handler for this connection (unlinked to avoid crashes)
            HandlerPid = spawn(fun() -> handle_connection(Socket) end),
            gen_tcp:controlling_process(Socket, HandlerPid),
            acceptor_loop(ListenSocket);
        {error, closed} ->
            ok;
        {error, _Reason} ->
            timer:sleep(100),
            acceptor_loop(ListenSocket)
    end.

handle_connection(Socket) ->
    case gen_tcp:recv(Socket, 0, 5000) of
        {ok, {http_request, Method, {abs_path, Path}, _Version}} ->
            %% Read headers
            Headers = read_headers(Socket, []),
            
            %% Read body if present
            Body = case proplists:get_value(<<"content-length">>, Headers) of
                undefined -> <<>>;
                LengthBin ->
                    Length = binary_to_integer(LengthBin),
                    case gen_tcp:recv(Socket, Length, 5000) of
                        {ok, BodyData} -> BodyData;
                        _ -> <<>>
                    end
            end,
            
            %% Process request
            Response = handle_http_request(Method, Path, Headers, Body),
            
            %% Send response
            gen_tcp:send(Socket, Response),
            gen_tcp:close(Socket);
        _ ->
            gen_tcp:close(Socket)
    end.

read_headers(Socket, Acc) ->
    case gen_tcp:recv(Socket, 0, 5000) of
        {ok, {http_header, _, Name, _, Value}} ->
            read_headers(Socket, [{Name, Value} | Acc]);
        {ok, http_eoh} ->
            lists:reverse(Acc);
        _ ->
            lists:reverse(Acc)
    end.

handle_http_request('GET', "/meta/status", _Headers, _Body) ->
    case get_meta_status() of
        {ok, Status} ->
            create_json_response(200, Status);
        {error, Error} ->
            create_json_response(500, #{error => Error})
    end;

handle_http_request('GET', "/meta/errors", _Headers, _Body) ->
    case get_error_dashboard() of
        {ok, Dashboard} ->
            create_json_response(200, Dashboard);
        {error, Error} ->
            create_json_response(500, #{error => Error})
    end;

handle_http_request('GET', "/meta/feedback", _Headers, _Body) ->
    case get_feedback_summary() of
        {ok, Summary} ->
            create_json_response(200, Summary);
        {error, Error} ->
            create_json_response(500, #{error => Error})
    end;

handle_http_request('GET', "/meta/modifications", _Headers, _Body) ->
    case get_self_modification_log() of
        {ok, Log} ->
            create_json_response(200, Log);
        {error, Error} ->
            create_json_response(500, #{error => Error})
    end;

handle_http_request('GET', "/meta/health", _Headers, _Body) ->
    case get_system_health() of
        {ok, Health} ->
            create_json_response(200, Health);
        {error, Error} ->
            create_json_response(500, #{error => Error})
    end;

handle_http_request('POST', "/meta/command", _Headers, Body) ->
    try
        Command = jsx:decode(Body, [return_maps]),
        CommandType = maps:get(<<"command">>, Command),
        Parameters = maps:get(<<"parameters">>, Command, #{}),
        
        case execute_meta_command(CommandType, Parameters) of
            {ok, Result} ->
                create_json_response(200, #{result => Result});
            {error, Error} ->
                create_json_response(400, #{error => Error})
        end
    catch
        _:_ ->
            create_json_response(400, #{error => <<"Invalid JSON">>})
    end;

handle_http_request('GET', "/", _Headers, _Body) ->
    %% Serve a simple dashboard HTML
    create_html_response(200, create_dashboard_html());

handle_http_request(_, _, _, _) ->
    create_json_response(404, #{error => <<"Not found">>}).

create_json_response(StatusCode, Data) ->
    StatusText = case StatusCode of
        200 -> "OK";
        400 -> "Bad Request";
        404 -> "Not Found";
        500 -> "Internal Server Error"
    end,
    
    JsonData = jsx:encode(Data),
    
    [
        "HTTP/1.1 ", integer_to_list(StatusCode), " ", StatusText, "\r\n",
        "Content-Type: application/json\r\n",
        "Content-Length: ", integer_to_list(byte_size(JsonData)), "\r\n",
        "Access-Control-Allow-Origin: *\r\n",
        "\r\n",
        JsonData
    ].

create_html_response(StatusCode, Html) ->
    StatusText = case StatusCode of
        200 -> "OK";
        _ -> "Error"
    end,
    
    [
        "HTTP/1.1 ", integer_to_list(StatusCode), " ", StatusText, "\r\n",
        "Content-Type: text/html\r\n",
        "Content-Length: ", integer_to_list(length(Html)), "\r\n",
        "\r\n",
        Html
    ].

create_dashboard_html() ->
    "<!DOCTYPE html>
<html>
<head>
    <title>Meta-System Dashboard</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; background: #1a1a1a; color: #fff; }
        .container { max-width: 1200px; margin: 0 auto; }
        .panel { background: #2a2a2a; border: 1px solid #444; border-radius: 8px; 
                margin: 10px 0; padding: 20px; }
        .title { color: #00ff88; font-size: 24px; margin-bottom: 20px; }
        .status { display: inline-block; padding: 4px 12px; border-radius: 4px; 
                 margin: 2px; font-size: 12px; }
        .status.healthy { background: #0a4a0a; color: #0f0; }
        .status.warning { background: #4a4a0a; color: #ff0; }
        .status.critical { background: #4a0a0a; color: #f00; }
        button { background: #444; color: #fff; border: 1px solid #666; 
                padding: 8px 16px; border-radius: 4px; cursor: pointer; margin: 5px; }
        button:hover { background: #555; }
        .metric { margin: 10px 0; padding: 10px; background: #333; border-radius: 4px; }
        .grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 20px; }
    </style>
</head>
<body>
    <div class='container'>
        <h1 class='title'>🌟 Meta-System Dashboard</h1>
        
        <div class='grid'>
            <div class='panel'>
                <h2>System Status</h2>
                <div id='status-container'>Loading...</div>
                <button onclick='refreshStatus()'>Refresh</button>
            </div>
            
            <div class='panel'>
                <h2>Error Dashboard</h2>
                <div id='error-container'>Loading...</div>
                <button onclick='refreshErrors()'>Refresh</button>
            </div>
            
            <div class='panel'>
                <h2>Feedback Summary</h2>
                <div id='feedback-container'>Loading...</div>
                <button onclick='refreshFeedback()'>Refresh</button>
            </div>
            
            <div class='panel'>
                <h2>System Health</h2>
                <div id='health-container'>Loading...</div>
                <button onclick='refreshHealth()'>Refresh</button>
            </div>
        </div>
        
        <div class='panel'>
            <h2>Meta Commands</h2>
            <button onclick='executeCommand(\"analyze_patterns\")'>Analyze Patterns</button>
            <button onclick='executeCommand(\"optimize_systems\")'>Optimize Systems</button>
            <button onclick='executeCommand(\"generate_report\")'>Generate Report</button>
            <button onclick='executeCommand(\"emergency_healing\")'>Emergency Healing</button>
        </div>
    </div>
    
    <script>
        function refreshStatus() {
            fetch('/meta/status')
                .then(r => r.json())
                .then(data => {
                    document.getElementById('status-container').innerHTML = 
                        formatStatus(data);
                });
        }
        
        function refreshErrors() {
            fetch('/meta/errors')
                .then(r => r.json())
                .then(data => {
                    document.getElementById('error-container').innerHTML = 
                        formatErrors(data);
                });
        }
        
        function refreshFeedback() {
            fetch('/meta/feedback')
                .then(r => r.json())
                .then(data => {
                    document.getElementById('feedback-container').innerHTML = 
                        formatFeedback(data);
                });
        }
        
        function refreshHealth() {
            fetch('/meta/health')
                .then(r => r.json())
                .then(data => {
                    document.getElementById('health-container').innerHTML = 
                        formatHealth(data);
                });
        }
        
        function executeCommand(cmd) {
            fetch('/meta/command', {
                method: 'POST',
                headers: {'Content-Type': 'application/json'},
                body: JSON.stringify({command: cmd, parameters: {}})
            })
            .then(r => r.json())
            .then(data => alert('Command result: ' + JSON.stringify(data)));
        }
        
        function formatStatus(data) {
            return Object.entries(data).map(([key, value]) => 
                `<div class='metric'><strong>${key}:</strong> ${JSON.stringify(value)}</div>`
            ).join('');
        }
        
        function formatErrors(data) {
            if (!data.recent_errors) return 'No recent errors';
            return data.recent_errors.slice(0, 5).map(error => 
                `<div class='metric status critical'>${error.type}: ${error.message}</div>`
            ).join('');
        }
        
        function formatFeedback(data) {
            return `<div class='metric'>Total Feedback: ${data.total_count || 0}</div>
                   <div class='metric'>Error Rate: ${data.error_rate || 'N/A'}</div>
                   <div class='metric'>Success Rate: ${data.success_rate || 'N/A'}</div>`;
        }
        
        function formatHealth(data) {
            return Object.entries(data).map(([system, health]) => {
                const statusClass = health.status === 'healthy' ? 'healthy' : 
                                   health.status === 'warning' ? 'warning' : 'critical';
                return `<div class='status ${statusClass}'>${system}: ${health.status}</div>`;
            }).join('');
        }
        
        // Auto-refresh every 10 seconds
        setInterval(() => {
            refreshStatus();
            refreshErrors();
            refreshFeedback();
            refreshHealth();
        }, 10000);
        
        // Initial load
        refreshStatus();
        refreshErrors();
        refreshFeedback();
        refreshHealth();
    </script>
</body>
</html>".

update_all_caches(State) ->
    %% Update meta status cache
    MetaStatus = fetch_meta_status(),
    
    %% Update error cache
    ErrorData = fetch_error_data(),
    
    %% Update feedback cache
    FeedbackData = fetch_feedback_data(),
    
    %% Update modification cache
    ModificationData = fetch_modification_data(),
    
    State#state{
        meta_status_cache = MetaStatus,
        error_cache = ErrorData,
        feedback_cache = FeedbackData,
        modification_cache = ModificationData
    }.

fetch_meta_status() ->
    %% Fetch status from all meta-systems
    Status = #{
        coordinator_status => get_coordinator_status(),
        monitor_status => get_monitor_status(),
        feedback_status => get_feedback_system_status(),
        intelligence_status => get_intelligence_status(),
        timestamp => erlang:system_time(millisecond)
    },
    Status.

get_coordinator_status() ->
    case erlang:whereis(meta_layer_coordinator) of
        undefined -> #{status => down};
        _Pid ->
            case catch meta_layer_coordinator:get_meta_insights() of
                {ok, Insights} -> #{status => running, insights_count => maps:size(Insights)};
                _ -> #{status => error}
            end
    end.

get_monitor_status() ->
    case erlang:whereis(meta_meta_monitor) of
        undefined -> #{status => down};
        _Pid ->
            case catch meta_meta_monitor:analyze_meta_performance() of
                {ok, Analysis} -> #{status => running, analysis => Analysis};
                _ -> #{status => error}
            end
    end.

get_feedback_system_status() ->
    case erlang:whereis(unified_feedback_system) of
        undefined -> #{status => down};
        _Pid ->
            case catch unified_feedback_system:get_feedback_summary() of
                {ok, Summary} -> #{status => running, summary => Summary};
                _ -> #{status => error}
            end
    end.

get_intelligence_status() ->
    case erlang:whereis(cross_system_intelligence) of
        undefined -> #{status => down};
        _Pid ->
            case catch cross_system_intelligence:get_collective_intelligence() of
                {ok, Intelligence} -> #{status => running, intelligence => Intelligence};
                _ -> #{status => error}
            end
    end.

fetch_error_data() ->
    %% Fetch recent errors from unified feedback system
    case erlang:whereis(unified_feedback_system) of
        undefined -> [];
        _Pid ->
            case catch unified_feedback_system:get_feedback_summary(error) of
                {ok, #{recent := Recent}} -> Recent;
                _ -> []
            end
    end.

fetch_feedback_data() ->
    %% Fetch recent feedback data
    case erlang:whereis(unified_feedback_system) of
        undefined -> #{};
        _Pid ->
            case catch unified_feedback_system:get_feedback_summary() of
                {ok, Summary} -> Summary;
                _ -> #{}
            end
    end.

fetch_modification_data() ->
    %% Fetch recent modification data
    case erlang:whereis(hot_code_reloader) of
        undefined -> [];
        _Pid ->
            %% Would fetch from hot_code_reloader if it had this interface
            []
    end.

compile_meta_status(State) ->
    State#state.meta_status_cache.

compile_error_dashboard(State) ->
    #{
        recent_errors => lists:sublist(State#state.error_cache, 10),
        error_count => length(State#state.error_cache),
        error_rate => calculate_error_rate(State#state.error_cache)
    }.

compile_feedback_summary(State) ->
    %% Handle both legacy map format and new list format
    case State#state.feedback_cache of
        Cache when is_list(Cache) -> 
            #{recent_feedback => Cache, count => length(Cache)};
        Cache when is_map(Cache) -> 
            Cache;  % Legacy map format
        _ -> 
            #{recent_feedback => [], count => 0}
    end.

compile_modification_log(State) ->
    #{
        recent_modifications => lists:sublist(State#state.modification_cache, 20),
        modification_count => length(State#state.modification_cache)
    }.

compile_system_health(State) ->
    %% Compile overall system health from cached data
    MetaStatus = State#state.meta_status_cache,
    
    Health = maps:fold(fun(SystemName, Status, Acc) ->
        HealthStatus = case maps:get(status, Status, unknown) of
            running -> healthy;
            down -> critical;
            error -> warning;
            _ -> unknown
        end,
        maps:put(SystemName, #{status => HealthStatus}, Acc)
    end, #{}, MetaStatus),
    
    Health.

execute_meta_command_impl(<<"analyze_patterns">>, _Parameters) ->
    case erlang:whereis(cross_system_intelligence) of
        undefined -> {error, <<"Intelligence system not available">>};
        _Pid ->
            case catch cross_system_intelligence:synthesize_knowledge() of
                {ok, Synthesis} -> {ok, Synthesis};
                Error -> {error, Error}
            end
    end;

execute_meta_command_impl(<<"optimize_systems">>, _Parameters) ->
    case erlang:whereis(meta_meta_monitor) of
        undefined -> {error, <<"Monitor system not available">>};
        _Pid ->
            case catch meta_meta_monitor:optimize_meta_systems() of
                {ok, Optimizations} -> {ok, Optimizations};
                Error -> {error, Error}
            end
    end;

execute_meta_command_impl(<<"generate_report">>, _Parameters) ->
    case erlang:whereis(meta_meta_monitor) of
        undefined -> {error, <<"Monitor system not available">>};
        _Pid ->
            case catch meta_meta_monitor:get_meta_health_report() of
                {ok, Report} -> {ok, Report};
                Error -> {error, Error}
            end
    end;

execute_meta_command_impl(<<"emergency_healing">>, _Parameters) ->
    case erlang:whereis(meta_layer_coordinator) of
        undefined -> {error, <<"Coordinator not available">>};
        _Pid ->
            %% Trigger emergency healing across all systems
            meta_layer_coordinator:coordinate_cross_system_action(
                emergency_healing,
                #{priority => critical, scope => all_systems}
            ),
            {ok, <<"Emergency healing initiated">>}
    end;

execute_meta_command_impl(Command, _Parameters) ->
    {error, iolist_to_binary(io_lib:format("Unknown command: ~p", [Command]))}.

broadcast_to_subscribers(State) ->
    case State#state.subscribers of
        [] -> ok;
        Subscribers ->
            Status = compile_meta_status(State),
            lists:foreach(fun(Pid) ->
                send_status_update(Pid, Status)
            end, Subscribers)
    end.

send_status_update(Pid, StatusOrState) ->
    Status = case StatusOrState of
        #state{} = State -> compile_meta_status(State);
        StatusData -> StatusData
    end,
    
    Pid ! {meta_status_update, Status}.

broadcast_meta_event(EventType, EventData, State) ->
    case State#state.subscribers of
        [] -> ok;
        Subscribers ->
            Event = #{
                type => EventType,
                data => EventData,
                timestamp => erlang:system_time(millisecond)
            },
            lists:foreach(fun(Pid) ->
                Pid ! {meta_event, Event}
            end, Subscribers)
    end.

handle_meta_event(error_recovery, EventData, State) ->
    %% Add to error cache
    ErrorEntry = #{
        type => error_recovery,
        data => EventData,
        timestamp => erlang:system_time(millisecond)
    },
    
    NewErrorCache = [ErrorEntry | lists:sublist(State#state.error_cache, 99)],
    State#state{error_cache = NewErrorCache};

handle_meta_event(feedback_analysis_complete, EventData, State) ->
    %% Update feedback cache
    FeedbackEntry = #{
        type => feedback_analysis,
        data => EventData,
        timestamp => erlang:system_time(millisecond)
    },
    
    %% Ensure feedback_cache is a list
    ExistingCache = case State#state.feedback_cache of
        Cache when is_list(Cache) -> Cache;
        _ -> []  % If it's not a list (e.g., a map), start with empty list
    end,
    NewFeedbackCache = [FeedbackEntry | lists:sublist(ExistingCache, 99)],
    State#state{feedback_cache = NewFeedbackCache};

handle_meta_event(_, _, State) ->
    State.

calculate_error_rate(ErrorCache) ->
    case length(ErrorCache) of
        0 -> 0.0;
        _N ->
            %% Calculate errors per hour (simplified)
            Now = erlang:system_time(millisecond),
            OneHourAgo = Now - (60 * 60 * 1000),
            
            RecentErrors = length([E || E = #{timestamp := T} <- ErrorCache, T > OneHourAgo]),
            RecentErrors / 1.0  % errors per hour
    end.

%% For HTTP request handling export
handle_http_request(Method, Path) ->
    handle_http_request(Method, Path, [], <<>>).