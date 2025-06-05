-module(test_self_scaffold_integration).
-export([run/0]).

run() ->
    io:format("=== Self-Scaffolding Integration Test ===~n~n"),
    
    % Start the supervisor tree
    ensure_started(kernel),
    ensure_started(stdlib),
    ensure_started(crypto),
    ensure_started(asn1),
    ensure_started(public_key),
    ensure_started(ssl),
    ensure_started(inets),
    ensure_started(jsx),
    ensure_started(yamerl),
    ensure_started(sqlite3),
    
    io:format("Starting self-scaffolding supervisor...~n"),
    case self_scaffold_sup:start_link() of
        {ok, Pid} ->
            io:format("✓ Self-scaffolding system started: ~p~n", [Pid]),
            
            % Wait for initial discovery
            timer:sleep(6000),
            
            % Check what happened
            check_system_status(),
            
            % Force an update
            force_update_test(),
            
            % Test endpoint discovery from real API
            test_real_api_discovery(),
            
            io:format("~n✅ Integration test complete!~n");
        {error, Reason} ->
            io:format("✗ Failed to start self-scaffolding system: ~p~n", [Reason])
    end.

ensure_started(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, _}} -> ok;
        {error, Reason} -> 
            io:format("Failed to start ~p: ~p~n", [App, Reason]),
            ok
    end.

check_system_status() ->
    io:format("~n--- System Status Check ---~n"),
    
    % Check scheduler
    ScheduleInfo = scaffold_scheduler:get_schedule_info(),
    io:format("Scheduler info: ~p~n", [ScheduleInfo]),
    
    % Check discovery status
    DiscoveryStatus = endpoint_discovery:get_discovery_status(),
    io:format("Discovery status: ~p~n", [DiscoveryStatus]),
    
    % Check registered endpoints
    Endpoints = endpoint_registry:get_all_endpoints(),
    io:format("Registered endpoints: ~p~n", [length(Endpoints)]).

force_update_test() ->
    io:format("~n--- Force Update Test ---~n"),
    
    io:format("Forcing immediate update...~n"),
    scaffold_scheduler:force_update_now(),
    
    % Wait for updates to process
    timer:sleep(3000),
    
    io:format("Update forced successfully~n").

test_real_api_discovery() ->
    io:format("~n--- Real API Discovery Test ---~n"),
    
    % Add a simple test API
    openapi_fetcher:add_schema_source(
        jsonplaceholder, 
        "https://raw.githubusercontent.com/typicode/jsonplaceholder/master/public/swagger.json"
    ),
    
    io:format("Discovering endpoints from test API...~n"),
    case endpoint_discovery:discover_endpoints(jsonplaceholder) of
        {ok, Count} ->
            io:format("✓ Discovered ~p endpoints~n", [Count]),
            
            % Show some discovered endpoints
            AllEndpoints = endpoint_registry:get_all_endpoints(),
            io:format("~nSample endpoints:~n"),
            lists:foreach(fun(Endpoint) ->
                Method = element(2, Endpoint),
                Path = element(3, Endpoint),
                Summary = element(5, Endpoint),
                io:format("  ~s ~s - ~s~n", [Method, Path, Summary])
            end, lists:sublist(AllEndpoints, 5));
        {error, Reason} ->
            io:format("ℹ API discovery failed (this is expected if offline): ~p~n", [Reason])
    end.

% Helper to show hourly update behavior
demonstrate_hourly_updates() ->
    io:format("~n--- Hourly Update Demonstration ---~n"),
    io:format("The system is configured to update every hour.~n"),
    io:format("Current schedules:~n"),
    
    ScheduleInfo = scaffold_scheduler:get_schedule_info(),
    maps:foreach(fun(Name, Schedule) ->
        IntervalMs = maps:get(interval, Schedule),
        IntervalMins = IntervalMs div 60000,
        io:format("  - ~p: Updates every ~p minutes~n", [Name, IntervalMins])
    end, maps:get(schedules, ScheduleInfo)),
    
    io:format("~nTo change update interval to 5 minutes:~n"),
    io:format("  scaffold_scheduler:set_update_interval(300000).~n"),
    io:format("  endpoint_discovery:set_discovery_interval(300000).~n").