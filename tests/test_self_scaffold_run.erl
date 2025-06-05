-module(test_self_scaffold_run).
-export([run/0]).

run() ->
    io:format("=== Testing Self-Scaffolding System ===~n~n"),
    
    % Start necessary applications
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
    
    % Test each component individually
    test_endpoint_registry(),
    test_openapi_fetcher(),
    test_endpoint_discovery(),
    test_scaffold_scheduler(),
    
    io:format("~n=== All Tests Complete ===~n").

ensure_started(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, _}} -> ok;
        {error, Reason} -> 
            io:format("Failed to start ~p: ~p~n", [App, Reason]),
            ok
    end.

test_endpoint_registry() ->
    io:format("~n1. Testing Endpoint Registry...~n"),
    
    case endpoint_registry:start_link() of
        {ok, Pid} ->
            io:format("  ✓ Started endpoint registry: ~p~n", [Pid]),
            
            % Test registering an endpoint
            TestEndpoint = #{
                method => <<"GET">>,
                path => <<"/test/endpoint">>,
                operation_id => <<"testOp">>,
                summary => <<"Test endpoint">>,
                tag => <<"test">>,
                parameters => [],
                request_schema => #{},
                response_schema => #{}
            },
            
            ok = endpoint_registry:register_endpoint(TestEndpoint),
            io:format("  ✓ Registered test endpoint~n"),
            
            % Test retrieving endpoint
            case endpoint_registry:get_endpoint(<<"GET">>, <<"/test/endpoint">>) of
                {ok, _} ->
                    io:format("  ✓ Retrieved endpoint successfully~n");
                Error ->
                    io:format("  ✗ Failed to retrieve endpoint: ~p~n", [Error])
            end,
            
            % Test getting all endpoints
            Endpoints = endpoint_registry:get_all_endpoints(),
            io:format("  ✓ Retrieved ~p endpoints~n", [length(Endpoints)]),
            
            gen_server:stop(Pid);
        {error, {already_started, Pid}} ->
            io:format("  ℹ Endpoint registry already running: ~p~n", [Pid]),
            gen_server:stop(Pid);
        {error, Reason} ->
            io:format("  ✗ Failed to start endpoint registry: ~p~n", [Reason])
    end.

test_openapi_fetcher() ->
    io:format("~n2. Testing OpenAPI Fetcher...~n"),
    
    case openapi_fetcher:start_link() of
        {ok, Pid} ->
            io:format("  ✓ Started OpenAPI fetcher: ~p~n", [Pid]),
            
            % Test adding a schema source
            openapi_fetcher:add_schema_source(test_api, "https://petstore.swagger.io/v2/swagger.json"),
            io:format("  ✓ Added test schema source~n"),
            
            % Test parsing a simple schema
            TestYaml = <<"openapi: 3.0.0
info:
  title: Test API
  version: 1.0.0
paths:
  /test:
    get:
      summary: Test endpoint
      operationId: getTest
      responses:
        '200':
          description: Success
">>,
            
            case openapi_fetcher:parse_schema(TestYaml) of
                {ok, Schema} ->
                    io:format("  ✓ Parsed test schema successfully~n"),
                    Paths = maps:get(paths, Schema, #{}),
                    io:format("  ✓ Found ~p paths in schema~n", [maps:size(Paths)]);
                {error, Reason} ->
                    io:format("  ✗ Failed to parse schema: ~p~n", [Reason])
            end,
            
            gen_server:stop(Pid);
        {error, {already_started, Pid}} ->
            io:format("  ℹ OpenAPI fetcher already running: ~p~n", [Pid]),
            gen_server:stop(Pid);
        {error, Reason} ->
            io:format("  ✗ Failed to start OpenAPI fetcher: ~p~n", [Reason])
    end.

test_endpoint_discovery() ->
    io:format("~n3. Testing Endpoint Discovery...~n"),
    
    % Ensure dependencies are started
    case endpoint_registry:start_link() of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    case openapi_fetcher:start_link() of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    
    case endpoint_discovery:start_link() of
        {ok, Pid} ->
            io:format("  ✓ Started endpoint discovery: ~p~n", [Pid]),
            
            % Check discovery status
            Status = endpoint_discovery:get_discovery_status(),
            io:format("  ✓ Discovery status: ~p~n", [maps:get(status, Status)]),
            
            % Set discovery interval to 5 minutes for testing
            endpoint_discovery:set_discovery_interval(300000),
            io:format("  ✓ Set discovery interval to 5 minutes~n"),
            
            gen_server:stop(Pid);
        {error, {already_started, Pid}} ->
            io:format("  ℹ Endpoint discovery already running: ~p~n", [Pid]),
            gen_server:stop(Pid);
        {error, Reason} ->
            io:format("  ✗ Failed to start endpoint discovery: ~p~n", [Reason])
    end.

test_scaffold_scheduler() ->
    io:format("~n4. Testing Scaffold Scheduler...~n"),
    
    case scaffold_scheduler:start_link() of
        {ok, Pid} ->
            io:format("  ✓ Started scaffold scheduler: ~p~n", [Pid]),
            
            % Get schedule info
            Info = scaffold_scheduler:get_schedule_info(),
            io:format("  ✓ Schedule info: ~p~n", [Info]),
            
            % Set update interval
            scaffold_scheduler:set_update_interval(3600000), % 1 hour
            io:format("  ✓ Set update interval to 1 hour~n"),
            
            gen_server:stop(Pid);
        {error, {already_started, Pid}} ->
            io:format("  ℹ Scaffold scheduler already running: ~p~n", [Pid]),
            gen_server:stop(Pid);
        {error, Reason} ->
            io:format("  ✗ Failed to start scaffold scheduler: ~p~n", [Reason])
    end.