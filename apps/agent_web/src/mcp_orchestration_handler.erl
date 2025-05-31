%%%-------------------------------------------------------------------
%%% @doc
%%% HTTP handler for MCP Orchestration Engine API
%%% @end
%%%-------------------------------------------------------------------
-module(mcp_orchestration_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path_info(Req0),
    handle_request(Method, Path, Req0, State).

handle_request(<<"GET">>, [<<"servers">>], Req0, State) ->
    case mcp_orchestration_engine:get_servers() of
        {ok, Servers} ->
            Response = jsx:encode(#{servers => Servers}),
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                Response,
                Req0),
            {ok, Req, State};
        {error, Reason} ->
            error_response(500, Reason, Req0, State)
    end;

handle_request(<<"GET">>, [<<"workflows">>], Req0, State) ->
    case mcp_orchestration_engine:get_workflows() of
        {ok, Workflows} ->
            Response = jsx:encode(#{workflows => Workflows}),
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                Response,
                Req0),
            {ok, Req, State};
        {error, Reason} ->
            error_response(500, Reason, Req0, State)
    end;

handle_request(<<"GET">>, [<<"metrics">>], Req0, State) ->
    %% Return real-time metrics for all servers
    MockMetrics = #{
        <<"server_1">> => #{
            latency_p50 => 85,
            latency_p95 => 200,
            latency_p99 => 350,
            throughput_rps => 850,
            error_rate => 0.02,
            cpu_usage => 45,
            memory_usage => 62,
            network_io => 25,
            concurrent_connections => 120
        },
        <<"server_2">> => #{
            latency_p50 => 92,
            latency_p95 => 220,
            latency_p99 => 380,
            throughput_rps => 720,
            error_rate => 0.015,
            cpu_usage => 38,
            memory_usage => 55,
            network_io => 28,
            concurrent_connections => 95
        }
    },
    Response = jsx:encode(MockMetrics),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Response,
        Req0),
    {ok, Req, State};

handle_request(<<"POST">>, [<<"analyze">>], Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    case jsx:decode(Body, [return_maps]) of
        #{<<"servers">> := ServerIds, <<"analysis_type">> := AnalysisType} ->
            Criteria = #{type => AnalysisType, servers => ServerIds},
            case mcp_orchestration_engine:analyze_servers(Criteria) of
                {ok, Analysis} ->
                    Response = jsx:encode(Analysis),
                    Req = cowboy_req:reply(200,
                        #{<<"content-type">> => <<"application/json">>},
                        Response,
                        Req1),
                    {ok, Req, State};
                {error, Reason} ->
                    error_response(500, Reason, Req1, State)
            end;
        _ ->
            error_response(400, <<"Invalid request body">>, Req1, State)
    end;

handle_request(<<"POST">>, [<<"workflows">>], Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    case jsx:decode(Body, [return_maps]) of
        #{<<"capabilities">> := Capabilities} = RequestBody ->
            Options = maps:without([<<"capabilities">>], RequestBody),
            case mcp_orchestration_engine:create_workflow(Capabilities, Options) of
                {ok, Workflow} ->
                    Response = jsx:encode(Workflow),
                    Req = cowboy_req:reply(201,
                        #{<<"content-type">> => <<"application/json">>},
                        Response,
                        Req1),
                    {ok, Req, State};
                {error, Reason} ->
                    error_response(500, Reason, Req1, State)
            end;
        _ ->
            error_response(400, <<"Invalid request body">>, Req1, State)
    end;

handle_request(<<"POST">>, [<<"optimize">>], Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    case jsx:decode(Body, [return_maps]) of
        #{<<"mode">> := Mode} ->
            Strategy = case Mode of
                <<"ai_assisted">> -> load_balancing;
                <<"autonomous">> -> performance_optimization;
                _ -> load_balancing
            end,
            case mcp_orchestration_engine:optimize_allocation(Strategy) of
                {ok, _Result} ->
                    Response = jsx:encode(#{status => <<"optimization_applied">>}),
                    Req = cowboy_req:reply(200,
                        #{<<"content-type">> => <<"application/json">>},
                        Response,
                        Req1),
                    {ok, Req, State};
                {error, Reason} ->
                    error_response(500, Reason, Req1, State)
            end;
        _ ->
            error_response(400, <<"Invalid request body">>, Req1, State)
    end;

handle_request(<<"POST">>, [<<"semantic-search">>], Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    case jsx:decode(Body, [return_maps]) of
        #{<<"query">> := Query, <<"servers">> := ServerIds} ->
            Context = #{servers => ServerIds},
            case mcp_orchestration_engine:semantic_search(Query, Context) of
                {ok, Results} ->
                    Response = jsx:encode(#{
                        relevant_servers => [maps:get(server_id, R) || R <- Results],
                        results => Results
                    }),
                    Req = cowboy_req:reply(200,
                        #{<<"content-type">> => <<"application/json">>},
                        Response,
                        Req1),
                    {ok, Req, State};
                {error, Reason} ->
                    error_response(500, Reason, Req1, State)
            end;
        _ ->
            error_response(400, <<"Invalid request body">>, Req1, State)
    end;

handle_request(<<"GET">>, [<<"recommendations">>, Type], Req0, State) ->
    RecommendationType = case Type of
        <<"servers">> -> server_recommendations;
        <<"capabilities">> -> capability_compositions;
        <<"optimization">> -> optimization_suggestions;
        <<"insights">> -> predictive_insights;
        _ -> server_recommendations
    end,
    
    case mcp_orchestration_engine:get_recommendations(RecommendationType) of
        {ok, Recommendations} ->
            Response = jsx:encode(#{recommendations => Recommendations}),
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                Response,
                Req0),
            {ok, Req, State};
        {error, Reason} ->
            error_response(500, Reason, Req0, State)
    end;

handle_request(<<"GET">>, [<<"servers">>, _ServerId, <<"health">>], Req0, State) ->
    %% Mock health check response
    MockHealth = #{
        status => <<"healthy">>,
        uptime => 3600 * 24 * 7, % 7 days
        last_check => calendar:datetime_to_gregorian_seconds(calendar:local_time()),
        error_count => 0,
        success_rate => 99.5
    },
    Response = jsx:encode(MockHealth),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Response,
        Req0),
    {ok, Req, State};

handle_request(<<"POST">>, [<<"servers">>, ServerId, <<"test">>], Req0, State) ->
    %% Mock capability testing
    MockTestResults = #{
        server_id => ServerId,
        test_timestamp => calendar:datetime_to_gregorian_seconds(calendar:local_time()),
        capabilities_tested => 5,
        successful_tests => 5,
        failed_tests => 0,
        average_response_time => 120,
        results => [
            #{
                capability => <<"text_processing">>,
                status => <<"passed">>,
                response_time => 95,
                accuracy => 0.98
            },
            #{
                capability => <<"data_analysis">>,
                status => <<"passed">>,
                response_time => 145,
                accuracy => 0.95
            },
            #{
                capability => <<"api_integration">>,
                status => <<"passed">>,
                response_time => 85,
                accuracy => 0.99
            }
        ]
    },
    Response = jsx:encode(MockTestResults),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Response,
        Req0),
    {ok, Req, State};

handle_request(_, _, Req0, State) ->
    error_response(404, <<"Not found">>, Req0, State).

error_response(Code, Message, Req0, State) ->
    ErrorMsg = case is_binary(Message) of
        true -> Message;
        false -> atom_to_binary(Message, utf8)
    end,
    Response = jsx:encode(#{error => ErrorMsg}),
    Req = cowboy_req:reply(Code,
        #{<<"content-type">> => <<"application/json">>},
        Response,
        Req0),
    {ok, Req, State}.