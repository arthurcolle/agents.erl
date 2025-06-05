%%%-------------------------------------------------------------------
%%% @doc Cost Tracking API Handler
%%% Provides REST API endpoints for cost tracking and reporting
%%% @end
%%%-------------------------------------------------------------------
-module(cost_tracking_handler).

-export([
    init/2,
    allowed_methods/2,
    content_types_provided/2,
    content_types_accepted/2,
    handle_get/2,
    handle_post/2,
    handle_delete/2
]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, handle_get}
    ], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {<<"application/json">>, handle_post}
    ], Req, State}.

handle_get(Req, State) ->
    Path = cowboy_req:path_info(Req),
    QS = cowboy_req:parse_qs(Req),
    
    Response = case Path of
        [] ->
            %% Get overall cost summary
            Period = get_period_from_qs(QS),
            get_cost_summary(Period);
            
        [<<"summary">>] ->
            %% Get detailed summary
            Period = get_period_from_qs(QS),
            get_detailed_summary(Period);
            
        [<<"agent">>, AgentId] ->
            %% Get costs for specific agent
            get_agent_costs(AgentId);
            
        [<<"model">>, Model] ->
            %% Get costs for specific model
            get_model_costs(Model);
            
        [<<"timerange">>] ->
            %% Get costs for time range
            StartTime = get_timestamp_from_qs(QS, <<"start">>, 0),
            EndTime = get_timestamp_from_qs(QS, <<"end">>, erlang:system_time(second)),
            get_timerange_costs(StartTime, EndTime);
            
        [<<"report">>] ->
            %% Generate comprehensive report with pagination
            generate_cost_report(Req, QS);
            
        _ ->
            #{error => <<"Invalid endpoint">>}
    end,
    
    {jsx:encode(Response), Req, State}.

handle_post(Req, State) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    
    try
        Data = jsx:decode(Body, [return_maps]),
        
        %% Manual cost tracking entry
        AgentId = maps:get(<<"agent_id">>, Data),
        Model = maps:get(<<"model">>, Data),
        Usage = maps:get(<<"usage">>, Data),
        Metadata = maps:get(<<"metadata">>, Data, #{}),
        
        cost_tracker:track_usage(AgentId, Model, Usage, Metadata),
        
        Response = #{
            status => <<"success">>,
            message => <<"Cost tracked successfully">>
        },
        
        {jsx:encode(Response), Req2, State}
    catch
        error:Reason ->
            ErrorResponse = #{
                error => <<"Invalid request">>,
                reason => list_to_binary(io_lib:format("~p", [Reason]))
            },
            {jsx:encode(ErrorResponse), Req2, State}
    end.

handle_delete(Req, State) ->
    case cowboy_req:path_info(Req) of
        [<<"reset">>] ->
            cost_tracker:reset_costs(),
            Response = #{
                status => <<"success">>,
                message => <<"Cost tracking reset">>
            },
            {jsx:encode(Response), Req, State};
        _ ->
            Response = #{error => <<"Invalid endpoint">>},
            {jsx:encode(Response), Req, State}
    end.

%% Internal functions

get_cost_summary(Period) ->
    case Period of
        undefined ->
            case cost_tracker:get_cost_summary() of
                {ok, Summary} -> Summary;
                {error, Reason} -> #{error => Reason}
            end;
        P ->
            case cost_tracker:get_cost_summary(P) of
                {ok, Summary} -> Summary;
                {error, Reason} -> #{error => Reason}
            end
    end.

get_detailed_summary(Period) ->
    case get_cost_summary(Period) of
        #{error := _} = Error -> Error;
        Summary ->
            %% Enhance with additional calculations
            TotalCost = maps:get(total_cost, Summary, 0.0),
            TotalCalls = maps:get(total_calls, Summary, 0),
            AvgCost = maps:get(average_cost_per_call, Summary, 0.0),
            
            %% Calculate cost projections
            HourlyRate = calculate_hourly_rate(Summary, Period),
            DailyProjection = HourlyRate * 24,
            MonthlyProjection = DailyProjection * 30,
            
            Summary#{
                projections => #{
                    hourly_rate => HourlyRate,
                    daily_projection => DailyProjection,
                    monthly_projection => MonthlyProjection
                },
                formatting => #{
                    total_cost_formatted => format_currency(TotalCost),
                    avg_cost_formatted => format_currency(AvgCost),
                    daily_projection_formatted => format_currency(DailyProjection),
                    monthly_projection_formatted => format_currency(MonthlyProjection)
                }
            }
    end.

get_agent_costs(AgentId) ->
    case cost_tracker:get_agent_costs(AgentId) of
        {ok, Summary} -> Summary;
        {error, Reason} -> #{error => Reason}
    end.

get_model_costs(Model) ->
    case cost_tracker:get_model_costs(Model) of
        {ok, Summary} -> Summary;
        {error, Reason} -> #{error => Reason}
    end.

get_timerange_costs(StartTime, EndTime) ->
    case cost_tracker:get_costs_by_timerange(StartTime, EndTime) of
        {ok, Summary} -> Summary;
        {error, Reason} -> #{error => Reason}
    end.

generate_cost_report(Req, QS) ->
    Period = get_period_from_qs(QS),
    PaginationParams = pagination_utils:parse_pagination_params(Req),
    #{page := _Page, page_size := _PageSize, offset := Offset} = PaginationParams,
    
    %% Get comprehensive data
    {ok, Summary} = cost_tracker:get_cost_summary(Period),
    
    %% Get top spenders and models with pagination
    TopSpenders = get_top_spenders(maps:get(agent_breakdown, Summary, #{}), 100), % Get more for pagination
    MostUsedModels = get_most_used(maps:get(model_breakdown, Summary, #{}), 100), % Get more for pagination
    
    %% Apply pagination to top spenders
    {PagedSpenders, SpendersMetadata} = pagination_utils:paginate_list(
        TopSpenders, 
        Offset, 
        maps:get(page_size, PaginationParams)
    ),
    
    %% Apply pagination to models (offset by spenders)
    {PagedModels, ModelsMetadata} = pagination_utils:paginate_list(
        MostUsedModels, 
        Offset, 
        maps:get(page_size, PaginationParams)
    ),
    
    %% Build detailed report
    BaseReport = #{
        report_generated => iso8601_timestamp(),
        period => case Period of
            undefined -> <<"all_time">>;
            P -> list_to_binary(integer_to_list(P) ++ " seconds")
        end,
        summary => Summary,
        cost_trends => calculate_cost_trends(Summary),
        recommendations => generate_cost_recommendations(Summary)
    },
    
    %% Format with pagination
    pagination_utils:format_pagination_response(
        #{
            <<"top_spending_agents">> => PagedSpenders,
            <<"most_used_models">> => PagedModels
        },
        SpendersMetadata, % Use spenders metadata as primary
        <<"data">>,
        BaseReport
    ).

get_period_from_qs(QS) ->
    case lists:keyfind(<<"period">>, 1, QS) of
        {_, Value} -> binary_to_integer(Value);
        false -> undefined
    end.

get_timestamp_from_qs(QS, Key, Default) ->
    case lists:keyfind(Key, 1, QS) of
        {_, Value} -> binary_to_integer(Value);
        false -> Default
    end.

calculate_hourly_rate(Summary, undefined) ->
    %% All time - use average over total period
    0.0;  % Would need start time tracking
calculate_hourly_rate(Summary, Period) ->
    TotalCost = maps:get(total_cost, Summary, 0.0),
    Hours = Period / 3600,
    case Hours of
        0 -> 0.0;
        H -> TotalCost / H
    end.

format_currency(Amount) ->
    list_to_binary(io_lib:format("$~.2f", [Amount])).

get_top_spenders(AgentBreakdown, Limit) ->
    Sorted = lists:sort(fun({_, A}, {_, B}) ->
        maps:get(total_cost, A, 0.0) > maps:get(total_cost, B, 0.0)
    end, maps:to_list(AgentBreakdown)),
    
    lists:map(fun({AgentId, Data}) ->
        #{
            agent_id => AgentId,
            total_cost => maps:get(total_cost, Data, 0.0),
            total_calls => maps:get(calls, Data, 0),
            formatted_cost => format_currency(maps:get(total_cost, Data, 0.0))
        }
    end, lists:sublist(Sorted, Limit)).

get_most_used(ModelBreakdown, Limit) ->
    Sorted = lists:sort(fun({_, A}, {_, B}) ->
        maps:get(calls, A, 0) > maps:get(calls, B, 0)
    end, maps:to_list(ModelBreakdown)),
    
    lists:map(fun({Model, Data}) ->
        #{
            model => Model,
            total_cost => maps:get(total_cost, Data, 0.0),
            total_calls => maps:get(calls, Data, 0),
            input_tokens => maps:get(input_tokens, Data, 0),
            output_tokens => maps:get(output_tokens, Data, 0),
            formatted_cost => format_currency(maps:get(total_cost, Data, 0.0))
        }
    end, lists:sublist(Sorted, Limit)).

calculate_cost_trends(_Summary) ->
    %% Placeholder for trend calculation
    %% Would need historical data storage
    #{
        trend => <<"stable">>,
        change_percentage => 0.0
    }.

generate_cost_recommendations(Summary) ->
    TotalCost = maps:get(total_cost, Summary, 0.0),
    ModelBreakdown = maps:get(model_breakdown, Summary, #{}),
    
    Recommendations = [],
    
    %% Check if using expensive models for simple tasks
    R1 = case maps:is_key(<<"gpt-4.1">>, ModelBreakdown) andalso 
              maps:is_key(<<"gpt-4.1-mini">>, ModelBreakdown) of
        true ->
            Gpt41Calls = maps:get(calls, maps:get(<<"gpt-4.1">>, ModelBreakdown, #{}), 0),
            case Gpt41Calls > 100 of
                true -> [<<"Consider using gpt-4.1-mini for simpler tasks to reduce costs">> | Recommendations];
                false -> Recommendations
            end;
        false -> Recommendations
    end,
    
    %% Check for high reasoning model usage
    R2 = case maps:is_key(<<"o3">>, ModelBreakdown) orelse 
              maps:is_key(<<"o1-pro">>, ModelBreakdown) of
        true ->
            [<<"High-cost reasoning models detected. Use only for complex reasoning tasks">> | R1];
        false -> R1
    end,
    
    %% Add caching recommendation if high token usage
    TotalTokens = maps:get(total_input_tokens, Summary, 0) + 
                  maps:get(total_output_tokens, Summary, 0),
    R3 = case TotalTokens > 1000000 of
        true -> [<<"Consider implementing response caching for repeated queries">> | R2];
        false -> R2
    end,
    
    R3.

iso8601_timestamp() ->
    {{Y,M,D},{H,Min,S}} = calendar:now_to_datetime(erlang:timestamp()),
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                                [Y, M, D, H, Min, S])).