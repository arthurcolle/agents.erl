-module(agent_monitoring_handler).

-export([init/2]).

init(Req0, State) ->
    Path = cowboy_req:path_info(Req0),
    Method = cowboy_req:method(Req0),
    handle_request(Method, Path, Req0, State).

handle_request(<<"GET">>, [<<"metrics">>], Req0, State) ->
    % Get overall system metrics
    Metrics = collect_all_metrics(),
    Response = jsx:encode(Metrics),
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, Response, Req0),
    {ok, Req, State};

handle_request(<<"GET">>, [<<"agents">>, AgentId, <<"metrics">>], Req0, State) ->
    % Get metrics for specific agent
    case get_agent_detailed_metrics(binary_to_list(AgentId)) of
        {ok, Metrics} ->
            Response = jsx:encode(Metrics),
            Req = cowboy_req:reply(200, #{
                <<"content-type">> => <<"application/json">>
            }, Response, Req0);
        {error, not_found} ->
            Req = cowboy_req:reply(404, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{error => <<"Agent not found">>}), Req0)
    end,
    {ok, Req, State};

handle_request(<<"GET">>, [<<"agents">>, AgentId, <<"history">>], Req0, State) ->
    % Get conversation history for an agent
    case get_agent_history(binary_to_list(AgentId)) of
        {ok, History} ->
            Response = jsx:encode(#{history => History}),
            Req = cowboy_req:reply(200, #{
                <<"content-type">> => <<"application/json">>
            }, Response, Req0);
        {error, Reason} ->
            Req = cowboy_req:reply(404, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{error => Reason}), Req0)
    end,
    {ok, Req, State};

handle_request(<<"POST">>, [<<"agents">>, AgentId, <<"collaborate">>], Req0, State) ->
    % Enable collaboration between agents
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    case jsx:decode(Body, [return_maps]) of
        #{<<"target_agent_id">> := TargetId, <<"message">> := Message} ->
            Result = initiate_collaboration(
                binary_to_list(AgentId), 
                binary_to_list(TargetId), 
                Message
            ),
            Response = jsx:encode(Result),
            Req = cowboy_req:reply(200, #{
                <<"content-type">> => <<"application/json">>
            }, Response, Req1);
        _ ->
            Req = cowboy_req:reply(400, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{error => <<"Invalid request">>}), Req1)
    end,
    {ok, Req, State};

handle_request(<<"GET">>, [<<"analytics">>], Req0, State) ->
    % Get analytics data
    Analytics = generate_analytics(),
    Response = jsx:encode(Analytics),
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, Response, Req0),
    {ok, Req, State};

handle_request(_, _, Req0, State) ->
    Req = cowboy_req:reply(404, #{}, Req0),
    {ok, Req, State}.

%% Internal functions

collect_all_metrics() ->
    Agents = agent_registry:list_agents(),
    AgentMetrics = lists:map(fun({Id, Pid, Meta}) ->
        #{
            id => list_to_binary(Id),
            type => maps:get(type, Meta, unknown),
            name => maps:get(name, Meta, <<"Unnamed">>),
            status => get_process_status(Pid),
            metrics => get_basic_metrics(Pid)
        }
    end, Agents),
    
    #{
        timestamp => erlang:timestamp(),
        system => #{
            node => node(),
            uptime => element(1, erlang:statistics(wall_clock)),
            memory => #{
                total => erlang:memory(total),
                processes => erlang:memory(processes),
                atom => erlang:memory(atom),
                binary => erlang:memory(binary),
                ets => erlang:memory(ets)
            },
            cpu => #{
                schedulers => erlang:system_info(schedulers_online),
                run_queue => erlang:statistics(run_queue)
            },
            processes => #{
                count => erlang:system_info(process_count),
                limit => erlang:system_info(process_limit)
            }
        },
        agents => AgentMetrics
    }.

get_agent_detailed_metrics(AgentId) ->
    case agent_registry:find_agent(AgentId) of
        {ok, Pid} ->
            case catch gen_server:call(Pid, get_state, 5000) of
                State when is_map(State) ->
                    ProcessInfo = get_detailed_process_info(Pid),
                    {ok, #{
                        agent_state => State,
                        process_info => ProcessInfo,
                        performance => calculate_performance_metrics(State)
                    }};
                _ ->
                    {error, <<"Failed to get agent state">>}
            end;
        {error, not_found} ->
            {error, not_found}
    end.

get_agent_history(AgentId) ->
    case agent_registry:find_agent(AgentId) of
        {ok, Pid} ->
            case catch gen_server:call(Pid, get_state, 5000) of
                #{conversation_length := Length} = State ->
                    % For now, return a summary. In production, would fetch actual history
                    {ok, #{
                        conversation_length => Length,
                        last_activity => maps:get(last_activity, State, undefined)
                    }};
                _ ->
                    {error, <<"Failed to get agent history">>}
            end;
        _ ->
            {error, <<"Agent not found">>}
    end.

initiate_collaboration(FromAgentId, ToAgentId, Message) ->
    case {agent_registry:find_agent(FromAgentId), agent_registry:find_agent(ToAgentId)} of
        {{ok, FromPid}, {ok, ToPid}} ->
            % Send collaboration request
            CollabId = generate_collaboration_id(),
            spawn(fun() ->
                % In a real implementation, this would coordinate between agents
                gen_server:cast(ToPid, {collaboration_request, FromAgentId, Message})
            end),
            #{
                status => <<"initiated">>,
                collaboration_id => CollabId,
                from => list_to_binary(FromAgentId),
                to => list_to_binary(ToAgentId)
            };
        _ ->
            #{error => <<"One or both agents not found">>}
    end.

generate_analytics() ->
    Agents = agent_registry:list_agents(),
    
    % Calculate various analytics
    TotalAgents = length(Agents),
    AgentsByType = count_by_type(Agents),
    
    % Aggregate metrics
    {TotalRequests, TotalTokens, AvgResponseTime} = aggregate_agent_metrics(Agents),
    
    #{
        summary => #{
            total_agents => TotalAgents,
            active_agents => count_active_agents(Agents),
            agents_by_type => AgentsByType
        },
        performance => #{
            total_requests => TotalRequests,
            total_tokens => TotalTokens,
            avg_response_time => AvgResponseTime
        },
        trends => generate_trends(),
        timestamp => erlang:timestamp()
    }.

%% Helper functions

get_process_status(Pid) ->
    case process_info(Pid) of
        undefined -> <<"dead">>;
        Info ->
            Current = proplists:get_value(current_function, Info, undefined),
            case Current of
                {gen_server, loop, _} -> <<"idle">>;
                _ -> <<"active">>
            end
    end.

get_basic_metrics(Pid) ->
    case process_info(Pid, [memory, message_queue_len, reductions]) of
        undefined -> #{};
        Info ->
            #{
                memory => proplists:get_value(memory, Info, 0),
                message_queue_len => proplists:get_value(message_queue_len, Info, 0),
                reductions => proplists:get_value(reductions, Info, 0)
            }
    end.

get_detailed_process_info(Pid) ->
    case process_info(Pid) of
        undefined -> #{status => <<"dead">>};
        Info ->
            #{
                status => <<"alive">>,
                memory => proplists:get_value(memory, Info, 0),
                heap_size => proplists:get_value(heap_size, Info, 0),
                stack_size => proplists:get_value(stack_size, Info, 0),
                reductions => proplists:get_value(reductions, Info, 0),
                message_queue_len => proplists:get_value(message_queue_len, Info, 0),
                current_function => format_mfa(proplists:get_value(current_function, Info, undefined))
            }
    end.

format_mfa(undefined) -> <<"unknown">>;
format_mfa({M, F, A}) -> 
    list_to_binary(io_lib:format("~p:~p/~p", [M, F, A])).

calculate_performance_metrics(State) ->
    Metrics = maps:get(metrics, State, #{}),
    TotalReqs = maps:get(total_requests, Metrics, 0),
    SuccessReqs = maps:get(successful_requests, Metrics, 0),
    
    SuccessRate = case TotalReqs of
        0 -> 0.0;
        _ -> (SuccessReqs / TotalReqs) * 100
    end,
    
    #{
        success_rate => SuccessRate,
        total_tokens => maps:get(total_tokens, Metrics, 0),
        avg_tokens_per_request => case TotalReqs of
            0 -> 0;
            _ -> maps:get(total_tokens, Metrics, 0) div TotalReqs
        end
    }.

count_by_type(Agents) ->
    lists:foldl(fun({_, _, Meta}, Acc) ->
        Type = maps:get(type, Meta, unknown),
        maps:update_with(Type, fun(V) -> V + 1 end, 1, Acc)
    end, #{}, Agents).

count_active_agents(Agents) ->
    lists:foldl(fun({_, Pid, _}, Count) ->
        case get_process_status(Pid) of
            <<"active">> -> Count + 1;
            _ -> Count
        end
    end, 0, Agents).

aggregate_agent_metrics(Agents) ->
    lists:foldl(fun({_, Pid, _}, {Reqs, Tokens, Times}) ->
        case catch gen_server:call(Pid, get_state, 1000) of
            #{metrics := Metrics} ->
                {
                    Reqs + maps:get(total_requests, Metrics, 0),
                    Tokens + maps:get(total_tokens, Metrics, 0),
                    Times + 1
                };
            _ ->
                {Reqs, Tokens, Times}
        end
    end, {0, 0, 0}, Agents).

generate_trends() ->
    % In a real implementation, this would analyze historical data
    #{
        agent_growth => <<"stable">>,
        request_trend => <<"increasing">>,
        performance_trend => <<"improving">>
    }.

generate_collaboration_id() ->
    list_to_binary(uuid:to_string(uuid:uuid4())).