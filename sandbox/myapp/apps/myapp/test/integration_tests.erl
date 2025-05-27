%%%-------------------------------------------------------------------
%%% @doc
%%% Integration Tests
%%% End-to-end integration tests for the agent system
%%% @end
%%%-------------------------------------------------------------------
-module(integration_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TIMEOUT, 10000).

%%%===================================================================
%%% Setup and Teardown
%%%===================================================================

setup() ->
    % Start all applications
    ok = application:ensure_all_started(myapp),
    ok = application:ensure_all_started(openai),
    
    % Clear any existing state
    agent_registry:clear_all(),
    agent_discovery:clear_all(),
    agent_tools:clear_all(),
    
    ok.

teardown(_) ->
    % Stop applications
    application:stop(openai),
    application:stop(myapp),
    
    % Clean up
    agent_registry:clear_all(),
    agent_discovery:clear_all(), 
    agent_tools:clear_all(),
    
    ok.

%%%===================================================================
%%% Test Generators
%%%===================================================================

integration_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
      {"Complete agent lifecycle", fun test_agent_lifecycle/0},
      {"Multi-agent collaboration", fun test_multi_agent_collaboration/0},
      {"Tool-based workflow", fun test_tool_workflow/0},
      {"Message routing scenario", fun test_message_routing/0},
      {"Error recovery", fun test_error_recovery/0},
      {"Performance under load", fun test_performance_load/0},
      {"Discovery and capability matching", fun test_discovery_capabilities/0},
      {"Distributed agent network", fun test_distributed_network/0},
      {"Real-world use case", fun test_real_world_scenario/0}
     ]}.

%%%===================================================================
%%% Test Cases
%%%===================================================================

test_agent_lifecycle() ->
    % Create and start an agent
    AgentId = <<"lifecycle_agent">>,
    AgentSpec = #{
        id => AgentId,
        name => <<"Lifecycle Test Agent">>,
        capabilities => [<<"test">>, <<"echo">>],
        tools => [
            #{
                name => <<"echo">>,
                handler => fun(#{<<"message">> := Msg}) -> {ok, Msg} end,
                parameters => [#{name => <<"message">>, type => string, required => true}]
            }
        ],
        handlers => #{
            message => fun(Msg) -> 
                io:format("Agent received: ~p~n", [Msg]),
                {ok, processed}
            end
        }
    },
    
    % Start agent
    {ok, AgentPid} = agent:start_agent(AgentSpec),
    ?assert(is_pid(AgentPid)),
    
    % Verify registration
    ?assertEqual({ok, AgentPid}, agent_registry:lookup(AgentId)),
    
    % Verify discovery
    Agents = agent_discovery:find_agents_by_capability(<<"test">>),
    ?assert(lists:member(AgentId, Agents)),
    
    % Send message to agent
    TestMessage = #{
        type => <<"test">>,
        content => <<"Hello, Agent!">>
    },
    
    {ok, _MsgId} = agent_messenger:send_message(<<"test_sender">>, AgentId, TestMessage),
    
    % Execute tool
    {ok, <<"Test echo">>} = agent:call_tool(AgentId, <<"echo">>, 
                                           #{<<"message">> => <<"Test echo">>}),
    
    % Update agent
    UpdatedSpec = AgentSpec#{
        capabilities => [<<"test">>, <<"echo">>, <<"analyze">>]
    },
    ok = agent:update_agent(AgentId, UpdatedSpec),
    
    % Verify update
    UpdatedAgents = agent_discovery:find_agents_by_capability(<<"analyze">>),
    ?assert(lists:member(AgentId, UpdatedAgents)),
    
    % Stop agent
    ok = agent:stop_agent(AgentId),
    
    % Verify cleanup
    ?assertEqual({error, not_found}, agent_registry:lookup(AgentId)),
    ?assertEqual([], agent_discovery:find_agents_by_capability(<<"test">>)).

test_multi_agent_collaboration() ->
    % Create a network of collaborating agents
    
    % Data collector agent
    CollectorId = <<"collector_agent">>,
    {ok, _} = agent:start_agent(#{
        id => CollectorId,
        name => <<"Data Collector">>,
        capabilities => [<<"collect">>, <<"aggregate">>],
        tools => [
            #{
                name => <<"collect_data">>,
                handler => fun(#{<<"source">> := Source}) ->
                    Data = #{
                        source => Source,
                        timestamp => erlang:system_time(millisecond),
                        value => rand:uniform(100)
                    },
                    {ok, Data}
                end,
                parameters => [#{name => <<"source">>, type => string, required => true}]
            }
        ]
    }),
    
    % Analyzer agent
    AnalyzerId = <<"analyzer_agent">>,
    {ok, _} = agent:start_agent(#{
        id => AnalyzerId,
        name => <<"Data Analyzer">>,
        capabilities => [<<"analyze">>, <<"statistics">>],
        tools => [
            #{
                name => <<"analyze_data">>,
                handler => fun(#{<<"data">> := DataList}) ->
                    Values = [maps:get(value, D) || D <- DataList],
                    Stats = #{
                        count => length(Values),
                        sum => lists:sum(Values),
                        avg => lists:sum(Values) / length(Values),
                        min => lists:min(Values),
                        max => lists:max(Values)
                    },
                    {ok, Stats}
                end,
                parameters => [#{name => <<"data">>, type => list, required => true}]
            }
        ]
    }),
    
    % Reporter agent
    ReporterId = <<"reporter_agent">>,
    Parent = self(),
    {ok, _} = agent:start_agent(#{
        id => ReporterId,
        name => <<"Report Generator">>,
        capabilities => [<<"report">>, <<"format">>],
        handlers => #{
            message => fun(Msg) ->
                case maps:get(type, maps:get(message, Msg)) of
                    <<"report_request">> ->
                        Parent ! {report_started, self()},
                        {ok, starting};
                    <<"report_data">> ->
                        Parent ! {report_data, maps:get(message, Msg)},
                        {ok, received};
                    _ ->
                        {ok, ignored}
                end
            end
        }
    }),
    
    % Orchestrate collaboration
    
    % 1. Collect data from multiple sources
    Sources = [<<"sensor_1">>, <<"sensor_2">>, <<"sensor_3">>],
    CollectedData = lists:map(fun(Source) ->
        {ok, Data} = agent:call_tool(CollectorId, <<"collect_data">>, 
                                   #{<<"source">> => Source}),
        Data
    end, Sources),
    
    % 2. Send data to analyzer
    {ok, Stats} = agent:call_tool(AnalyzerId, <<"analyze_data">>,
                                #{<<"data">> => CollectedData}),
    
    ?assert(maps:is_key(avg, Stats)),
    ?assertEqual(3, maps:get(count, Stats)),
    
    % 3. Send report request to reporter
    {ok, _} = agent_messenger:send_message(
        <<"orchestrator">>, 
        ReporterId,
        #{type => <<"report_request">>, stats => Stats}
    ),
    
    receive
        {report_started, _} -> ok
    after ?TIMEOUT ->
        ?assert(false)
    end,
    
    % 4. Reporter requests formatted data from analyzer
    {ok, _} = agent_messenger:send_message(
        ReporterId,
        ReporterId,
        #{type => <<"report_data">>, data => Stats}
    ),
    
    receive
        {report_data, ReportMsg} ->
            ?assert(maps:is_key(data, ReportMsg))
    after ?TIMEOUT ->
        ?assert(false)
    end,
    
    % Clean up
    agent:stop_agent(CollectorId),
    agent:stop_agent(AnalyzerId),
    agent:stop_agent(ReporterId).

test_tool_workflow() ->
    % Create agent with chained tools
    WorkflowId = <<"workflow_agent">>,
    
    {ok, _} = agent:start_agent(#{
        id => WorkflowId,
        name => <<"Workflow Agent">>,
        capabilities => [<<"process">>, <<"transform">>],
        tools => [
            #{
                name => <<"validate">>,
                handler => fun(#{<<"input">> := Input}) ->
                    case is_binary(Input) andalso byte_size(Input) > 0 of
                        true -> {ok, #{valid => true, input => Input}};
                        false -> {error, invalid_input}
                    end
                end,
                parameters => [#{name => <<"input">>, type => any, required => true}]
            },
            #{
                name => <<"transform">>,
                handler => fun(#{<<"data">> := Data, <<"operation">> := Op}) ->
                    Input = maps:get(input, Data),
                    Result = case Op of
                        <<"uppercase">> -> string:uppercase(Input);
                        <<"reverse">> -> list_to_binary(lists:reverse(binary_to_list(Input)));
                        <<"base64">> -> base64:encode(Input);
                        _ -> Input
                    end,
                    {ok, #{transformed => Result, operation => Op}}
                end,
                parameters => [
                    #{name => <<"data">>, type => map, required => true},
                    #{name => <<"operation">>, type => string, required => true}
                ]
            },
            #{
                name => <<"finalize">>,
                handler => fun(#{<<"result">> := Result}) ->
                    {ok, #{
                        finalized => true,
                        output => maps:get(transformed, Result),
                        metadata => #{
                            processed_at => erlang:system_time(millisecond),
                            operation => maps:get(operation, Result)
                        }
                    }}
                end,
                parameters => [#{name => <<"result">>, type => map, required => true}]
            }
        ]
    }),
    
    % Execute workflow
    Input = <<"Hello, World!">>,
    
    % Step 1: Validate
    {ok, ValidatedData} = agent:call_tool(WorkflowId, <<"validate">>, 
                                        #{<<"input">> => Input}),
    ?assertEqual(true, maps:get(valid, ValidatedData)),
    
    % Step 2: Transform
    {ok, TransformedData} = agent:call_tool(WorkflowId, <<"transform">>,
                                          #{<<"data">> => ValidatedData,
                                            <<"operation">> => <<"uppercase">>}),
    ?assertEqual(<<"HELLO, WORLD!">>, maps:get(transformed, TransformedData)),
    
    % Step 3: Finalize
    {ok, FinalResult} = agent:call_tool(WorkflowId, <<"finalize">>,
                                      #{<<"result">> => TransformedData}),
    ?assertEqual(true, maps:get(finalized, FinalResult)),
    ?assertEqual(<<"HELLO, WORLD!">>, maps:get(output, FinalResult)),
    
    % Test error handling in workflow
    {error, invalid_input} = agent:call_tool(WorkflowId, <<"validate">>,
                                           #{<<"input">> => <<>>}),
    
    % Clean up
    agent:stop_agent(WorkflowId).

test_message_routing() ->
    % Create a message routing network
    
    % Gateway agent
    GatewayId = <<"gateway_agent">>,
    {ok, _} = agent:start_agent(#{
        id => GatewayId,
        name => <<"Gateway">>,
        capabilities => [<<"route">>, <<"filter">>],
        handlers => #{
            message => fun(Msg) ->
                InnerMsg = maps:get(message, Msg),
                Type = maps:get(type, InnerMsg, undefined),
                
                case Type of
                    <<"public">> ->
                        % Route to public processor
                        agent_messenger:send_message(GatewayId, <<"public_processor">>, InnerMsg);
                    <<"private">> ->
                        % Route to private processor
                        agent_messenger:send_message(GatewayId, <<"private_processor">>, InnerMsg);
                    <<"broadcast">> ->
                        % Send to all processors
                        agent_messenger:broadcast_message(GatewayId, 
                            [<<"public_processor">>, <<"private_processor">>], InnerMsg);
                    _ ->
                        {error, unknown_message_type}
                end
            end
        }
    }),
    
    % Processor agents
    Parent = self(),
    
    PublicId = <<"public_processor">>,
    {ok, _} = agent:start_agent(#{
        id => PublicId,
        name => <<"Public Processor">>,
        capabilities => [<<"process_public">>],
        handlers => #{
            message => fun(Msg) ->
                Parent ! {public_received, maps:get(message, Msg)},
                {ok, processed}
            end
        }
    }),
    
    PrivateId = <<"private_processor">>,
    {ok, _} = agent:start_agent(#{
        id => PrivateId,
        name => <<"Private Processor">>,
        capabilities => [<<"process_private">>],
        handlers => #{
            message => fun(Msg) ->
                Parent ! {private_received, maps:get(message, Msg)},
                {ok, processed}
            end
        }
    }),
    
    % Test routing
    
    % Send public message
    {ok, _} = agent_messenger:send_message(<<"client">>, GatewayId,
                                         #{type => <<"public">>, data => <<"public data">>}),
    
    receive
        {public_received, PubMsg} ->
            ?assertEqual(<<"public">>, maps:get(type, PubMsg))
    after ?TIMEOUT ->
        ?assert(false)
    end,
    
    % Send private message
    {ok, _} = agent_messenger:send_message(<<"client">>, GatewayId,
                                         #{type => <<"private">>, data => <<"secret">>}),
    
    receive
        {private_received, PrivMsg} ->
            ?assertEqual(<<"private">>, maps:get(type, PrivMsg))
    after ?TIMEOUT ->
        ?assert(false)
    end,
    
    % Send broadcast
    {ok, _} = agent_messenger:send_message(<<"client">>, GatewayId,
                                         #{type => <<"broadcast">>, data => <<"announcement">>}),
    
    % Both should receive
    Messages = lists:sort([
        receive {public_received, M1} -> M1 after ?TIMEOUT -> timeout end,
        receive {private_received, M2} -> M2 after ?TIMEOUT -> timeout end
    ]),
    
    ?assertEqual(2, length([M || M <- Messages, M =/= timeout])),
    
    % Clean up
    agent:stop_agent(GatewayId),
    agent:stop_agent(PublicId),
    agent:stop_agent(PrivateId).

test_error_recovery() ->
    % Test system resilience and error recovery
    
    % Create agent that can fail
    UnstableId = <<"unstable_agent">>,
    FailureCount = ets:new(failure_count, [public, set]),
    ets:insert(FailureCount, {count, 0}),
    
    {ok, _} = agent:start_agent(#{
        id => UnstableId,
        name => <<"Unstable Agent">>,
        capabilities => [<<"compute">>],
        tools => [
            #{
                name => <<"unreliable_compute">>,
                handler => fun(#{<<"value">> := Value}) ->
                    Count = ets:update_counter(FailureCount, count, 1),
                    if
                        Count rem 3 =:= 0 -> error(deliberate_crash);
                        Count rem 3 =:= 1 -> {error, computation_failed};
                        true -> {ok, Value * 2}
                    end
                end,
                parameters => [#{name => <<"value">>, type => number, required => true}]
            }
        ],
        restart_strategy => #{
            type => transient,
            max_restarts => 5,
            max_time => 60000
        }
    }),
    
    % Test automatic recovery
    Results = lists:map(fun(N) ->
        timer:sleep(100), % Give time for potential restart
        try
            agent:call_tool(UnstableId, <<"unreliable_compute">>, 
                          #{<<"value">> => N}, #{timeout => 1000})
        catch
            _:_ -> {error, crashed}
        end
    end, lists:seq(1, 10)),
    
    % Should have mix of successes and failures
    Successes = [R || R <- Results, element(1, R) =:= ok],
    Errors = [R || R <- Results, element(1, R) =:= error],
    
    ?assert(length(Successes) > 0),
    ?assert(length(Errors) > 0),
    
    % Agent should still be alive after crashes
    ?assertMatch({ok, _}, agent_registry:lookup(UnstableId)),
    
    % Test circuit breaker
    CircuitId = <<"circuit_breaker_agent">>,
    {ok, _} = agent:start_agent(#{
        id => CircuitId,
        name => <<"Circuit Breaker Agent">>,
        capabilities => [<<"protected">>],
        tools => [
            #{
                name => <<"protected_call">>,
                handler => fun(_) ->
                    % Always fail
                    {error, service_unavailable}
                end,
                parameters => [],
                circuit_breaker => #{
                    threshold => 3,
                    timeout => 1000,
                    reset_timeout => 2000
                }
            }
        ]
    }),
    
    % Make calls until circuit opens
    OpenResults = lists:map(fun(_) ->
        agent:call_tool(CircuitId, <<"protected_call">>, #{})
    end, lists:seq(1, 5)),
    
    % After threshold, circuit should be open
    CircuitOpenErrors = [R || R <- OpenResults, R =:= {error, circuit_open}],
    ?assert(length(CircuitOpenErrors) > 0),
    
    % Clean up
    agent:stop_agent(UnstableId),
    agent:stop_agent(CircuitId),
    ets:delete(FailureCount).

test_performance_load() ->
    % Test system under load
    NumAgents = 50,
    NumMessages = 100,
    
    % Create load test agents
    AgentIds = lists:map(fun(N) ->
        Id = list_to_binary("load_agent_" ++ integer_to_list(N)),
        {ok, _} = agent:start_agent(#{
            id => Id,
            name => <<"Load Test Agent">>,
            capabilities => [<<"load_test">>],
            handlers => #{
                message => fun(Msg) ->
                    % Simulate some work
                    timer:sleep(rand:uniform(10)),
                    {ok, maps:get(id, maps:get(message, Msg))}
                end
            }
        }),
        Id
    end, lists:seq(1, NumAgents)),
    
    % Measure message throughput
    StartTime = erlang:monotonic_time(millisecond),
    
    % Send messages concurrently
    Parent = self(),
    lists:foreach(fun(MsgNum) ->
        spawn(fun() ->
            Target = lists:nth(rand:uniform(NumAgents), AgentIds),
            Sender = list_to_binary("sender_" ++ integer_to_list(MsgNum)),
            Message = #{id => MsgNum, timestamp => erlang:system_time()},
            
            Result = agent_messenger:send_message(Sender, Target, Message),
            Parent ! {sent, MsgNum, Result}
        end)
    end, lists:seq(1, NumMessages)),
    
    % Collect results
    SentResults = lists:map(fun(_) ->
        receive
            {sent, _, Result} -> Result
        after ?TIMEOUT ->
            timeout
        end
    end, lists:seq(1, NumMessages)),
    
    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,
    
    % Calculate metrics
    SuccessCount = length([R || R <- SentResults, element(1, R) =:= ok]),
    Throughput = SuccessCount * 1000 / Duration,
    
    io:format("Load test: ~p messages in ~p ms (~.2f msg/sec)~n",
              [SuccessCount, Duration, Throughput]),
    
    ?assert(SuccessCount >= NumMessages * 0.95), % 95% success rate
    ?assert(Throughput > 50), % At least 50 msg/sec
    
    % Clean up
    lists:foreach(fun(Id) -> agent:stop_agent(Id) end, AgentIds).

test_discovery_capabilities() ->
    % Test capability-based discovery and matching
    
    % Create agents with various capabilities
    Agents = [
        {<<"nlp_agent">>, [<<"text_analysis">>, <<"sentiment">>, <<"translation">>]},
        {<<"vision_agent">>, [<<"image_recognition">>, <<"ocr">>, <<"face_detection">>]},
        {<<"data_agent">>, [<<"etl">>, <<"analytics">>, <<"visualization">>]},
        {<<"ml_agent">>, [<<"classification">>, <<"regression">>, <<"clustering">>]},
        {<<"hybrid_agent">>, [<<"text_analysis">>, <<"classification">>, <<"etl">>]}
    ],
    
    lists:foreach(fun({Id, Caps}) ->
        {ok, _} = agent:start_agent(#{
            id => Id,
            name => Id,
            capabilities => Caps
        })
    end, Agents),
    
    % Test single capability search
    TextAgents = agent_discovery:find_agents_by_capability(<<"text_analysis">>),
    ?assertEqual(2, length(TextAgents)),
    ?assert(lists:member(<<"nlp_agent">>, TextAgents)),
    ?assert(lists:member(<<"hybrid_agent">>, TextAgents)),
    
    % Test multiple capability search
    MultiCapAgents = agent_discovery:find_agents_with_all_capabilities(
        [<<"text_analysis">>, <<"classification">>]),
    ?assertEqual([<<"hybrid_agent">>], MultiCapAgents),
    
    % Test capability matching with scoring
    Requirements = [<<"text_analysis">>, <<"sentiment">>, <<"classification">>],
    Matches = agent_discovery:find_best_matches(Requirements, #{max_results => 3}),
    
    % hybrid_agent and nlp_agent should score highest
    TopMatches = [Id || {Id, _Score} <- lists:sublist(Matches, 2)],
    ?assert(lists:member(<<"nlp_agent">>, TopMatches) orelse 
            lists:member(<<"hybrid_agent">>, TopMatches)),
    
    % Clean up
    lists:foreach(fun({Id, _}) -> agent:stop_agent(Id) end, Agents).

test_distributed_network() ->
    % Test distributed agent network behavior
    
    % Create node agents
    Nodes = [
        {<<"node_a">>, <<"region_1">>},
        {<<"node_b">>, <<"region_1">>},
        {<<"node_c">>, <<"region_2">>},
        {<<"node_d">>, <<"region_2">>}
    ],
    
    % Create network topology
    lists:foreach(fun({NodeId, Region}) ->
        {ok, _} = agent:start_agent(#{
            id => NodeId,
            name => NodeId,
            capabilities => [<<"relay">>, Region],
            metadata => #{region => Region},
            handlers => #{
                message => fun(Msg) ->
                    InnerMsg = maps:get(message, Msg),
                    case maps:get(type, InnerMsg) of
                        <<"ping">> ->
                            From = maps:get(from, InnerMsg),
                            agent_messenger:send_message(NodeId, From,
                                #{type => <<"pong">>, from => NodeId});
                        _ ->
                            {ok, received}
                    end
                end
            }
        })
    end, Nodes),
    
    % Test connectivity
    Parent = self(),
    TesterId = <<"network_tester">>,
    {ok, _} = agent:start_agent(#{
        id => TesterId,
        name => <<"Network Tester">>,
        capabilities => [<<"test">>],
        handlers => #{
            message => fun(Msg) ->
                Parent ! {pong_received, maps:get(message, Msg)},
                {ok, received}
            end
        }
    }),
    
    % Ping all nodes
    lists:foreach(fun({NodeId, _}) ->
        agent_messenger:send_message(TesterId, NodeId,
            #{type => <<"ping">>, from => TesterId})
    end, Nodes),
    
    % Collect pongs
    Pongs = lists:map(fun(_) ->
        receive
            {pong_received, Pong} -> maps:get(from, Pong)
        after 1000 ->
            timeout
        end
    end, Nodes),
    
    % All nodes should respond
    ?assertEqual(4, length([P || P <- Pongs, P =/= timeout])),
    
    % Test region-based routing
    Region1Nodes = agent_discovery:find_agents_by_capability(<<"region_1">>),
    ?assertEqual(2, length(Region1Nodes)),
    
    % Clean up
    lists:foreach(fun({Id, _}) -> agent:stop_agent(Id) end, Nodes),
    agent:stop_agent(TesterId).

test_real_world_scenario() ->
    % Simulate a real-world chatbot scenario with multiple specialized agents
    
    % User interface agent
    UIAgentId = <<"ui_agent">>,
    Parent = self(),
    {ok, _} = agent:start_agent(#{
        id => UIAgentId,
        name => <<"UI Agent">>,
        capabilities => [<<"interface">>, <<"format">>],
        handlers => #{
            message => fun(Msg) ->
                Parent ! {ui_response, maps:get(message, Msg)},
                {ok, displayed}
            end
        }
    }),
    
    % NLP agent for understanding
    NLPAgentId = <<"nlp_agent">>,
    {ok, _} = agent:start_agent(#{
        id => NLPAgentId,
        name => <<"NLP Agent">>,
        capabilities => [<<"understand">>, <<"parse">>],
        tools => [
            #{
                name => <<"parse_intent">>,
                handler => fun(#{<<"text">> := Text}) ->
                    % Simple intent detection
                    Intent = case Text of
                        <<"What is the weather", _/binary>> -> <<"weather_query">>;
                        <<"Tell me about", _/binary>> -> <<"information_query">>;
                        <<"Calculate", _/binary>> -> <<"math_query">>;
                        _ -> <<"general_query">>
                    end,
                    {ok, #{intent => Intent, text => Text}}
                end,
                parameters => [#{name => <<"text">>, type => string, required => true}]
            }
        ]
    }),
    
    % Knowledge agent
    KnowledgeAgentId = <<"knowledge_agent">>,
    {ok, _} = agent:start_agent(#{
        id => KnowledgeAgentId,
        name => <<"Knowledge Agent">>,
        capabilities => [<<"search">>, <<"retrieve">>],
        tools => [
            #{
                name => <<"search_knowledge">>,
                handler => fun(#{<<"query">> := Query}) ->
                    % Mock knowledge base search
                    Results = case Query of
                        #{intent := <<"weather_query">>} ->
                            <<"Current weather: Sunny, 72°F">>;
                        #{intent := <<"information_query">>} ->
                            <<"Here's what I found about that topic...">>;
                        _ ->
                            <<"I can help you with that.">>
                    end,
                    {ok, #{results => Results}}
                end,
                parameters => [#{name => <<"query">>, type => map, required => true}]
            }
        ]
    }),
    
    % Orchestrator agent
    OrchestratorId = <<"orchestrator">>,
    {ok, _} = agent:start_agent(#{
        id => OrchestratorId,
        name => <<"Orchestrator">>,
        capabilities => [<<"coordinate">>, <<"workflow">>],
        handlers => #{
            message => fun(Msg) ->
                UserQuery = maps:get(text, maps:get(message, Msg)),
                
                % Step 1: Parse intent
                {ok, ParsedIntent} = agent:call_tool(NLPAgentId, <<"parse_intent">>,
                                                   #{<<"text">> => UserQuery}),
                
                % Step 2: Search knowledge
                {ok, SearchResults} = agent:call_tool(KnowledgeAgentId, <<"search_knowledge">>,
                                                    #{<<"query">> => ParsedIntent}),
                
                % Step 3: Format and send response
                Response = maps:get(results, SearchResults),
                agent_messenger:send_message(OrchestratorId, UIAgentId,
                    #{type => <<"response">>, text => Response}),
                
                {ok, processed}
            end
        }
    }),
    
    % Simulate user interaction
    UserQuery = <<"What is the weather today?">>,
    
    % Send query to orchestrator
    {ok, _} = agent_messenger:send_message(<<"user">>, OrchestratorId,
                                         #{type => <<"query">>, text => UserQuery}),
    
    % Wait for UI response
    receive
        {ui_response, UIMsg} ->
            ResponseText = maps:get(text, UIMsg),
            ?assert(is_binary(ResponseText)),
            ?assertMatch(<<"Current weather:", _/binary>>, ResponseText)
    after ?TIMEOUT ->
        ?assert(false)
    end,
    
    % Clean up
    lists:foreach(fun(Id) -> agent:stop_agent(Id) end,
                  [UIAgentId, NLPAgentId, KnowledgeAgentId, OrchestratorId]).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

% Add any helper functions here if needed