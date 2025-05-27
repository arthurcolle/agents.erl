%% agent_tests.erl
%% Unit tests for the agent module
-module(agent_tests).
-include_lib("eunit/include/eunit.hrl").

-import(test_helpers, [
    setup_test_env/0,
    cleanup_test_env/0,
    create_test_tool/1,
    create_test_tool/2,
    with_mock_openai/1,
    eventually/1
]).

%% Test fixture setup
setup() ->
    setup_test_env().

cleanup(_) ->
    cleanup_test_env().

agent_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Agent lifecycle tests", fun test_agent_lifecycle/0},
      {"Tool definition tests", fun test_tool_definition/0},
      {"Tool execution tests", fun test_tool_execution/0},
      {"Agent execution tests", fun test_agent_execution/0},
      {"Error handling tests", fun test_error_handling/0},
      {"Timeout tests", fun test_timeouts/0},
      {"Concurrent agent tests", fun test_concurrent_agents/0}
     ]
    }.

%% Test agent lifecycle
test_agent_lifecycle() ->
    % Test starting the application
    ?assertMatch({ok, _}, application:ensure_all_started(myapp)),
    
    % Verify supervisor is running
    ?assertMatch(Pid when is_pid(Pid), whereis(agent)),
    
    % Test stopping the application
    ?assertEqual(ok, agent:stop()),
    
    % Restart for other tests
    ?assertEqual(ok, agent:start()).

%% Test tool definition
test_tool_definition() ->
    % Define a simple tool
    ToolName = test_tool_def,
    Schema = #{
        type => function,
        function => #{
            name => <<"test_tool_def">>,
            description => <<"A test tool">>,
            parameters => #{
                type => object,
                properties => #{
                    value => #{
                        type => string,
                        description => <<"Test value">>
                    }
                },
                required => [<<"value">>]
            }
        }
    },
    
    % Test successful definition
    ?assertEqual(ok, agent:define_tool(ToolName, Schema)),
    
    % Test redefining tool (should succeed)
    ?assertEqual(ok, agent:define_tool(ToolName, Schema)),
    
    % Test invalid schema
    ?assertMatch({error, _}, agent:define_tool(bad_tool, invalid_schema)).

%% Test tool execution
test_tool_execution() ->
    % Create a test tool
    ToolName = test_exec_tool,
    create_test_tool(ToolName, #{
        response => fun(#{<<"input">> := Input}) ->
            {ok, <<"Processed: ", Input/binary>>}
        end
    }),
    
    % Register executor
    ExecuteCalled = self(),
    ?assertEqual(ok, agent:execute_tool(ToolName,
        fun(Args) ->
            ExecuteCalled ! {executed, Args},
            {ok, <<"test_result">>}
        end,
        #{}
    )),
    
    % Test tool execution through agent_tools
    Result = agent_tools:execute_tool(ToolName, #{<<"input">> => <<"test">>}),
    
    % Verify execution
    receive
        {executed, _Args} -> ok
    after 1000 ->
        ?assert(false, "Tool was not executed")
    end,
    
    ?assertEqual({ok, <<"test_result">>}, Result).

%% Test agent execution
test_agent_execution() ->
    with_mock_openai(fun() ->
        % Create test tool
        ToolName = agent_test_tool,
        create_test_tool(ToolName),
        
        % Mock OpenAI response without tool calls
        meck:new(openai_chat, [passthrough]),
        meck:expect(openai_chat, create_chat_completion,
            fun(_Model, _Messages, _Options) ->
                {ok, #{
                    <<"choices">> => [#{
                        <<"message">> => #{
                            <<"content">> => <<"Test response">>
                        }
                    }]
                }}
            end
        ),
        
        % Run agent
        Result = agent:run_agent(
            <<"Test prompt">>,
            [ToolName],
            #{timeout => 5000}
        ),
        
        ?assertEqual({ok, <<"Test response">>}, Result),
        
        % Cleanup
        meck:unload(openai_chat)
    end).

%% Test error handling
test_error_handling() ->
    % Test with non-existent tool
    Result1 = agent:run_agent(
        <<"Use non-existent tool">>,
        [non_existent_tool],
        #{timeout => 1000}
    ),
    ?assertMatch({error, _}, Result1),
    
    % Test with tool that returns error
    ErrorTool = error_test_tool,
    create_test_tool(ErrorTool, #{
        response => fun(_) -> {error, tool_error} end
    }),
    
    with_mock_openai(fun() ->
        meck:new(openai_chat, [passthrough]),
        meck:expect(openai_chat, create_chat_completion,
            fun(_Model, _Messages, _Options) ->
                {ok, #{
                    <<"choices">> => [#{
                        <<"message">> => #{
                            <<"tool_calls">> => [#{
                                <<"id">> => <<"call_123">>,
                                <<"name">> => atom_to_binary(ErrorTool),
                                <<"arguments">> => <<"{\"input\":\"test\"}">>
                            }]
                        }
                    }]
                }}
            end
        ),
        
        Result2 = agent:run_agent(
            <<"Use error tool">>,
            [ErrorTool],
            #{timeout => 5000}
        ),
        
        % Should handle tool error gracefully
        ?assertMatch({ok, _}, Result2),
        
        meck:unload(openai_chat)
    end).

%% Test timeouts
test_timeouts() ->
    % Create slow tool
    SlowTool = slow_test_tool,
    create_test_tool(SlowTool, #{
        response => fun(_) ->
            timer:sleep(2000),
            {ok, <<"slow_response">>}
        end
    }),
    
    with_mock_openai(fun() ->
        meck:new(openai_chat, [passthrough]),
        meck:expect(openai_chat, create_chat_completion,
            fun(_Model, _Messages, _Options) ->
                timer:sleep(2000),
                {ok, #{<<"choices">> => []}}
            end
        ),
        
        % Test with short timeout
        Result = agent:run_agent(
            <<"Slow request">>,
            [SlowTool],
            #{timeout => 100}
        ),
        
        ?assertEqual({error, timeout}, Result),
        
        meck:unload(openai_chat)
    end).

%% Test concurrent agents
test_concurrent_agents() ->
    NumAgents = 10,
    
    with_mock_openai(fun() ->
        % Create a counting tool
        CounterPid = spawn_link(fun() -> counter_loop(0) end),
        
        CountTool = count_tool,
        create_test_tool(CountTool, #{
            response => fun(_) ->
                CounterPid ! {increment, self()},
                receive
                    {count, N} -> {ok, integer_to_binary(N)}
                after 1000 ->
                    {error, timeout}
                end
            end
        }),
        
        meck:new(openai_chat, [passthrough]),
        meck:expect(openai_chat, create_chat_completion,
            fun(_Model, _Messages, _Options) ->
                {ok, #{
                    <<"choices">> => [#{
                        <<"message">> => #{
                            <<"tool_calls">> => [#{
                                <<"id">> => <<"call_", (integer_to_binary(erlang:unique_integer()))/binary>>,
                                <<"name">> => atom_to_binary(CountTool),
                                <<"arguments">> => <<"{\"input\":\"count\"}">>
                            }]
                        }
                    }]
                }}
            end
        ),
        
        % Spawn concurrent agents
        Parent = self(),
        _Pids = [
            spawn_link(fun() ->
                Result = agent:run_agent(
                    <<"Count request ", (integer_to_binary(I))/binary>>,
                    [CountTool],
                    #{timeout => 5000}
                ),
                Parent ! {agent_done, I, Result}
            end)
            || I <- lists:seq(1, NumAgents)
        ],
        
        % Collect results
        Results = [
            receive
                {agent_done, I, Result} -> {I, Result}
            after 10000 ->
                {I, {error, timeout}}
            end
            || I <- lists:seq(1, NumAgents)
        ],
        
        % Verify all agents completed
        ?assertEqual(NumAgents, length(Results)),
        
        % Verify all got unique counts
        Counts = [
            binary_to_integer(Count)
            || {_, {ok, Count}} <- Results
        ],
        SortedCounts = lists:sort(Counts),
        ?assertEqual(lists:seq(1, NumAgents), SortedCounts),
        
        % Cleanup
        CounterPid ! stop,
        meck:unload(openai_chat)
    end).

%% Helper: Counter process
counter_loop(N) ->
    receive
        {increment, From} ->
            NewN = N + 1,
            From ! {count, NewN},
            counter_loop(NewN);
        stop ->
            ok
    end.

%% Additional test cases

supervisor_restart_test() ->
    % Test that supervisor restarts failed children
    ?assertMatch({ok, _}, application:ensure_all_started(myapp)),
    
    % Get registry pid
    RegistryPid = whereis(agent_registry),
    ?assert(is_pid(RegistryPid)),
    
    % Kill the registry
    exit(RegistryPid, kill),
    
    % Wait for restart
    timer:sleep(100),
    
    % Verify it restarted
    NewRegistryPid = whereis(agent_registry),
    ?assert(is_pid(NewRegistryPid)),
    ?assertNotEqual(RegistryPid, NewRegistryPid).

api_endpoint_listing_test() ->
    % Test listing available endpoints
    Endpoints = agent:list_available_endpoints(),
    ?assert(is_list(Endpoints)),
    ?assert(lists:member(chat, Endpoints)).

ensure_api_client_test() ->
    % Test ensuring API client
    ?assertEqual(ok, agent:ensure_api_client(chat)),
    
    % Test with invalid group
    ?assertMatch({error, _}, agent:ensure_api_client(invalid_group)).

complex_tool_schema_test() ->
    % Test with complex nested schema
    ComplexSchema = #{
        type => function,
        function => #{
            name => <<"complex_tool">>,
            description => <<"A complex tool">>,
            parameters => #{
                type => object,
                properties => #{
                    nested => #{
                        type => object,
                        properties => #{
                            field1 => #{type => string},
                            field2 => #{type => integer},
                            array => #{
                                type => array,
                                items => #{type => string}
                            }
                        }
                    },
                    optional => #{
                        type => string,
                        description => <<"Optional field">>
                    }
                },
                required => [<<"nested">>]
            }
        }
    },
    
    ?assertEqual(ok, agent:define_tool(complex_tool, ComplexSchema)).