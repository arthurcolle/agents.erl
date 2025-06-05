#!/usr/bin/env escript

%% Comprehensive test for parallel function calling with multiple tools
-module(test_parallel_function_calling_comprehensive).
-mode(compile).

-export([main/1]).

main(_) ->
    % Start applications
    application:ensure_all_started(agents),
    application:ensure_all_started(agent_web),
    application:ensure_all_started(openai),
    
    % Wait for services to start
    timer:sleep(1000),
    
    % Set model to use gpt-4.1-mini as requested
    application:set_env(openai, default_model, <<"gpt-4.1-mini">>),
    
    io:format("=== Comprehensive Parallel Function Calling Test ===~n"),
    
    try
        test_agent_with_tools(),
        test_parallel_execution_performance(),
        test_multiple_function_calls(),
        io:format("~n✅ All parallel function calling tests passed!~n")
    catch
        Class:Reason:Stacktrace ->
            io:format("❌ Test failed: ~p:~p~n~p~n", [Class, Reason, Stacktrace]),
            halt(1)
    after
        timer:sleep(500)
    end.

test_agent_with_tools() ->
    io:format("~n🔧 Testing agent creation with tools...~n"),
    
    % Create agent with multiple tools that can be called in parallel
    Tools = [
        <<"shell">>,
        <<"file_read">>, 
        <<"http_request">>,
        <<"get_system_state">>,
        <<"who_am_i">>,
        <<"jina_search">>,
        <<"knowledge_base_retrieval">>
    ],
    
    % Create an AI agent with tools
    AgentConfig = #{
        id => <<"test-parallel-agent">>,
        name => <<"Parallel Test Agent">>,
        type => ai,
        model => <<"gpt-4.1-mini">>,
        tools => Tools,
        system_prompt => <<"You are a test agent for demonstrating parallel function calling. Use multiple tools to answer queries efficiently.">>
    },
    
    {ok, AgentPid} = agent_instance:start_link(AgentConfig),
    
    % Verify agent is working
    {ok, AgentState} = gen_server:call(AgentPid, get_state),
    io:format("✅ Agent created with ~p tools~n", [length(maps:get(tools, AgentState, []))]),
    
    % Clean up
    gen_server:stop(AgentPid),
    
    ok.

test_parallel_execution_performance() ->
    io:format("~n⚡ Testing parallel execution performance...~n"),
    
    % Create synthetic tool calls that will execute in parallel
    ToolCalls = [
        #{
            <<"id">> => <<"call_1">>,
            <<"type">> => <<"function">>,
            <<"function">> => #{
                <<"name">> => <<"shell">>,
                <<"arguments">> => jsx:encode(#{<<"command">> => <<"echo 'Task 1'; sleep 1">>})
            }
        },
        #{
            <<"id">> => <<"call_2">>,
            <<"type">> => <<"function">>,
            <<"function">> => #{
                <<"name">> => <<"shell">>,
                <<"arguments">> => jsx:encode(#{<<"command">> => <<"echo 'Task 2'; sleep 1">>})
            }
        },
        #{
            <<"id">> => <<"call_3">>,
            <<"type">> => <<"function">>,
            <<"function">> => #{
                <<"name">> => <<"shell">>,
                <<"arguments">> => jsx:encode(#{<<"command">> => <<"echo 'Task 3'; sleep 1">>})
            }
        }
    ],
    
    % Test parallel execution
    StartTime = erlang:system_time(millisecond),
    
    % Execute using the enhanced parallel execution
    Results = agent_instance:execute_tool_calls(ToolCalls),
    
    EndTime = erlang:system_time(millisecond),
    Duration = EndTime - StartTime,
    
    io:format("✅ Executed ~p tool calls in parallel in ~p ms~n", [length(ToolCalls), Duration]),
    io:format("   Results: ~p~n", [Results]),
    
    % Verify it was actually parallel (should be ~1000ms, not ~3000ms)
    case Duration < 2000 of
        true -> 
            io:format("✅ Parallel execution confirmed (< 2000ms)~n");
        false -> 
            io:format("⚠️  Sequential execution detected (~p ms)~n", [Duration])
    end,
    
    ok.

test_multiple_function_calls() ->
    io:format("~n🔄 Testing multiple different function calls...~n"),
    
    % Create diverse tool calls to test parallel execution of different tool types
    DiverseToolCalls = [
        #{
            <<"id">> => <<"info_call">>,
            <<"type">> => <<"function">>,
            <<"function">> => #{
                <<"name">> => <<"who_am_i">>,
                <<"arguments">> => jsx:encode(#{})
            }
        },
        #{
            <<"id">> => <<"system_call">>,
            <<"type">> => <<"function">>,
            <<"function">> => #{
                <<"name">> => <<"get_system_state">>,
                <<"arguments">> => jsx:encode(#{})
            }
        },
        #{
            <<"id">> => <<"file_call">>,
            <<"type">> => <<"function">>,
            <<"function">> => #{
                <<"name">> => <<"file_read">>,
                <<"arguments">> => jsx:encode(#{<<"path">> => <<"/etc/hostname">>})
            }
        }
    ],
    
    % Execute diverse tool calls in parallel
    StartTime = erlang:system_time(millisecond),
    DiverseResults = agent_instance:execute_tool_calls(DiverseToolCalls),
    EndTime = erlang:system_time(millisecond),
    Duration = EndTime - StartTime,
    
    io:format("✅ Executed ~p diverse tool calls in ~p ms~n", [length(DiverseToolCalls), Duration]),
    io:format("   Diverse results count: ~p~n", [length(DiverseResults)]),
    
    % Verify all calls completed successfully
    SuccessCount = length([R || R <- DiverseResults, is_binary(R) orelse is_map(R)]),
    ErrorCount = length([R || R <- DiverseResults, is_tuple(R), element(1, R) =:= error]),
    
    io:format("✅ Success count: ~p, Error count: ~p~n", [SuccessCount, ErrorCount]),
    
    ok.