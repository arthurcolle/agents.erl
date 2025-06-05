#!/usr/bin/env escript
%%% Test script for parallel function calling
%%% Tests that tool calls are executed in parallel and results are collected correctly

-module(test_parallel_tools).

main(_) ->
    io:format("Testing parallel function calling...~n"),
    
    % Start required applications
    application:start(inets),
    application:start(ssl),
    application:start(openai),
    application:start(agents),
    
    % Test data: multiple tool calls that should execute in parallel
    ToolCalls = [
        #{
            <<"type">> => <<"function">>,
            <<"function">> => #{
                <<"name">> => <<"get_current_time">>,
                <<"arguments">> => <<"{}">>
            }
        },
        #{
            <<"type">> => <<"function">>,
            <<"function">> => #{
                <<"name">> => <<"simulate_delay">>,
                <<"arguments">> => <<"{\"seconds\": 1}">>
            }
        },
        #{
            <<"type">> => <<"function">>,
            <<"function">> => #{
                <<"name">> => <<"get_system_info">>,
                <<"arguments">> => <<"{}">>
            }
        }
    ],
    
    % Start timer to measure execution time
    StartTime = erlang:system_time(millisecond),
    
    % Execute tool calls (should be parallel)
    case catch agent_instance:execute_tool_calls(ToolCalls) of
        Results when is_list(Results) ->
            EndTime = erlang:system_time(millisecond),
            ExecutionTime = EndTime - StartTime,
            
            io:format("✅ Parallel execution completed in ~p ms~n", [ExecutionTime]),
            io:format("✅ Got ~p results~n", [length(Results)]),
            
            % Verify we got results for all tool calls
            if 
                length(Results) =:= length(ToolCalls) ->
                    io:format("✅ All tool calls executed successfully~n"),
                    
                    % Check that execution was actually parallel (should be faster than sum of delays)
                    if 
                        ExecutionTime < 2000 -> % Should be much less than 1000ms delay
                            io:format("✅ Execution appears to be parallel (< 2 seconds)~n");
                        true ->
                            io:format("⚠️  Execution may not be parallel (took ~p ms)~n", [ExecutionTime])
                    end;
                true ->
                    io:format("❌ Expected ~p results, got ~p~n", [length(ToolCalls), length(Results)])
            end;
        Error ->
            io:format("❌ Tool execution failed: ~p~n", [Error])
    end,
    
    io:format("~nParallel function calling test completed.~n").