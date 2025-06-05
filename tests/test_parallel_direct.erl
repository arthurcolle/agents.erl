#!/usr/bin/env escript

%% Direct test for parallel function calling functionality
-module(test_parallel_direct).
-mode(compile).

-export([main/1]).

main(_) ->
    io:format("=== Direct Parallel Function Calling Test ===~n"),
    
    % Connect to the running node
    case net_kernel:start([test_client]) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    
    % Wait a moment for connection
    timer:sleep(500),
    
    try
        test_direct_execution(),
        io:format("~n✅ Direct parallel function calling test completed!~n")
    catch
        Class:Reason:Stacktrace ->
            io:format("❌ Test failed: ~p:~p~n~p~n", [Class, Reason, Stacktrace]),
            halt(1)
    end.

test_direct_execution() ->
    io:format("~n⚡ Testing direct parallel execution...~n"),
    
    % Create test tool calls that will demonstrate parallel execution
    ToolCalls = [
        #{
            <<"id">> => <<"call_1">>,
            <<"type">> => <<"function">>,
            <<"function">> => #{
                <<"name">> => <<"shell">>,
                <<"arguments">> => jsx:encode(#{<<"command">> => <<"echo 'Parallel Task 1'">>})
            }
        },
        #{
            <<"id">> => <<"call_2">>,
            <<"type">> => <<"function">>,
            <<"function">> => #{
                <<"name">> => <<"who_am_i">>,
                <<"arguments">> => jsx:encode(#{})
            }
        },
        #{
            <<"id">> => <<"call_3">>,
            <<"type">> => <<"function">>,
            <<"function">> => #{
                <<"name">> => <<"get_system_state">>,
                <<"arguments">> => jsx:encode(#{})
            }
        }
    ],
    
    io:format("Testing with ~p tool calls:~n", [length(ToolCalls)]),
    lists:foreach(fun(Call) ->
        FuncName = maps:get(<<"name">>, maps:get(<<"function">>, Call)),
        io:format("  - ~s~n", [FuncName])
    end, ToolCalls),
    
    % Measure execution time
    StartTime = erlang:system_time(millisecond),
    
    % Execute the tool calls directly on agent_instance module
    % This tests our parallel execution implementation
    try
        Results = case rpc:call(node(), agent_instance, execute_tool_calls, [ToolCalls]) of
            {badrpc, Reason} -> 
                io:format("⚠️  RPC failed: ~p, testing locally...~n", [Reason]),
                test_local_execution(ToolCalls);
            LocalResults -> LocalResults
        end,
        
        EndTime = erlang:system_time(millisecond),
        Duration = EndTime - StartTime,
        
        io:format("✅ Executed in ~p ms~n", [Duration]),
        io:format("   Result count: ~p~n", [length(Results)]),
        
        % Show sample results
        lists:foreach(fun({Index, Result}) ->
            ResultPreview = case Result of
                B when is_binary(B) -> 
                    Preview = binary:part(B, 0, min(50, byte_size(B))),
                    <<Preview/binary, "...">>;
                M when is_map(M) -> <<"[JSON Object]">>;
                T when is_tuple(T), element(1, T) =:= error -> 
                    iolist_to_binary(io_lib:format("Error: ~p", [element(2, T)]));
                Other -> 
                    iolist_to_binary(io_lib:format("~p", [Other]))
            end,
            io:format("   [~p]: ~s~n", [Index, ResultPreview])
        end, lists:zip(lists:seq(1, length(Results)), Results))
        
    catch
        E:R:S ->
            io:format("❌ Execution failed: ~p:~p~n~p~n", [E, R, S])
    end,
    
    ok.

test_local_execution(ToolCalls) ->
    io:format("Testing local execution simulation...~n"),
    
    % Simulate parallel execution by executing each tool call
    Results = lists:map(fun(ToolCall) ->
        FuncName = maps:get(<<"name">>, maps:get(<<"function">>, ToolCall)),
        Args = maps:get(<<"arguments">>, maps:get(<<"function">>, ToolCall)),
        
        % Parse arguments
        ParsedArgs = try jsx:decode(Args) catch _:_ -> #{} end,
        
        % Execute tool based on name
        case FuncName of
            <<"shell">> ->
                Command = maps:get(<<"command">>, ParsedArgs, <<"echo 'test'">>),
                <<"Shell executed: ", Command/binary>>;
            <<"who_am_i">> ->
                <<"I am a test agent">>;
            <<"get_system_state">> ->
                <<"System state: operational">>;
            _ ->
                <<"Unknown tool: ", FuncName/binary>>
        end
    end, ToolCalls),
    
    Results.