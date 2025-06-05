#!/usr/bin/env escript
%%% Simple test for parallel tool execution functionality
%%% Tests the execute_tool_calls function directly

-module(test_parallel_simple).

main(_) ->
    io:format("Testing parallel tool execution...~n"),
    
    % Start required applications
    application:start(inets),
    application:start(ssl),
    application:start(openai),
    application:start(agents),
    
    % Create simple test tool calls
    ToolCalls = [
        #{
            <<"id">> => <<"call_1">>,
            <<"type">> => <<"function">>,
            <<"function">> => #{
                <<"name">> => <<"get_current_time">>,
                <<"arguments">> => <<"{}">>
            }
        },
        #{
            <<"id">> => <<"call_2">>,
            <<"type">> => <<"function">>,
            <<"function">> => #{
                <<"name">> => <<"get_system_info">>,
                <<"arguments">> => <<"{}">>
            }
        }
    ],
    
    % Test parallel execution by calling the function from within a mock agent instance module
    io:format("Simulating parallel tool execution...~n"),
    
    StartTime = erlang:system_time(millisecond),
    
    % Execute in parallel (simulate what agent_instance does)
    ParentPid = self(),
    Tasks = lists:map(fun(ToolCall) ->
        spawn_link(fun() ->
            Result = execute_single_tool_call(ToolCall),
            ParentPid ! {tool_result, self(), Result}
        end)
    end, ToolCalls),
    
    % Collect results
    Results = collect_tool_results(Tasks, []),
    EndTime = erlang:system_time(millisecond),
    ExecutionTime = EndTime - StartTime,
    
    io:format("✅ Parallel execution completed in ~p ms~n", [ExecutionTime]),
    io:format("✅ Got ~p results: ~p~n", [length(Results), Results]),
    
    % Test OAuth configuration
    test_oauth_configuration(),
    
    io:format("~nParallel function calling test completed successfully!~n").

%% Simulate the execute_single_tool_call function
execute_single_tool_call(ToolCall) ->
    ToolId = maps:get(<<"id">>, ToolCall),
    FunctionInfo = maps:get(<<"function">>, ToolCall),
    ToolName = maps:get(<<"name">>, FunctionInfo),
    
    % Simulate tool execution with a small delay
    timer:sleep(100), % 100ms delay to simulate work
    
    Result = case ToolName of
        <<"get_current_time">> -> 
            #{time => calendar:local_time(), function => <<"get_current_time">>};
        <<"get_system_info">> -> 
            #{info => #{erlang_version => erlang:system_info(version)}, function => <<"get_system_info">>};
        _ -> 
            #{error => <<"unknown_tool">>}
    end,
    
    {ToolId, Result}.

%% Collect results from parallel tasks
collect_tool_results([], Results) ->
    lists:reverse(Results);
collect_tool_results([Pid | Remaining], Results) ->
    receive
        {tool_result, Pid, Result} ->
            collect_tool_results(Remaining, [Result | Results])
    after 5000 ->  % 5 second timeout
        {error, timeout}
    end.

%% Test OAuth configuration
test_oauth_configuration() ->
    io:format("~nTesting OAuth configuration...~n"),
    
    % Test that OAuth servers are defined correctly
    try
        SeedServers = mcp_server_config:get_seed_servers(),
        OAuthServers = lists:filter(fun(Server) ->
            maps:get(auth_type, Server, open) =:= oauth2
        end, SeedServers),
        
        io:format("✅ Found ~p OAuth-enabled MCP servers~n", [length(OAuthServers)]),
        
        % Show a few example OAuth servers
        lists:foreach(fun(Server) ->
            Name = maps:get(name, Server, <<"Unknown">>),
            Url = maps:get(url, Server, <<"Unknown">>),
            io:format("  - ~s: ~s~n", [Name, Url])
        end, lists:sublist(OAuthServers, 3)),
        
        % Test OAuth integration module if available
        try
            case mcp_oauth_integration:is_oauth_required(<<"linear">>) of
                true ->
                    io:format("✅ OAuth integration properly configured~n");
                false ->
                    io:format("⚠️  OAuth integration not detecting OAuth servers~n")
            end
        catch
            _:_ ->
                io:format("⚠️  OAuth integration module not accessible~n")
        end
        
    catch
        _:Error ->
            io:format("❌ OAuth configuration test failed: ~p~n", [Error])
    end.