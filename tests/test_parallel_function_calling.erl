#!/usr/bin/env escript
%%% Test script for parallel function calling via the agent system
%%% Tests that tool calls are executed in parallel through the agent API

-module(test_parallel_function_calling).

main(_) ->
    io:format("Testing parallel function calling via agent system...~n"),
    
    % Start required applications
    application:start(inets),
    application:start(ssl),
    application:start(openai),
    application:start(agents),
    
    % Create an agent with tool calling capabilities
    AgentConfig = #{
        name => <<"test_agent">>,
        type => multi_agent,
        model => <<"gpt-4">>,
        tools => [get_current_time, simulate_delay, get_system_info],
        system_prompt => <<"You are a test agent for parallel function calling.">>,
        conversation_history => []
    },
    
    % Start the agent
    case agent_instance:start_link(AgentConfig) of
        {ok, AgentPid} ->
            io:format("✅ Agent started successfully~n"),
            
            % Test message that should trigger parallel tool calls
            TestMessage = <<"Please get the current time, simulate a 1-second delay, and get system info - do all in parallel">>,
            
            % Execute the message (should trigger function calling)
            StartTime = erlang:system_time(millisecond),
            
            case agent_instance:execute(AgentPid, #{message => TestMessage}) of
                {ok, Response, _State} ->
                    EndTime = erlang:system_time(millisecond),
                    ExecutionTime = EndTime - StartTime,
                    
                    io:format("✅ Agent execution completed in ~p ms~n", [ExecutionTime]),
                    io:format("✅ Response: ~p~n", [maps:get(message, Response, <<"No message">>)]),
                    
                    % If it took less than 2 seconds, it's likely parallel
                    if 
                        ExecutionTime < 2000 ->
                            io:format("✅ Execution appears to be parallel (< 2 seconds)~n");
                        true ->
                            io:format("⚠️  Execution may not be parallel (took ~p ms)~n", [ExecutionTime])
                    end;
                {error, Reason} ->
                    io:format("❌ Agent execution failed: ~p~n", [Reason])
            end,
            
            % Test OAuth integration by checking if OAuth-enabled servers are available
            io:format("~nTesting OAuth integration...~n"),
            test_oauth_integration(),
            
            gen_server:stop(AgentPid);
        {error, Reason} ->
            io:format("❌ Failed to start agent: ~p~n", [Reason])
    end,
    
    io:format("~nParallel function calling and OAuth integration tests completed.~n").

test_oauth_integration() ->
    % Test that OAuth integration is properly configured
    try
        case mcp_oauth_integration:is_oauth_required(<<"linear">>) of
            true ->
                io:format("✅ OAuth integration properly detects OAuth-required servers~n");
            false ->
                io:format("⚠️  OAuth integration may not be properly configured~n")
        end,
        
        % Test getting OAuth URL for a known OAuth server
        case mcp_oauth_integration:get_oauth_url(<<"linear">>, <<"test_user">>) of
            {ok, Url} when is_binary(Url) ->
                io:format("✅ OAuth URL generation works: ~s~n", [Url]);
            {error, Reason} ->
                io:format("⚠️  OAuth URL generation failed: ~p~n", [Reason])
        end
    catch
        _:Error ->
            io:format("❌ OAuth integration test failed: ~p~n", [Error])
    end.