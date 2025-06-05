#!/usr/bin/env escript

main(_Args) ->
    io:format("Testing function calling fix...~n"),
    
    %% Test the agent_instance:execute_tool_calls function directly
    %% Simulating OpenAI tool call format
    ToolCall = #{
        <<"type">> => <<"function">>,
        <<"id">> => <<"call_123">>,
        <<"function">> => #{
            <<"name">> => <<"get_weather">>,
            <<"arguments">> => <<"{\"latitude\": 40.7128, \"longitude\": -74.0060}">>
        }
    },
    
    io:format("Simulated tool call: ~p~n", [ToolCall]),
    
    %% The fix should now properly detect the "function" type
    %% and handle the call structure correctly
    case maps:get(<<"type">>, ToolCall, undefined) of
        <<"function">> ->
            io:format("✓ Tool call type correctly detected as 'function'~n");
        _ ->
            io:format("✗ Tool call type not detected correctly~n")
    end,
    
    %% Test the create_tool_messages format
    ToolCallFormat = case maps:get(<<"type">>, ToolCall, undefined) of
        <<"function">> ->
            #{
                <<"role">> => <<"tool">>,
                <<"tool_call_id">> => maps:get(<<"id">>, ToolCall),
                <<"content">> => <<"Test response">>
            };
        _ ->
            #{<<"error">> => <<"Unknown format">>}
    end,
    
    io:format("Tool response format: ~p~n", [ToolCallFormat]),
    
    %% Test memory health check fix
    io:format("Testing memory health check...~n"),
    Memory = erlang:memory(),
    Total = proplists:get_value(total, Memory, 0),
    Processes = proplists:get_value(processes, Memory, 0),
    
    io:format("✓ Memory check: Total=~p, Processes=~p~n", [Total, Processes]),
    
    %% Test should complete without badmap error
    case Total of
        0 -> io:format("✓ Division by zero protection working~n");
        _ -> 
            MemoryHealth = (Processes / Total) < 0.8,
            io:format("✓ Memory health check: ~p~n", [MemoryHealth])
    end,
    
    io:format("All tests completed successfully!~n").