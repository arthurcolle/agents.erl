#!/usr/bin/env escript
%% Test script for training data generation and timeline access

main(_) ->
    io:format("Testing Training Data Generation and Timeline Access System~n"),
    
    % Test 1: Timeline access functions
    test_timeline_access(),
    
    % Test 2: Training data generation
    test_training_data_generation(),
    
    io:format("All tests completed!~n").

test_timeline_access() ->
    io:format("~n=== Testing Timeline Access Functions ===~n"),
    
    % Test getting timeline events
    io:format("Testing get_timeline_events...~n"),
    Args1 = #{<<"limit">> => 10},
    try
        case agent_tools:execute_tool(get_timeline_events, Args1) of
            {ok, Result1} ->
                io:format("✓ get_timeline_events succeeded: ~p~n", [Result1]);
            {error, Reason1} ->
                io:format("✗ get_timeline_events failed: ~p~n", [Reason1])
        end
    catch
        _:Error1 ->
            io:format("✗ get_timeline_events crashed: ~p~n", [Error1])
    end,
    
    % Test searching timeline
    io:format("Testing search_timeline...~n"),
    Args2 = #{<<"query">> => <<"hello">>, <<"limit">> => 5},
    try
        case agent_tools:execute_tool(search_timeline, Args2) of
            {ok, Result2} ->
                io:format("✓ search_timeline succeeded: ~p~n", [Result2]);
            {error, Reason2} ->
                io:format("✗ search_timeline failed: ~p~n", [Reason2])
        end
    catch
        _:Error2 ->
            io:format("✗ search_timeline crashed: ~p~n", [Error2])
    end.

test_training_data_generation() ->
    io:format("~n=== Testing Training Data Generation ===~n"),
    
    % Test training data generation
    io:format("Testing generate_training_data...~n"),
    Args = #{<<"min_quality">> => 0.1, <<"max_samples">> => 100},
    try
        case agent_tools:execute_tool(generate_training_data, Args) of
            {ok, Result} ->
                io:format("✓ generate_training_data succeeded: ~p~n", [Result]);
            {error, Reason} ->
                io:format("✗ generate_training_data failed: ~p~n", [Reason])
        end
    catch
        _:Error ->
            io:format("✗ generate_training_data crashed: ~p~n", [Error])
    end.