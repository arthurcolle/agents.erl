#!/usr/bin/env escript
%% Test script to verify bytes streaming tokens are fixed

main([]) ->
    io:format("Testing bytes streaming token fix...~n"),
    
    % Test cases with different token formats
    TestCases = [
        % Byte list representing "Sociology"
        {[83, 111, 99, 105, 111, 108, 111, 103, 121], <<"Sociology">>},
        % Byte list representing "Hello"  
        {[72, 101, 108, 108, 111], <<"Hello">>},
        % Regular binary
        {<<"Already binary">>, <<"Already binary">>},
        % UTF-8 with special characters
        {[195, 169], <<"é">>},  % é in UTF-8
        % Mixed content
        {[72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100], <<"Hello World">>}
    ],
    
    % Test process_token_for_display function
    io:format("Testing process_token_for_display/1:~n"),
    lists:foreach(fun({Input, Expected}) ->
        Result = streaming_function_handler:process_token_for_display(Input),
        Status = case Result of
            Expected -> "✓ PASS";
            _ -> "✗ FAIL"
        end,
        io:format("  ~s: ~p -> ~p (expected: ~p)~n", [Status, Input, Result, Expected])
    end, TestCases),
    
    % Test ensure_json_safe function  
    io:format("~nTesting ensure_json_safe/1:~n"),
    lists:foreach(fun({Input, Expected}) ->
        Result = agent_ws_handler:ensure_json_safe(Input),
        Status = case Result of
            Expected -> "✓ PASS";
            _ -> "✗ FAIL"
        end,
        io:format("  ~s: ~p -> ~p (expected: ~p)~n", [Status, Input, Result, Expected])
    end, TestCases),
    
    io:format("~nBytes streaming token fix test complete!~n"),
    halt(0).