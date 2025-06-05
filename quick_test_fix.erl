-module(quick_test_fix).
-export([run/0]).

run() ->
    io:format("🧪 Quick Test of Streaming Fix~n"),
    io:format("=============================~n~n"),
    
    %% Test the fix with various inputs
    TestCases = [
        {<<"Hello world">>, "Binary text"},
        {"Sociology is", "String text"},
        {[83, 111, 99, 105, 111, 108, 111, 103, 121], "Byte list 'Sociology'"},
        {[72, 101, 108, 108, 111], "Byte list 'Hello'"}
    ],
    
    lists:foreach(fun({Input, Description}) ->
        io:format("🔍 ~s~n", [Description]),
        io:format("   Input:  ~p~n", [Input]),
        Result = streaming_function_handler:process_token_for_display(Input),
        io:format("   Output: ~p~n", [Result]),
        
        %% Check if it looks like readable text vs bytes
        case Result of
            <<C, _/binary>> when C >= 32, C =< 126 ->
                io:format("   Status: ✅ Looks like readable text~n");
            _ ->
                io:format("   Status: ⚠️  May need review~n")
        end,
        io:format("~n")
    end, TestCases),
    
    io:format("🎯 Summary: Fix should show readable text, not byte sequences~n").