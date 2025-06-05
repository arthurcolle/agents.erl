#!/usr/bin/env escript
%% test_streaming_fix.erl
%% Quick test to verify the streaming token fix

main(_Args) ->
    io:format("🧪 Testing Streaming Token Fix~n"),
    io:format("============================~n~n"),
    
    %% Test cases that would previously show as bytes
    TestCases = [
        {<<"Hello world">>, "Binary text"},
        {"Sociology is", "String text"},
        {[83, 111, 99, 105, 111, 108, 111, 103, 121], "Byte list (should be 'Sociology')"},
        {[72, 101, 108, 108, 111], "Byte list (should be 'Hello')"},
        {"the scientific study", "Normal string"},
        {<<"streams correctly">>, "Binary string"}
    ],
    
    %% Add compiled paths and ensure the module is loaded
    code:add_paths(["_build/default/lib/agents/ebin", "_build/default/lib/openai/ebin", "_build/default/lib/agent_web/ebin"]),
    case code:ensure_loaded(streaming_function_handler) of
        {module, streaming_function_handler} ->
            io:format("✅ streaming_function_handler module loaded~n~n");
        {error, LoadError} ->
            io:format("❌ Failed to load streaming_function_handler: ~p~n", [LoadError]),
            halt(1)
    end,
    
    %% Test each case
    lists:foreach(fun({Input, Description}) ->
        io:format("🔍 Testing: ~s~n", [Description]),
        io:format("   Input:  ~p~n", [Input]),
        
        try
            Result = streaming_function_handler:process_token_for_display(Input),
            io:format("   Output: ~p~n", [Result]),
            
            %% Check if result looks like the old bug (starts with numbers)
            case Result of
                <<N, _/binary>> when N >= $0, N =< $9 ->
                    %% Starts with a digit, might be the old bug format
                    io:format("   Status: ⚠️  Might still show as bytes~n");
                _ ->
                    io:format("   Status: ✅ Looks good~n")
            end
        catch
            Error:Reason:Stacktrace ->
                io:format("   ERROR:  ~p:~p~n", [Error, Reason]),
                io:format("   Stacktrace: ~p~n", [Stacktrace]),
                io:format("   Status: ❌ Function call failed~n")
        end,
        io:format("~n")
    end, TestCases),
    
    io:format("🎯 Summary~n"),
    io:format("--------~n"),
    io:format("✅ If output shows readable text (not numbers), the fix works~n"),
    io:format("❌ If output shows byte sequences like '831119...', issue persists~n"),
    io:format("~n"),
    io:format("💡 The fix ensures:~n"),
    io:format("   • UTF-8 strings display as text, not bytes~n"),
    io:format("   • Byte lists convert to readable strings~n"),
    io:format("   • Binary data stays as readable binaries~n"),
    io:format("   • Invalid UTF-8 is handled gracefully~n").