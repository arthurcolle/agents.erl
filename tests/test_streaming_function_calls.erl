#!/usr/bin/env escript
%% test_streaming_function_calls.erl
%% Test script for streaming function calls

-mode(compile).

main([]) ->
    io:format("~n=== Testing Streaming Function Calls ===~n~n"),
    
    % Start required applications
    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssl),
    application:start(inets),
    application:start(jsx),
    
    % Add project paths
    code:add_paths([
        "_build/default/lib/agents/ebin",
        "_build/default/lib/openai/ebin",
        "_build/default/lib/agent_web/ebin"
    ]),
    
    % Start the applications
    application:ensure_all_started(openai),
    application:ensure_all_started(agents),
    
    % Test streaming with function calls
    test_streaming_function_calls(),
    
    io:format("~n=== Test Complete ===~n"),
    halt(0).

test_streaming_function_calls() ->
    io:format("1. Creating AI agent with function calling tools...~n"),
    
    % Start the agent supervisor first
    {ok, _} = agent_supervisor:start_link(),
    
    % Create an agent with search and file tools
    {ok, AgentPid} = agent_supervisor:start_agent(#{
        name => <<"Streaming Function Test Agent">>,
        model => <<"gpt-4o">>,
        tools => [jina_search, file_read, file_write],
        system_prompt => <<"You are a helpful assistant with access to web search and file operations. 
                           Always use tools when they would be helpful to answer questions.">>,
        api_preference => chat  % Test with Chat API first
    }),
    
    io:format("   Agent created: ~p~n~n", [AgentPid]),
    
    % Test 1: Simple streaming without function calls
    io:format("2. Testing simple streaming (no function calls)...~n"),
    test_simple_streaming(AgentPid),
    timer:sleep(2000),
    
    % Test 2: Streaming with single function call
    io:format("~n3. Testing streaming with single function call...~n"),
    test_streaming_with_function(AgentPid),
    timer:sleep(2000),
    
    % Test 3: Streaming with multiple parallel function calls
    io:format("~n4. Testing streaming with multiple parallel function calls...~n"),
    test_streaming_with_parallel_functions(AgentPid),
    timer:sleep(2000),
    
    % Test 4: Test with Responses API
    io:format("~n5. Testing with Responses API...~n"),
    test_responses_api_streaming(AgentPid),
    
    ok.

test_simple_streaming(AgentPid) ->
    StreamPid = self(),
    Message = <<"Tell me a very short joke about programming.">>,
    
    io:format("   Sending message: ~s~n", [Message]),
    agent:stream_chat(AgentPid, Message, StreamPid),
    
    collect_stream_output().

test_streaming_with_function(AgentPid) ->
    StreamPid = self(),
    Message = <<"What's the current weather in San Francisco? Be brief.">>,
    
    io:format("   Sending message: ~s~n", [Message]),
    agent:stream_chat(AgentPid, Message, StreamPid),
    
    collect_stream_output().

test_streaming_with_parallel_functions(AgentPid) ->
    StreamPid = self(),
    Message = <<"Search for 'Erlang OTP' and also write a file called test_output.txt with the search results summary.">>,
    
    io:format("   Sending message: ~s~n", [Message]),
    agent:stream_chat(AgentPid, Message, StreamPid),
    
    collect_stream_output().

test_responses_api_streaming(AgentPid) ->
    % Update agent to use Responses API
    agent_instance:update_config(AgentPid, #{api_preference => responses}),
    
    StreamPid = self(),
    Message = <<"Search for information about streaming in OpenAI API and summarize it.">>,
    
    io:format("   Sending message: ~s~n", [Message]),
    agent:stream_chat(AgentPid, Message, StreamPid),
    
    collect_stream_output().

collect_stream_output() ->
    collect_stream_output(<<>>, false).

collect_stream_output(Content, FunctionCallStarted) ->
    receive
        {stream_start, Info} ->
            io:format("   [STREAM START] ~p~n", [Info]),
            collect_stream_output(Content, FunctionCallStarted);
            
        {stream_token, Token} ->
            io:format("~s", [Token]),
            collect_stream_output(<<Content/binary, Token/binary>>, FunctionCallStarted);
            
        {stream_function_call_started, FuncCall} ->
            io:format("~n   [FUNCTION CALL STARTED] ~p~n", [FuncCall]),
            collect_stream_output(Content, true);
            
        {stream_function_arguments_delta, Index, Delta} ->
            io:format("   [FUNCTION ARGS DELTA] Index: ~p, Delta: ~p~n", [Index, Delta]),
            collect_stream_output(Content, FunctionCallStarted);
            
        {stream_function_call_complete, FuncCall} ->
            io:format("   [FUNCTION CALL COMPLETE] ~p~n", [FuncCall]),
            collect_stream_output(Content, FunctionCallStarted);
            
        {stream_function_execution_start} ->
            io:format("   [EXECUTING FUNCTIONS...]~n"),
            collect_stream_output(Content, FunctionCallStarted);
            
        {stream_tool_result, Result} ->
            io:format("   [TOOL RESULT] ~p~n", [Result]),
            collect_stream_output(Content, FunctionCallStarted);
            
        {stream_complete, Response} ->
            io:format("~n   [STREAM COMPLETE]~n"),
            io:format("   Final content: ~s~n", [maps:get(message, Response, Content)]),
            case maps:get(tool_calls, Response, []) of
                [] -> ok;
                ToolCalls -> io:format("   Tool calls executed: ~p~n", [length(ToolCalls)])
            end,
            ok;
            
        {stream_error, Reason} ->
            io:format("~n   [STREAM ERROR] ~p~n", [Reason]),
            error;
            
        Other ->
            io:format("~n   [UNKNOWN MESSAGE] ~p~n", [Other]),
            collect_stream_output(Content, FunctionCallStarted)
            
    after 30000 ->
        io:format("~n   [TIMEOUT]~n"),
        timeout
    end.