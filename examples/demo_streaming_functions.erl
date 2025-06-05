%% demo_streaming_functions.erl
%% Demonstrates streaming function calls implementation

-module(demo_streaming_functions).
-export([demo/0]).

demo() ->
    io:format("~n=== Streaming Function Calls Demo ===~n~n"),
    
    % Show the key components
    io:format("1. Key Components:~n"),
    io:format("   - streaming_function_handler.erl: Handles aggregation of streaming function calls~n"),
    io:format("   - openai_chat.erl: Enhanced SSE parsing for tool calls~n"),
    io:format("   - agent_instance.erl: Integrated streaming handler~n~n"),
    
    % Show how it works
    io:format("2. How Streaming Function Calls Work:~n"),
    io:format("   a) Stream events are parsed and accumulated~n"),
    io:format("   b) Tool call deltas are merged as they arrive~n"),
    io:format("   c) When complete, tools are executed in parallel~n"),
    io:format("   d) Results are streamed back to continue the conversation~n~n"),
    
    % Example of the accumulator structure
    io:format("3. Accumulator Structure Example:~n"),
    Accumulator = #{
        content => <<"The weather in">>,
        tool_calls => [],
        tool_call_map => #{
            0 => #{
                <<"id">> => <<"call_abc123">>,
                <<"type">> => <<"function">>,
                <<"function">> => #{
                    <<"name">> => <<"jina_search">>,
                    <<"arguments">> => <<"{\"query\": \"weather San Francisco\"">>
                }
            }
        }
    },
    io:format("   ~p~n~n", [Accumulator]),
    
    % Show the streaming event flow
    io:format("4. Streaming Event Flow:~n"),
    Events = [
        {stream_start, #{agent_id => <<"agent_123">>}},
        {stream_token, <<"I'll search for">>},
        {stream_token, <<" the current weather.">>},
        {stream_function_call_started, #{<<"name">> => <<"jina_search">>}},
        {stream_function_arguments_delta, 0, <<"{\"query\":">>},
        {stream_function_arguments_delta, 0, <<" \"weather SF\"}">>},
        {stream_function_call_complete, #{<<"id">> => <<"call_123">>}},
        {stream_function_execution_start},
        {stream_tool_result, #{tool => jina_search, result => <<"Weather data...">>}},
        {stream_token, <<"Based on my search,">>},
        {stream_token, <<" it's 65°F and sunny.">>},
        {stream_complete, #{message => <<"Complete response">>}}
    ],
    
    lists:foreach(fun(Event) ->
        io:format("   -> ~p~n", [Event])
    end, Events),
    
    io:format("~n5. Benefits of Streaming Function Calls:~n"),
    io:format("   - Users see text as it's generated~n"),
    io:format("   - Function calls are detected and executed mid-stream~n"),
    io:format("   - Parallel execution of multiple tools~n"),
    io:format("   - Seamless continuation after tool results~n~n"),
    
    io:format("6. API Support:~n"),
    io:format("   - Chat Completions API: Full streaming with tool_calls~n"),
    io:format("   - Responses API: Enhanced streaming with function_call events~n"),
    io:format("   - Both APIs use the same streaming_function_handler~n~n"),
    
    io:format("=== Demo Complete ===~n~n"),
    ok.