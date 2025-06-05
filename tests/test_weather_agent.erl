-module(test_weather_agent).
-export([test/0]).

test() ->
    io:format("Starting weather agent test with enhanced search...~n"),
    
    % Start required applications
    application:ensure_all_started(openai),
    application:ensure_all_started(agents),
    
    % Create an agent with the new jina_search_and_read tool
    {ok, Agent} = agent:create_with_tools(
        <<"weather_agent">>,
        <<"gpt-4o-mini">>,
        [jina_search_and_read],
        <<"You are a weather assistant. When asked about weather, use the jina_search_and_read tool to find current conditions and provide accurate, real-time information.">>
    ),
    
    io:format("Agent created: ~p~n", [Agent]),
    
    % Send weather query
    Message = <<"What's the current weather in Boston, Massachusetts?">>,
    io:format("Sending message: ~s~n", [Message]),
    
    case agent:send_message(Agent, Message) of
        {ok, Response} ->
            io:format("~n=== Agent Response ===~n~s~n", [Response]);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end,
    
    % Clean up
    agent:stop(Agent),
    io:format("Weather agent test completed!~n").