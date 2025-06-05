#!/usr/bin/env escript
%% Test script to verify the weather query fix

-module(test_weather_fix).

main(_Args) ->
    io:format("Starting test of weather query fix...~n"),
    
    % Start the applications
    application:ensure_all_started(openai),
    application:ensure_all_started(agents),
    application:ensure_all_started(agent_web),
    
    % Wait a moment for everything to initialize
    timer:sleep(2000),
    
    % Create a test agent
    {ok, AgentPid} = agent_supervisor:start_agent(#{
        id => <<"test-weather-agent">>,
        name => <<"Test Weather Agent">>,
        model => <<"gpt-4o-mini">>,
        tools => [jina_search],
        system_prompt => <<"You are a helpful assistant that can search for information.">>
    }),
    
    io:format("Agent created: ~p~n", [AgentPid]),
    
    % Test the weather query that was problematic
    Message = <<"current weather in Washington DC">>,
    io:format("Testing query: ~p~n", [Message]),
    
    % Execute the chat and capture the result
    try
        Result = agent:chat(AgentPid, Message),
        io:format("SUCCESS! Agent response: ~p~n", [Result])
    catch
        Error:Reason:Stack ->
            io:format("ERROR! ~p:~p~n~p~n", [Error, Reason, Stack])
    end,
    
    % Stop the agent
    agent_supervisor:stop_agent(AgentPid),
    io:format("Test completed.~n").