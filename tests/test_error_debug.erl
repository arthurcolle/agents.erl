#!/usr/bin/env escript

main(_) ->
    code:add_paths(["_build/default/lib/*/ebin"]),
    
    % Start applications
    application:ensure_all_started(crypto),
    application:ensure_all_started(jsx),
    application:ensure_all_started(openai),
    application:ensure_all_started(agents),
    
    io:format("Testing agent with weather query...~n"),
    
    % Create an agent
    Config = #{
        id => <<"test-agent">>,
        name => <<"Test Agent">>,
        type => ai,
        model => <<"gpt-4">>,
        tools => [weather_info],  % This will likely trigger the error
        system_prompt => <<"You are a helpful assistant.">>,
        api_preference => responses
    },
    
    % Start the agent application
    agent:start(),
    
    % Use the agent API directly
    try
        Result = agent:chat(<<"test-agent">>, <<"What is the weather in DC?">>),
        io:format("Result: ~p~n", [Result])
    catch
        Error:Reason:Stack ->
            io:format("Caught error: ~p:~p~nStack: ~p~n", [Error, Reason, Stack])
    end,
    
    ok.