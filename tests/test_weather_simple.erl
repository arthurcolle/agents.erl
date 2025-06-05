#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/*/ebin

main(_) ->
    % Start required applications
    application:ensure_all_started(openai),
    application:ensure_all_started(agents),
    
    % Start the agent_tools server
    agent_tools:start_link(#{}),
    
    % Test the get_weather tool
    Arguments = #{<<"location">> => <<"San Francisco, CA">>},
    
    io:format("Testing get_weather tool with location: San Francisco, CA~n"),
    
    case agent_tools:execute_tool(get_weather, Arguments) of
        {ok, Result} ->
            io:format("Success: ~s~n", [Result]);
        {error, Error} ->
            io:format("Error: ~p~n", [Error])
    end,
    
    timer:sleep(1000), % Give time for async operations
    ok.