%% test_fix.erl
%% Script to test the fixes for the OpenAI startup issue
-module(test_fix).
-export([test/0]).

test() ->
    io:format("Starting applications~n"),
    ok = application:ensure_all_started(agent),
    
    % Check which applications are running
    Apps = application:which_applications(),
    io:format("Running applications: ~p~n", [Apps]),
    
    % Verify openai application is running
    case lists:keyfind(openai, 1, Apps) of
        {openai, _, _} -> io:format("OpenAI application is running correctly~n");
        false -> io:format("ERROR: OpenAI application is not running~n")
    end,
    
    % Try to run the agent with tools
    io:format("~nTesting agent with tools...~n"),
    Prompt = <<"What is the current time?">>,
    ToolNames = [shell, file_read],
    
    try
        Result = agent:run_agent(Prompt, ToolNames),
        io:format("Agent result: ~p~n", [Result])
    catch
        E:R:S ->
            io:format("Error running agent: ~p:~p~n", [E, R]),
            io:format("Stack trace: ~p~n", [S])
    end,
    
    ok.