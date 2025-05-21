-module(test_agent).
-export([main/1]).

main(_) ->
    io:format("Testing agent functionality...~n"),
    
    % Ensure application is started
    application:ensure_all_started(agent),
    
    % Test a simple agent call with no tools
    io:format("Running a simple agent call...~n"),
    Result = agent:run_agent(<<"Hello, world!">>, [], #{timeout => 10000}),
    io:format("Agent response: ~p~n", [Result]),
    
    ok.