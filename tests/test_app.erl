-module(test_app).
-export([test/0]).

test() ->
    % Start the applications
    io:format("Starting applications...~n"),
    {ok, _} = application:ensure_all_started(agent),
    
    % Print status
    io:format("Applications running: ~p~n", [application:which_applications()]),
    
    % Check if key processes are running
    io:format("agent_registry is running: ~p~n", [whereis(agent_registry) =/= undefined]),
    io:format("agent_tools is running: ~p~n", [whereis(agent_tools) =/= undefined]),
    io:format("openai_sup is running: ~p~n", [whereis(openai_sup) =/= undefined]),
    
    % Success
    ok.