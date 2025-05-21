-module(test_shell).
-export([main/1, check/0]).

main(_) ->
    check().

check() ->
    io:format("Checking application status in shell...~n"),
    
    % Attempt to start agent
    application:ensure_all_started(agent),
    
    % Check applications
    Apps = application:which_applications(),
    io:format("Applications running: ~p~n", [Apps]),
    
    % Check processes
    io:format("agent: ~p~n", [whereis(agent) =/= undefined]),
    io:format("agent_registry: ~p~n", [whereis(agent_registry) =/= undefined]),
    io:format("agent_tools: ~p~n", [whereis(agent_tools) =/= undefined]),
    io:format("openai_sup: ~p~n", [whereis(openai_sup) =/= undefined]),
    
    ok.