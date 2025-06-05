%% Minimal test to verify basic functionality
-module(minimal_test).
-export([start/0]).

start() ->
    io:format("Minimal test starting...~n"),
    
    % Try to load agent module
    case code:ensure_loaded(agent) of
        {module, agent} ->
            io:format("✓ Agent module loaded~n");
        Error ->
            io:format("✗ Failed to load agent module: ~p~n", [Error])
    end,
    
    % Try to load agent_tools module  
    case code:ensure_loaded(agent_tools) of
        {module, agent_tools} ->
            io:format("✓ Agent tools module loaded~n");
        Error2 ->
            io:format("✗ Failed to load agent_tools module: ~p~n", [Error2])
    end,
    
    io:format("Test completed~n").