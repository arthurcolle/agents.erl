#\!/bin/bash

echo "=== LIVE DEMONSTRATION OF SELF-AWARE AGENTS ==="
echo ""
echo "Starting Erlang shell with applications loaded..."
echo ""

cd /Users/agent/agents.erl

# Start the shell with proper configuration
rebar3 shell --eval '
    io:format("~n=== SYSTEM INITIALIZATION ===~n"),
    
    % Test basic modules
    io:format("~nTesting core modules...~n"),
    
    case dynamic_supervisor_manager:start_link() of
        {ok, Pid1} -> 
            io:format("✓ Dynamic supervisor manager started~n");
        {error, {already_started, _}} -> 
            io:format("✓ Dynamic supervisor manager running~n");
        Err1 -> 
            io:format("✗ Failed to start dynamic supervisor manager: ~p~n", [Err1])
    end,
    
    case system_introspection:start_link() of
        {ok, Pid2} -> 
            io:format("✓ System introspection started~n");
        {error, {already_started, _}} -> 
            io:format("✓ System introspection running~n");
        Err2 -> 
            io:format("✗ Failed to start system introspection: ~p~n", [Err2])
    end,
    
    io:format("~n=== DEMONSTRATING SELF-AWARENESS ===~n"),
    
    % Show system state
    case system_introspection:get_system_state() of
        {ok, State} ->
            io:format("~nSystem State:~n"),
            io:format("- Node: ~p~n", [maps:get(node, State)]),
            io:format("- Total processes: ~p~n", [maps:get(total_processes, State)]),
            io:format("- Uptime: ~p seconds~n", [maps:get(uptime, State)]);
        Error ->
            io:format("Error getting system state: ~p~n", [Error])
    end,
    
    % Create a dynamic supervisor
    io:format("~nCreating dynamic supervisor...~n"),
    case dynamic_supervisor_manager:create_supervisor(test_sup, #{}) of
        {ok, SupPid} ->
            io:format("✓ Created supervisor with PID: ~p~n", [SupPid]);
        SupError ->
            io:format("✗ Failed to create supervisor: ~p~n", [SupError])
    end,
    
    io:format("~n=== SYSTEM IS READY ===~n"),
    io:format("~nThe self-aware agent system is now running\!~n"),
    io:format("~nYou can interact with the system using:~n"),
    io:format("- system_introspection:get_system_state().~n"),
    io:format("- dynamic_supervisor_manager:list_supervisors().~n"),
    io:format("- And many more self-awareness tools\!~n~n").
'
chmod +x live_demo.sh < /dev/null