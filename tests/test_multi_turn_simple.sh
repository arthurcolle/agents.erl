#!/bin/bash
cd /Users/agent/agents.erl

echo "Starting multi-turn function calling test..."

# Start rebar3 shell and run test
./rebar3 shell <<'EOF'
% Compile test module
compile:file("test_multi_turn_shell.erl").

% Ensure agent_supervisor is running
case whereis(agent_supervisor) of
    undefined -> 
        io:format("Starting agent_supervisor...~n"),
        agent_supervisor:start_link();
    Pid -> 
        io:format("Agent supervisor already running: ~p~n", [Pid])
end.

% Run the test
test_multi_turn_shell:test().

% Exit after a short delay
timer:sleep(2000).
q().
EOF