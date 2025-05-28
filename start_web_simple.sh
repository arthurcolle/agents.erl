#!/bin/bash

echo "Starting Erlang Agent Web Application..."

# Kill any existing beam processes
pkill -f beam.smp 2>/dev/null || true
sleep 2

# Compile the project
echo "Compiling project..."
./rebar3 compile

# Start the application
echo "Starting web server on http://localhost:8080"

# Use a different approach - start with eval parameter
erl -pa _build/default/lib/*/ebin \
    -eval "application:ensure_all_started(agent_web)" \
    -eval "io:format('~nWeb server running on http://localhost:8080~nPress Ctrl+C to stop~n')" \
    -noshell