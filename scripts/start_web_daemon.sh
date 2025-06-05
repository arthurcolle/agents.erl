#!/bin/bash

echo "Starting Erlang Agent Web Application as daemon..."

# Check if rebar3 is available
if ! command -v ./rebar3 &> /dev/null; then
    echo "Error: rebar3 not found. Please ensure rebar3 is in the current directory."
    exit 1
fi

# Compile the project
echo "Compiling project..."
./rebar3 compile

# Set default port if not specified
PORT=${PORT:-8080}

# Start the application as a daemon
echo "Starting web server on http://localhost:$PORT as daemon"

# Use erl directly with -detached flag
erl -pa _build/default/lib/*/ebin \
    -config config/sys.config \
    -sname agent_web \
    -setcookie agent_web_cookie \
    -noshell \
    -detached \
    -eval "application:ensure_all_started(agents), application:ensure_all_started(openai), application:ensure_all_started(agent_web)."

echo "Server started in background. Check logs for details."
echo "To stop: pkill -f 'sname agent_web'"