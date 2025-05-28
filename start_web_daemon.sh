#!/bin/bash

echo "Starting Erlang Agent Web Application in daemon mode..."

# Check if rebar3 is available
if ! command -v ./rebar3 &> /dev/null; then
    echo "Error: rebar3 not found. Please ensure rebar3 is in the current directory."
    exit 1
fi

# Kill any existing beam processes on port 8080
echo "Checking for existing processes on port 8080..."
EXISTING_PID=$(lsof -ti :8080)
if [ ! -z "$EXISTING_PID" ]; then
    echo "Killing existing process $EXISTING_PID on port 8080"
    kill $EXISTING_PID
    sleep 2
fi

# Compile the project
echo "Compiling project..."
./rebar3 compile

# Start the application in background
echo "Starting web server on http://localhost:8080"
echo "Application will run in background. Check logs for status."

# Start with daemon mode using erl directly
erl -pa _build/default/lib/*/ebin \
    -boot start_sasl \
    -config rel/sys \
    -s agent_web_app \
    -detached \
    -noshell \
    -name agent_web@localhost

echo "Web application started in background mode."
echo "Access the application at: http://localhost:8080"
echo "To stop: kill \$(pgrep -f 'agent_web@localhost')"