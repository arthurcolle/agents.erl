#!/bin/bash

echo "Starting Erlang Agent Web Application..."

# Check if rebar3 is available
if ! command -v ./rebar3 &> /dev/null; then
    echo "Error: rebar3 not found. Please ensure rebar3 is in the current directory."
    exit 1
fi

# Compile the project
echo "Compiling project..."
./rebar3 compile

# Start the application
echo "Starting web server on http://localhost:8080"
echo "Press Ctrl+C twice to stop the server."

# Start with shell for interactive development
./rebar3 shell --apps agent,openai,agent_web