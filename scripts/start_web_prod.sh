#!/bin/bash

echo "Starting Erlang Agent Web Application..."

# Create necessary directories
mkdir -p logs

# Check if rebar3 is available
if ! command -v ./rebar3 &> /dev/null; then
    echo "Error: rebar3 not found. Please ensure rebar3 is in the current directory."
    exit 1
fi

# Compile the project
echo "Compiling project..."
./rebar3 compile

if [ $? -ne 0 ]; then
    echo "Compilation failed!"
    exit 1
fi

echo "Starting web server on http://localhost:8080"

# Write the PID to a file
echo $$ > agent_web.pid

# Start the application in production mode with minimal console output
ERL_FLAGS="+K true +A 10 +S 4:4" \
exec erl \
    -pa _build/default/lib/*/ebin \
    -config config/sys.config \
    -sname agent_web \
    -s agent_web_app start \
    -noshell \
    -noinput \
    $@ 2>&1 | tee -a logs/agent_web.log