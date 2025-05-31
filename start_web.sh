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

# Set default port if not specified
PORT=${PORT:-8080}

# Start the application
echo "Starting web server on http://localhost:$PORT"
echo "Press Ctrl+C twice to stop the server."

# Check if we should use perf profiling
if [ "$1" = "perf" ]; then
    echo "Starting with perf monitoring enabled..."
    export ERL_FLAGS="+JPperf true +JPfp true"
    ./rebar3 as perf shell --apps agents,openai,agent_web --config config/sys.config
elif [ "$1" = "prod" ]; then
    echo "Starting in production mode with JIT optimizations..."
    ./rebar3 as prod shell --apps agents,openai,agent_web --config config/sys.config
else
    # Start with shell for interactive development with basic JIT optimizations
    export ERL_FLAGS="+JMsingle true +S 4:4"
    ./rebar3 shell --apps agents,openai,agent_web --config config/sys.config
fi