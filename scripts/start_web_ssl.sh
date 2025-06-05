#!/bin/bash

# Start the Erlang Agent Web Application with SSL/TLS support
# This script enables HTTPS on port 8443 alongside HTTP on port 8080

echo "Starting Erlang Agent Web Application with SSL/TLS support..."

# Check if rebar3 is available
if ! command -v ./rebar3 &> /dev/null; then
    echo "Error: rebar3 not found. Please ensure rebar3 is in the current directory."
    exit 1
fi

# Check if certificates exist, generate if needed
if [ ! -f "./certs/localhost.crt" ] || [ ! -f "./certs/localhost.key" ]; then
    echo "SSL certificates not found. Generating them now..."
    ./generate_certs.sh
fi

# Compile the project
echo "Compiling project..."
./rebar3 compile

echo "Starting web server with SSL enabled..."
echo "HTTP:  http://localhost:8080"
echo "HTTPS: https://localhost:8443"
echo "Press Ctrl+C twice to stop the server."

# Start with SSL configuration using rebar3 shell
export ERL_FLAGS="+JMsingle true +S 4:4"
./rebar3 shell --apps agents,openai,agent_web --config config/ssl