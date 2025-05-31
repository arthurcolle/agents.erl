#!/bin/bash

# Start the Erlang Agent Web Application with SSL/TLS support
# This script enables HTTPS on port 8443 alongside HTTP on port 8080

echo "Starting Erlang Agent Web Application with SSL/TLS support..."

# Check if certificates exist, generate if needed
if [ ! -f "./certs/localhost.crt" ] || [ ! -f "./certs/localhost.key" ]; then
    echo "SSL certificates not found. Generating them now..."
    ./generate_certs.sh
fi

echo "Starting web server with SSL enabled..."
echo "HTTP:  http://localhost:8080"
echo "HTTPS: https://localhost:8443"
echo "Press Ctrl+C twice to stop the server."

# Start with SSL configuration
ERL_CRASH_DUMP_SECONDS=0 erl \
    -pa _build/default/lib/*/ebin \
    -pa apps/*/ebin \
    -config config/ssl \
    -s agent_web \
    -eval "application:ensure_all_started(agent_web)."