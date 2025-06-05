#!/bin/bash

echo "Starting Erlang Agent Web Application in background..."

# Kill any existing beam processes
pkill -f "agent_web" 2>/dev/null || true
sleep 2

# Compile the project
echo "Compiling project..."
./rebar3 compile

# Start the application in background
echo "Starting web server on http://localhost:8080"

# Start with detached mode
nohup erl -pa _build/default/lib/*/ebin \
    -eval "application:ensure_all_started(agent_web)" \
    -eval "io:format('Web server running on http://localhost:8080~n')" \
    -noshell \
    > web_server.log 2>&1 &

# Get the process ID
WEB_PID=$!
echo "Web server started with PID: $WEB_PID"
echo "Logs available in: web_server.log"
echo "To stop: kill $WEB_PID"

# Wait a moment and check if it's running
sleep 3
if kill -0 $WEB_PID 2>/dev/null; then
    echo "Web server is running successfully!"
    echo "Access the application at: http://localhost:8080"
else
    echo "Warning: Web server may have failed to start. Check web_server.log"
fi