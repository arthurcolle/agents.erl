#!/bin/bash

echo "Starting Erlang Agent Web Application in background..."

# Create necessary directories
mkdir -p logs

# Compile the project
echo "Compiling project..."
./rebar3 compile

if [ $? -ne 0 ]; then
    echo "Compilation failed!"
    exit 1
fi

echo "Starting web server on http://localhost:8080"

# Start the application in background
nohup erl \
    -pa _build/default/lib/*/ebin \
    -config config/sys.config \
    -sname agent_web \
    -eval "
        application:ensure_all_started(openai),
        application:ensure_all_started(agents),
        application:ensure_all_started(agent_web),
        io:format(\"Web server started~n\"),
        timer:sleep(infinity)
    " \
    -noshell > logs/agent_web_background.log 2>&1 &

PID=$!
echo $PID > agent_web.pid

# Wait for server to start
echo "Waiting for server to start..."
for i in {1..10}; do
    if curl -s http://localhost:8080 > /dev/null 2>&1; then
        echo "✓ Server started successfully with PID $PID"
        echo "✓ Web interface at: http://localhost:8080"
        echo ""
        echo "Monitor with: tail -f logs/agent_web_background.log"
        echo "Stop with: kill $PID"
        exit 0
    fi
    sleep 1
done

echo "✗ Server failed to start. Check logs/agent_web_background.log"
exit 1