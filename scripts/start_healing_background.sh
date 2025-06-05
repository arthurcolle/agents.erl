#!/bin/bash

# Start Auto-Healing System in Background
echo "Starting Auto-Healing Agent System in background..."

# Clean up
pkill -f "beam.*agent_web" 2>/dev/null || true
sleep 1

# Start in background using nohup
nohup erl \
    -sname agent_web \
    -pa _build/default/lib/*/ebin \
    -config config/sys.config \
    -noshell \
    -noinput \
    -eval "application:ensure_all_started(agent_web), auto_healing_startup:start_with_healing(), timer:sleep(infinity)." \
    > logs/auto_healing.log 2>&1 &

# Save PID
echo $! > agent_web.pid

sleep 3

# Check if running
if kill -0 $(cat agent_web.pid) 2>/dev/null; then
    echo "✓ System started with PID $(cat agent_web.pid)"
    echo "✓ Web interface at: http://localhost:8080"
    echo ""
    echo "Monitor with: tail -f logs/auto_healing.log"
    echo "Stop with: kill $(cat agent_web.pid)"
else
    echo "✗ Failed to start. Check logs/auto_healing.log"
    exit 1
fi