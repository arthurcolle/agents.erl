#!/bin/bash

# Start Auto-Healing System
# This script starts the agents.erl system with auto-healing capabilities

echo "Starting Auto-Healing Agent System..."

# Set up environment
export ERL_FLAGS="+P 1000000 +Q 65536"
export TERM=xterm-256color

# Ensure logs directory exists
mkdir -p logs

# Clean up any previous runs
pkill -f "beam.*agent_web" 2>/dev/null || true
sleep 1

# Start the system in detached mode with auto-healing
echo "Launching system with auto-healing supervisor..."
erl -detached \
    -sname agent_web \
    -pa _build/default/lib/*/ebin \
    -config config/sys.config \
    -boot start_sasl \
    -s agent_web \
    -s auto_healing_startup start_with_healing \
    -noshell \
    -noinput \
    > logs/auto_healing.log 2>&1

# Give it a moment to start
sleep 3

# Check if it's running
if pgrep -f "beam.*agent_web" > /dev/null; then
    echo "✓ Auto-healing system started successfully!"
    echo "✓ Web interface available at: http://localhost:8080"
    echo ""
    echo "To monitor the system:"
    echo "  tail -f logs/*.log"
    echo ""
    echo "To check auto-healing status:"
    echo "  tail -f logs/*.log | grep -E 'AUTO_HEALING|RESILIENCE|SELF_HEALING'"
    echo ""
    echo "To stop the system:"
    echo "  pkill -f 'beam.*agent_web'"
else
    echo "✗ Failed to start the system. Check logs/auto_healing.log for details."
    exit 1
fi