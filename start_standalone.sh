#!/bin/bash

echo "🚀 Starting Agents.erl STANDALONE Mode"
echo "======================================"
echo ""

# Kill existing processes
pkill -f "beam.*agents" 2>/dev/null

# Ensure directories exist
mkdir -p logs crash_dumps

echo "📋 Standalone Configuration:"
echo "   Node: agents@localhost"
echo "   Web UI: http://localhost:8080"
echo "   Mode: Single machine"
echo ""

echo "🔧 Starting standalone system..."

exec erl \
  -sname agents \
  -pa _build/default/lib/*/ebin \
  -config config/sys \
  -eval "
    io:format('🔧 Starting applications...~n'),
    application:ensure_all_started(openai),
    application:ensure_all_started(agents),
    application:ensure_all_started(agent_web),
    io:format('~n✅ STANDALONE SYSTEM READY!~n'),
    io:format('🌐 Web interface: http://localhost:8080~n'),
    io:format('🎯 Single machine mode - no clustering~n'),
    io:format('~n')." \
  +JMsingle true \
  +MBas aobf \
  +MHas aobf \
  +MMmcs 30 \
  +P 1048576 \
  +Q 1048576 \
  +S 4:4 \
  +A 32 \
  +K true \
  +c true