#!/bin/bash

# Clear screen and show clean startup
clear
echo "🚀 Starting Agents.erl Web Application..."
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

# Check if rebar3 is available
if ! command -v ./rebar3 &> /dev/null; then
    echo "❌ Error: rebar3 not found"
    exit 1
fi

# Set default port if not specified
PORT=${PORT:-8080}

# Quick compile with reduced output
echo "📦 Compiling..."
./rebar3 compile 2>/dev/null | grep -v "Compiling" | head -5

echo "🌐 Web server: http://localhost:$PORT"
echo "🔧 Dashboard: http://localhost:$PORT/dashboard"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

# Start with minimal logging and clean output
if [ "$1" = "perf" ]; then
    export ERL_FLAGS="+JPperf true +JPfp true +W w"
    ./rebar3 as perf shell --apps agents,openai,agent_web --config config/sys.config 2>/dev/null
elif [ "$1" = "prod" ]; then
    export ERL_FLAGS="+JMsingle true +S 4:4 +W w"
    ./rebar3 as prod shell --apps agents,openai,agent_web --config config/sys.config 2>/dev/null
else
    # Clean development mode
    export ERL_FLAGS="+JMsingle true +S 2:2 +W w"
    ./rebar3 shell --apps agents,openai,agent_web --config config/sys.config 2>/dev/null
fi