#!/bin/bash

echo "Starting Distributed Systems with BeamAsm JIT..."

# Check if rebar3 is available
if ! command -v ./rebar3 &> /dev/null; then
    echo "Error: rebar3 not found. Please ensure rebar3 is in the current directory."
    exit 1
fi

# Create necessary directories
mkdir -p crash_dumps
mkdir -p logs

# Compile the project (development mode)
echo "Compiling with development mode..."
./rebar3 compile

if [ $? -ne 0 ]; then
    echo "Compilation failed!"
    exit 1
fi

echo "=== BeamAsm JIT Configuration ==="
echo "✓ Development compilation"
echo "✓ Hot code reloading enabled"
echo "✓ Debug info included"
echo ""

# Start the application using system Erlang
echo "Starting development release with system Erlang..."
echo "Web server will be available at http://localhost:8080"
echo "Logs will be written to logs/"
echo "Press Ctrl+C to stop the server."

# Set environment
export ERL_CRASH_DUMP=crash_dumps/dev_crash.dump
export ERL_CRASH_DUMP_SECONDS=30

# Use system Erlang with the compiled beams
exec ./rebar3 shell \
    --config config/sys.config \
    --sname agent_web \
    --setcookie agent_cookie \
    --eval "application:ensure_all_started(agent_web)"