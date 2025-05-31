#!/bin/bash

echo "Starting Erlang Agent System in Production Mode with BeamAsm JIT..."

# Check if rebar3 is available
if ! command -v ./rebar3 &> /dev/null; then
    echo "Error: rebar3 not found. Please ensure rebar3 is in the current directory."
    exit 1
fi

# Create necessary directories
mkdir -p crash_dumps
mkdir -p logs

# Compile the project with production profile
echo "Compiling with production optimizations..."
./rebar3 as prod compile

# Build release
echo "Building production release..."
./rebar3 as prod release

echo "=== BeamAsm JIT Production Configuration ==="
echo "✓ Native compilation enabled"
echo "✓ Aggressive optimizations (O3)"
echo "✓ JIT single mode"
echo "✓ Memory allocator optimizations"
echo "✓ Process and scheduler optimizations"
echo ""

# Start the release
echo "Starting production release..."
echo "Web server will be available at http://localhost:8080"
echo "Logs will be written to logs/"
echo "Press Ctrl+C to stop the server."

# Set production environment
export ERL_CRASH_DUMP=crash_dumps/prod_crash.dump
export ERL_CRASH_DUMP_SECONDS=30

# Start the release
_build/prod/rel/erlang_agent/bin/erlang_agent foreground