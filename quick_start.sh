#!/bin/bash

# Quick start script for agents.erl
echo "=== Starting agents.erl ==="

# Check if release exists
if [ -d "_build/default/rel/agents" ]; then
    echo "Starting from release..."
    _build/default/rel/agents/bin/agents console
else
    echo "No release found. Starting in development mode..."
    ./rebar3 shell
fi