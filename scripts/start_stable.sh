#!/bin/bash

# Stable startup script with all fixes applied

echo "Starting Erlang Agent Web Application (Stable Mode)..."

# Set environment variables for MCP servers if not already set
export SLACK_BOT_TOKEN=${SLACK_BOT_TOKEN:-"xoxb-dummy-token"}
export SLACK_TEAM_ID=${SLACK_TEAM_ID:-"T00000000"}
export LINEAR_API_KEY=${LINEAR_API_KEY:-"lin_api_dummy_key"}
export PLAID_CLIENT_ID=${PLAID_CLIENT_ID:-"dummy_client_id"}
export PLAID_SECRET=${PLAID_SECRET:-"dummy_secret"}
export PLAID_ENV=${PLAID_ENV:-"sandbox"}
export GRAPHLIT_API_KEY=${GRAPHLIT_API_KEY:-"dummy_graphlit_key"}
export GITHUB_TOKEN=${GITHUB_TOKEN:-"ghp_dummy_token"}

# Ensure we're in the project directory
cd "$(dirname "$0")/.." || exit 1

# Build frontend if needed
if [ ! -d "apps/agent_web/priv/static/dist" ]; then
    echo "Building frontend..."
    (cd apps/agent_web/frontend && npm install && npm run build)
fi

# Compile the project
echo "Compiling project..."
./rebar3 compile

# Start the application with proper app ordering
echo "Starting web server on http://localhost:8080"
echo "Press Ctrl+C twice to stop the server."
echo ""
echo "Logs will appear below:"
echo "======================"

# Start with explicit app loading order to avoid OpenTelemetry issues
./rebar3 shell \
    --apps opentelemetry_api,agents,openai,agent_web \
    --eval "application:ensure_all_started(agent_web)."