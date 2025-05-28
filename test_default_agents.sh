#!/bin/bash

echo "Testing default agents..."

# Start the web server in background
./start_web.sh &
SERVER_PID=$!

# Wait for server to start
sleep 5

# Test endpoints
echo -e "\n1. Listing templates:"
curl -s http://localhost:8080/api/templates | jq .

echo -e "\n2. Listing agents:"
curl -s http://localhost:8080/api/agents | jq .

echo -e "\n3. Testing agent execution:"
AGENT_ID=$(curl -s http://localhost:8080/api/agents | jq -r '.agents[0].id' 2>/dev/null)
if [ ! -z "$AGENT_ID" ] && [ "$AGENT_ID" != "null" ]; then
    echo "Testing agent $AGENT_ID..."
    curl -s -X POST http://localhost:8080/api/agents/$AGENT_ID/execute \
        -H "Content-Type: application/json" \
        -d '{"message":"Hello, what can you help me with?"}' | jq .
else
    echo "No agents found to test"
fi

# Clean up
echo -e "\nStopping server..."
kill $SERVER_PID 2>/dev/null

echo "Test complete!"