#!/bin/bash

# Test MCP web server startup

echo "Starting MCP web server on port 8765..."
./scripts/start_mcp web 8765 &
SERVER_PID=$!

echo "Waiting for server to start..."
sleep 5

echo "Testing server availability..."
if curl -s -o /dev/null -w "%{http_code}" http://localhost:8765/api/system/health | grep -q "200"; then
    echo "✅ Server is running and healthy!"
    echo "Web interface: http://localhost:8765"
    echo "MCP WebSocket: ws://localhost:8765/mcp"
else
    echo "❌ Server failed to start properly"
fi

echo "Server PID: $SERVER_PID"
echo "To stop the server: kill $SERVER_PID"