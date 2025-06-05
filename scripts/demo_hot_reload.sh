#!/bin/bash
# demo_hot_reload.sh - Demonstrate hot code reloading capabilities

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
NC='\033[0m' # No Color

echo -e "${BLUE}🔥 Hot Code Reloading Demo for agents.erl${NC}"
echo -e "${BLUE}===========================================${NC}"
echo ""

# Check if the system is running
HOST=${HOST:-localhost}
PORT=${PORT:-8080}
API_URL="http://${HOST}:${PORT}/api/reload"

echo -e "${YELLOW}Checking if agents.erl system is running...${NC}"
if curl -s "$API_URL" > /dev/null 2>&1; then
    echo -e "${GREEN}✅ System is running on $HOST:$PORT${NC}"
else
    echo -e "${RED}❌ System not running. Please start with: ./start_web.sh${NC}"
    exit 1
fi

echo ""
echo -e "${PURPLE}Demo 1: Get current module information${NC}"
echo -e "${YELLOW}Getting info for agent_tools module...${NC}"
./hot_reload.sh info agent_tools | head -10

echo ""
echo -e "${PURPLE}Demo 2: Reload specific module${NC}"
echo -e "${YELLOW}Reloading agent_tools module...${NC}"
./hot_reload.sh reload agent_tools

echo ""
echo -e "${PURPLE}Demo 3: Compile and reload from source${NC}"
echo -e "${YELLOW}Compiling and reloading hot_code_reloader...${NC}"
./hot_reload.sh compile apps/agent_web/src/hot_code_reloader.erl

echo ""
echo -e "${PURPLE}Demo 4: Reload all application modules${NC}"
echo -e "${YELLOW}Reloading all application modules...${NC}"
./hot_reload.sh reload-all

echo ""
echo -e "${PURPLE}Demo 5: File watching (5 seconds)${NC}"
echo -e "${YELLOW}Starting file watcher for 5 seconds...${NC}"
./hot_reload.sh watch apps/agents/src/agent_tools.erl &
WATCH_PID=$!

echo "File watching is active. Try modifying apps/agents/src/agent_tools.erl in another terminal..."
sleep 5

echo -e "${YELLOW}Stopping file watcher...${NC}"
./hot_reload.sh stop-watch
wait $WATCH_PID 2>/dev/null || true

echo ""
echo -e "${GREEN}🎉 Hot reload demo completed!${NC}"
echo ""
echo -e "${BLUE}Key Benefits:${NC}"
echo "• ⚡ Zero-downtime code updates"
echo "• 🔍 Live system introspection"
echo "• 📱 Multiple interfaces (CLI, HTTP API, Agent Tools)"
echo "• 🎯 Automatic file watching"
echo "• 🔄 Selective or batch reloading"
echo ""
echo -e "${BLUE}Usage in production:${NC}"
echo "1. Make code changes"
echo "2. Use hot_reload.sh to update running system"
echo "3. Verify changes with module info"
echo "4. No service interruption!"