#!/bin/bash
# hot_reload.sh - Convenient script for hot reloading code

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
HOST=${HOST:-localhost}
PORT=${PORT:-8080}
API_URL="http://${HOST}:${PORT}/api/reload"

echo -e "${BLUE}🔥 Hot Code Reloader for agents.erl${NC}"
echo -e "${BLUE}====================================${NC}"

# Function to make API call
make_api_call() {
    local action="$1"
    local data="$2"
    
    if command -v curl >/dev/null 2>&1; then
        curl -s -X POST "$API_URL" \
             -H "Content-Type: application/json" \
             -d "$data"
    else
        echo -e "${RED}Error: curl not found. Please install curl to use this script.${NC}"
        exit 1
    fi
}

# Function to compile and reload a module
compile_and_reload() {
    local source_file="$1"
    
    if [ ! -f "$source_file" ]; then
        echo -e "${RED}Error: Source file '$source_file' not found${NC}"
        exit 1
    fi
    
    echo -e "${YELLOW}Compiling and reloading: $source_file${NC}"
    
    local data=$(cat <<EOF
{
    "action": "compile_and_reload",
    "source_file": "$source_file"
}
EOF
)
    
    local response=$(make_api_call "compile_and_reload" "$data")
    
    if echo "$response" | grep -q '"error"'; then
        echo -e "${RED}❌ Reload failed:${NC}"
        echo "$response" | jq '.' 2>/dev/null || echo "$response"
        exit 1
    else
        echo -e "${GREEN}✅ Successfully reloaded!${NC}"
        echo "$response" | jq '.' 2>/dev/null || echo "$response"
    fi
}

# Function to reload a specific module
reload_module() {
    local module="$1"
    
    echo -e "${YELLOW}Reloading module: $module${NC}"
    
    local data=$(cat <<EOF
{
    "action": "reload_module",
    "module": "$module"
}
EOF
)
    
    local response=$(make_api_call "reload_module" "$data")
    
    if echo "$response" | grep -q '"error"'; then
        echo -e "${RED}❌ Reload failed:${NC}"
        echo "$response" | jq '.' 2>/dev/null || echo "$response"
        exit 1
    else
        echo -e "${GREEN}✅ Module reloaded successfully!${NC}"
        echo "$response" | jq '.' 2>/dev/null || echo "$response"
    fi
}

# Function to reload all modules
reload_all() {
    echo -e "${YELLOW}Reloading all application modules...${NC}"
    
    local data='{"action": "reload_all"}'
    local response=$(make_api_call "reload_all" "$data")
    
    if echo "$response" | grep -q '"error"'; then
        echo -e "${RED}❌ Reload failed:${NC}"
        echo "$response" | jq '.' 2>/dev/null || echo "$response"
        exit 1
    else
        echo -e "${GREEN}✅ All modules reloaded!${NC}"
        echo "$response" | jq '.' 2>/dev/null || echo "$response"
    fi
}

# Function to start watching files
watch_files() {
    local files=("$@")
    
    if [ ${#files[@]} -eq 0 ]; then
        # Default files to watch
        files=(
            "apps/agents/src/agent_tools.erl"
            "apps/agents/src/agent_instance.erl"
            "apps/agent_web/src/hot_code_reloader.erl"
        )
    fi
    
    echo -e "${YELLOW}Starting file watcher for: ${files[*]}${NC}"
    
    local files_json=$(printf '"%s",' "${files[@]}")
    files_json="[${files_json%,}]"
    
    local data=$(cat <<EOF
{
    "action": "watch_files",
    "files": $files_json
}
EOF
)
    
    local response=$(make_api_call "watch_files" "$data")
    
    if echo "$response" | grep -q '"error"'; then
        echo -e "${RED}❌ Watch failed:${NC}"
        echo "$response" | jq '.' 2>/dev/null || echo "$response"
        exit 1
    else
        echo -e "${GREEN}✅ File watching started!${NC}"
        echo "$response" | jq '.' 2>/dev/null || echo "$response"
    fi
}

# Function to stop watching
stop_watch() {
    echo -e "${YELLOW}Stopping file watcher...${NC}"
    
    local data='{"action": "stop_watch"}'
    local response=$(make_api_call "stop_watch" "$data")
    
    if echo "$response" | grep -q '"error"'; then
        echo -e "${RED}❌ Stop watch failed:${NC}"
        echo "$response" | jq '.' 2>/dev/null || echo "$response"
        exit 1
    else
        echo -e "${GREEN}✅ File watching stopped!${NC}"
        echo "$response" | jq '.' 2>/dev/null || echo "$response"
    fi
}

# Function to get module info
get_module_info() {
    local module="$1"
    
    echo -e "${YELLOW}Getting info for module: $module${NC}"
    
    local data=$(cat <<EOF
{
    "action": "get_module_info",
    "module": "$module"
}
EOF
)
    
    local response=$(make_api_call "get_module_info" "$data")
    echo "$response" | jq '.' 2>/dev/null || echo "$response"
}

# Function to show usage
usage() {
    echo -e "${BLUE}Usage:${NC}"
    echo "  $0 compile <source_file>     - Compile and reload a source file"
    echo "  $0 reload <module>           - Reload a specific module"
    echo "  $0 reload-all                - Reload all application modules"
    echo "  $0 watch [files...]          - Start watching files for changes"
    echo "  $0 stop-watch                - Stop file watching"
    echo "  $0 info <module>             - Get module information"
    echo ""
    echo -e "${BLUE}Examples:${NC}"
    echo "  $0 compile apps/agents/src/agent_tools.erl"
    echo "  $0 reload agent_tools"
    echo "  $0 reload-all"
    echo "  $0 watch apps/agents/src/*.erl"
    echo "  $0 info agent_tools"
    echo ""
    echo -e "${BLUE}Environment Variables:${NC}"
    echo "  HOST - Server host (default: localhost)"
    echo "  PORT - Server port (default: 8080)"
}

# Main script logic
case "${1:-}" in
    compile)
        if [ -z "${2:-}" ]; then
            echo -e "${RED}Error: Source file required${NC}"
            usage
            exit 1
        fi
        compile_and_reload "$2"
        ;;
    reload)
        if [ -z "${2:-}" ]; then
            echo -e "${RED}Error: Module name required${NC}"
            usage
            exit 1
        fi
        reload_module "$2"
        ;;
    reload-all)
        reload_all
        ;;
    watch)
        shift
        watch_files "$@"
        ;;
    stop-watch)
        stop_watch
        ;;
    info)
        if [ -z "${2:-}" ]; then
            echo -e "${RED}Error: Module name required${NC}"
            usage
            exit 1
        fi
        get_module_info "$2"
        ;;
    help|--help|-h)
        usage
        ;;
    *)
        echo -e "${RED}Error: Unknown command '${1:-}'${NC}"
        echo ""
        usage
        exit 1
        ;;
esac