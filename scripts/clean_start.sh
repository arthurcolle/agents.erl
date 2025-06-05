#!/bin/bash

# Clean Start Script - Kills existing ERTS processes before starting

# Script location
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${YELLOW}=== Erlang/OTP Clean Start Script ===${NC}"
echo -e "${YELLOW}Project root: $PROJECT_ROOT${NC}"

# Function to kill processes
kill_erlang_processes() {
    echo -e "\n${YELLOW}Checking for existing Erlang processes...${NC}"
    
    # Find all beam.smp processes (Erlang VM)
    BEAM_PIDS=$(pgrep -f "beam.smp.*agent@matrix" || true)
    
    if [ -n "$BEAM_PIDS" ]; then
        echo -e "${RED}Found existing Erlang processes:${NC}"
        echo "$BEAM_PIDS"
        
        echo -e "${YELLOW}Killing processes...${NC}"
        for pid in $BEAM_PIDS; do
            kill -TERM $pid 2>/dev/null || true
            echo -e "${GREEN}Killed process $pid${NC}"
        done
        
        # Wait a moment for processes to terminate
        sleep 2
        
        # Force kill if still running
        REMAINING=$(pgrep -f "beam.smp.*agent@matrix" || true)
        if [ -n "$REMAINING" ]; then
            echo -e "${RED}Force killing remaining processes...${NC}"
            for pid in $REMAINING; do
                kill -9 $pid 2>/dev/null || true
            done
        fi
    else
        echo -e "${GREEN}No existing Erlang processes found${NC}"
    fi
    
    # Also check for epmd (Erlang Port Mapper Daemon)
    EPMD_PID=$(pgrep epmd || true)
    if [ -n "$EPMD_PID" ]; then
        echo -e "${YELLOW}Killing epmd (pid: $EPMD_PID)...${NC}"
        kill -TERM $EPMD_PID 2>/dev/null || true
        sleep 1
    fi
}

# Function to clean up Mnesia data if requested
clean_mnesia() {
    if [ "$1" = "--clean-mnesia" ] || [ "$1" = "-c" ]; then
        echo -e "\n${YELLOW}Cleaning Mnesia data...${NC}"
        rm -rf "$PROJECT_ROOT/Mnesia."* 2>/dev/null || true
        rm -rf "$PROJECT_ROOT/_build/default/rel/agents/Mnesia."* 2>/dev/null || true
        echo -e "${GREEN}Mnesia data cleaned${NC}"
    fi
}

# Function to clean up crash dumps
clean_crash_dumps() {
    echo -e "\n${YELLOW}Cleaning up crash dumps...${NC}"
    rm -f "$PROJECT_ROOT/erl_crash.dump" 2>/dev/null || true
    rm -f "$PROJECT_ROOT/"*.crashdump 2>/dev/null || true
    echo -e "${GREEN}Crash dumps cleaned${NC}"
}

# Main execution
echo -e "\n${YELLOW}Starting cleanup...${NC}"

# Kill existing processes
kill_erlang_processes

# Clean crash dumps
clean_crash_dumps

# Clean Mnesia if requested
clean_mnesia "$1"

echo -e "\n${GREEN}Cleanup complete!${NC}"

# Now start the application
echo -e "\n${YELLOW}Starting rebar3 shell...${NC}\n"

cd "$PROJECT_ROOT"
exec ./rebar3 shell