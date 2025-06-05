#!/bin/bash

# Colored Log Viewer Script for Agents.erl System
# This script adds ANSI color codes to log files for better readability

# Color codes
RED='\033[31m'
YELLOW='\033[33m'
GREEN='\033[32m'
BLUE='\033[34m'
CYAN='\033[36m'
MAGENTA='\033[35m'
BOLD='\033[1m'
RESET='\033[0m'

# Function to colorize logs
colorize_logs() {
    local logfile="$1"
    
    if [[ ! -f "$logfile" ]]; then
        echo -e "${RED}Error: Log file '$logfile' not found${RESET}"
        return 1
    fi
    
    # Read the log file and apply colors based on patterns
    sed -E \
        -e "s/(ERROR|error|Error|failed|Failed|FAILED|crash|Crash|CRASH|exception|Exception)/\\${RED}&\\${RESET}/g" \
        -e "s/(WARNING|warning|Warning|WARN|warn|Warn)/\\${YELLOW}&\\${RESET}/g" \
        -e "s/(SUCCESS|success|Success|started|Started|STARTED|complete|Complete|COMPLETE|ready|Ready|READY)/\\${GREEN}&\\${RESET}/g" \
        -e "s/(HEALTH|health|Health|check|Check|CHECK|monitor|Monitor|MONITOR|ping|Ping|PING)/\\${BLUE}&\\${RESET}/g" \
        -e "s/(API|api|GET|POST|PUT|DELETE|PATCH|HTTP|http|request|Request|REQUEST)/\\${CYAN}&\\${RESET}/g" \
        -e "s/(CRITICAL|critical|Critical|URGENT|urgent|Urgent|ALERT|alert|Alert)/\\${MAGENTA}\\${BOLD}&\\${RESET}/g" \
        -e "s/(\[.*\])/\\${BOLD}&\\${RESET}/g" \
        -e "s/([0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2})/\\${CYAN}&\\${RESET}/g" \
        -e "s/(PID [<][0-9.>]+)/\\${BLUE}&\\${RESET}/g" \
        -e "s/(port [0-9]+)/\\${GREEN}&\\${RESET}/g" \
        "$logfile"
}

# Function to tail logs with colors
tail_colored_logs() {
    local logfile="$1"
    local lines="${2:-50}"
    
    if [[ ! -f "$logfile" ]]; then
        echo -e "${RED}Error: Log file '$logfile' not found${RESET}"
        return 1
    fi
    
    tail -f -n "$lines" "$logfile" | while read line; do
        echo "$line" | sed -E \
            -e "s/(ERROR|error|Error|failed|Failed|FAILED|crash|Crash|CRASH|exception|Exception)/\\${RED}&\\${RESET}/g" \
            -e "s/(WARNING|warning|Warning|WARN|warn|Warn)/\\${YELLOW}&\\${RESET}/g" \
            -e "s/(SUCCESS|success|Success|started|Started|STARTED|complete|Complete|COMPLETE|ready|Ready|READY)/\\${GREEN}&\\${RESET}/g" \
            -e "s/(HEALTH|health|Health|check|Check|CHECK|monitor|Monitor|MONITOR|ping|Ping|PING)/\\${BLUE}&\\${RESET}/g" \
            -e "s/(API|api|GET|POST|PUT|DELETE|PATCH|HTTP|http|request|Request|REQUEST)/\\${CYAN}&\\${RESET}/g" \
            -e "s/(CRITICAL|critical|Critical|URGENT|urgent|Urgent|ALERT|alert|Alert)/\\${MAGENTA}\\${BOLD}&\\${RESET}/g" \
            -e "s/(\[.*\])/\\${BOLD}&\\${RESET}/g" \
            -e "s/([0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2})/\\${CYAN}&\\${RESET}/g" \
            -e "s/(PID [<][0-9.>]+)/\\${BLUE}&\\${RESET}/g" \
            -e "s/(port [0-9]+)/\\${GREEN}&\\${RESET}/g"
    done
}

# Function to show help
show_help() {
    echo -e "${BOLD}Colored Log Viewer for Agents.erl System${RESET}"
    echo ""
    echo -e "${GREEN}Usage:${RESET}"
    echo "  $0 view <logfile>           # View entire log file with colors"
    echo "  $0 tail <logfile> [lines]   # Tail log file with colors (default: 50 lines)"
    echo "  $0 server                   # View server.log"
    echo "  $0 web                      # View web.log"
    echo "  $0 app                      # View app.log"
    echo "  $0 all                      # View all log files"
    echo ""
    echo -e "${GREEN}Color Legend:${RESET}"
    echo -e "  ${RED}Red${RESET}     - Errors, failures, crashes"
    echo -e "  ${YELLOW}Yellow${RESET}  - Warnings"
    echo -e "  ${GREEN}Green${RESET}   - Success messages, startup events"
    echo -e "  ${BLUE}Blue${RESET}    - Health checks, monitoring"
    echo -e "  ${CYAN}Cyan${RESET}    - API calls, HTTP requests"
    echo -e "  ${MAGENTA}${BOLD}Magenta${RESET} - Critical system events"
}

# Main script logic
case "$1" in
    "view")
        if [[ -z "$2" ]]; then
            echo -e "${RED}Error: Please specify a log file${RESET}"
            show_help
            exit 1
        fi
        colorize_logs "$2"
        ;;
    "tail")
        if [[ -z "$2" ]]; then
            echo -e "${RED}Error: Please specify a log file${RESET}"
            show_help
            exit 1
        fi
        tail_colored_logs "$2" "$3"
        ;;
    "server")
        if [[ -f "server.log" ]]; then
            colorize_logs "server.log"
        else
            echo -e "${RED}Error: server.log not found${RESET}"
        fi
        ;;
    "web")
        if [[ -f "web.log" ]]; then
            colorize_logs "web.log"
        else
            echo -e "${RED}Error: web.log not found${RESET}"
        fi
        ;;
    "app")
        if [[ -f "app.log" ]]; then
            colorize_logs "app.log"
        else
            echo -e "${RED}Error: app.log not found${RESET}"
        fi
        ;;
    "all")
        for logfile in server.log web.log app.log web_server.log startup.log; do
            if [[ -f "$logfile" ]]; then
                echo -e "${BOLD}${GREEN}=== $logfile ===${RESET}"
                colorize_logs "$logfile"
                echo ""
            fi
        done
        ;;
    "help"|"-h"|"--help"|"")
        show_help
        ;;
    *)
        echo -e "${RED}Error: Unknown command '$1'${RESET}"
        show_help
        exit 1
        ;;
esac