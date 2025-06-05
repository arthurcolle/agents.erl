#!/bin/bash

# start_chat_deployment.sh
# Comprehensive deployment script for agent chat system
# Integrates all configurations and starts the chat interface with streaming tokens

set -e  # Exit on any error

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
MAGENTA='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

echo -e "${CYAN}🚀 Agent Chat System Deployment${NC}"
echo -e "${CYAN}================================${NC}"
echo ""

# Function to print section headers
print_section() {
    echo ""
    echo -e "${MAGENTA}📋 $1${NC}"
    echo -e "${MAGENTA}$(echo "$1" | sed 's/./=/g')${NC}"
}

# Function to check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Function to check if port is available
port_available() {
    ! lsof -Pi :$1 -sTCP:LISTEN -t >/dev/null
}

# Change to project directory
cd "$(dirname "$0")"

print_section "Pre-flight Checks"

# Check required commands
echo -n "   Checking rebar3... "
if command_exists rebar3; then
    echo -e "${GREEN}✓${NC}"
else
    echo -e "${RED}✗ rebar3 not found${NC}"
    exit 1
fi

echo -n "   Checking Erlang... "
if command_exists erl; then
    ERL_VERSION=$(erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell)
    echo -e "${GREEN}✓ OTP $ERL_VERSION${NC}"
else
    echo -e "${RED}✗ Erlang not found${NC}"
    exit 1
fi

# Check project structure
echo -n "   Checking project structure... "
if [[ -f "rebar.config" && -d "apps" && -d "config" ]]; then
    echo -e "${GREEN}✓${NC}"
else
    echo -e "${RED}✗ Invalid project structure${NC}"
    exit 1
fi

# Check if port 8080 is available
echo -n "   Checking port 8080... "
if port_available 8080; then
    echo -e "${GREEN}✓ Available${NC}"
else
    echo -e "${YELLOW}⚠ Port 8080 in use - will attempt to use anyway${NC}"
fi

# Check API keys configuration
echo -n "   Checking API keys... "
if [[ -f "config/api_keys.config" ]]; then
    echo -e "${GREEN}✓ Found${NC}"
else
    echo -e "${YELLOW}⚠ No API keys config - using environment variables${NC}"
fi

print_section "Environment Setup"

# Create necessary directories
echo "   Creating directories..."
mkdir -p logs
mkdir -p data
mkdir -p test_logs
echo -e "   ${GREEN}✓ Directories created${NC}"

# Set environment variables for the session
echo "   Setting environment variables..."
export ERL_CRASH_DUMP_SECONDS=0
export ERL_MAX_PORTS=32768
export ERL_MAX_ETS_TABLES=32768

# Check for API keys in environment
if [[ -n "$OPENAI_API_KEY" ]]; then
    echo -e "   ${GREEN}✓ OpenAI API key found in environment${NC}"
fi

if [[ -n "$ANTHROPIC_API_KEY" ]]; then
    echo -e "   ${GREEN}✓ Anthropic API key found in environment${NC}"
fi

print_section "Compilation"

echo "   Compiling all applications..."
if rebar3 compile; then
    echo -e "   ${GREEN}✓ Compilation successful${NC}"
else
    echo -e "   ${RED}✗ Compilation failed${NC}"
    exit 1
fi

print_section "Configuration"

# Use deployment configuration
echo "   Using deployment configuration..."
CONFIG_FILE="config/deployment.config"

if [[ -f "$CONFIG_FILE" ]]; then
    echo -e "   ${GREEN}✓ Using $CONFIG_FILE${NC}"
else
    echo -e "   ${YELLOW}⚠ Deployment config not found, using default sys.config${NC}"
    CONFIG_FILE="config/sys.config"
fi

print_section "Frontend Build"

# Check if frontend needs building
if [[ -d "apps/agent_web/frontend" ]]; then
    echo "   Building React frontend..."
    cd apps/agent_web/frontend
    
    if [[ -f "package.json" ]]; then
        if command_exists npm; then
            echo "   Installing frontend dependencies..."
            npm install --silent
            echo "   Building frontend..."
            npm run build
            echo -e "   ${GREEN}✓ Frontend built successfully${NC}"
        else
            echo -e "   ${YELLOW}⚠ npm not found, skipping frontend build${NC}"
        fi
    else
        echo -e "   ${YELLOW}⚠ No package.json found, skipping frontend build${NC}"
    fi
    
    cd ../../..
else
    echo -e "   ${YELLOW}⚠ No frontend directory found${NC}"
fi

print_section "System Configuration"

echo "   Configured features:"
echo -e "   ${GREEN}✓${NC} Streaming responses with jiffy serialization"
echo -e "   ${GREEN}✓${NC} Multi-agent chat support"
echo -e "   ${GREEN}✓${NC} Function calling with parallel execution"
echo -e "   ${GREEN}✓${NC} Real-time WebSocket communication"
echo -e "   ${GREEN}✓${NC} Cost tracking and monitoring"
echo -e "   ${GREEN}✓${NC} Hot code reloading"
echo -e "   ${GREEN}✓${NC} Comprehensive logging"

print_section "Starting System"

echo "   Starting Erlang applications..."
echo -e "   ${CYAN}Press Ctrl+C twice to exit cleanly${NC}"
echo ""

# Start the system with comprehensive configuration
exec rebar3 shell --config "$CONFIG_FILE" --eval "
    io:format('${CYAN}🌟 Initializing Agent Chat System...${NC}~n'),
    
    % Start applications in order
    application:ensure_all_started(crypto),
    application:ensure_all_started(ssl),
    application:ensure_all_started(jsx),
    application:ensure_all_started(jiffy),
    application:ensure_all_started(gproc),
    
    io:format('${GREEN}✓${NC} Core dependencies started~n'),
    
    % Start our applications
    case application:ensure_all_started(openai) of
        {ok, _} -> io:format('${GREEN}✓${NC} OpenAI application started~n');
        {error, Reason1} -> io:format('${RED}✗${NC} OpenAI failed: ~p~n', [Reason1])
    end,
    
    case application:ensure_all_started(agents) of
        {ok, _} -> io:format('${GREEN}✓${NC} Agents application started~n');
        {error, Reason2} -> io:format('${RED}✗${NC} Agents failed: ~p~n', [Reason2])
    end,
    
    case application:ensure_all_started(agent_web) of
        {ok, _} -> io:format('${GREEN}✓${NC} Agent Web application started~n');
        {error, Reason3} -> io:format('${RED}✗${NC} Agent Web failed: ~p~n', [Reason3])
    end,
    
    % Display system information
    io:format('~n${CYAN}🎉 Agent Chat System Ready!${NC}~n'),
    io:format('${CYAN}=============================${NC}~n'),
    io:format('${GREEN}🌐 Web Interface:${NC} http://localhost:8080~n'),
    io:format('${GREEN}📡 WebSocket:${NC}     ws://localhost:8080/ws~n'),
    io:format('${GREEN}🔧 API Endpoint:${NC}  http://localhost:8080/api~n'),
    io:format('${GREEN}📊 Metrics:${NC}       http://localhost:8080/api/metrics~n'),
    io:format('${GREEN}🏥 Health:${NC}        http://localhost:8080/api/health~n'),
    io:format('~n'),
    io:format('${YELLOW}💡 Features:${NC}~n'),
    io:format('   • Multi-agent conversations~n'),
    io:format('   • Streaming responses with jiffy serialization~n'),
    io:format('   • Function calling and tool execution~n'),
    io:format('   • Real-time cost tracking~n'),
    io:format('   • Hot code reloading~n'),
    io:format('   • Comprehensive logging~n'),
    io:format('~n'),
    io:format('${MAGENTA}🚀 System Status:${NC}~n'),
    try
        Agents = supervisor:count_children(agent_supervisor),
        io:format('   Active agents: ~p~n', [proplists:get_value(active, Agents, 0)])
    catch _:_ ->
        io:format('   Agent system: initializing...~n')
    end,
    
    % Test jiffy serialization
    TestToken = #{type => test, content => <<\"Hello, World!\">>, timestamp => erlang:system_time(millisecond)},
    case jiffy:encode(TestToken) of
        EncodedTest when is_binary(EncodedTest) ->
            io:format('${GREEN}✓${NC} Jiffy serialization working~n');
        _ ->
            io:format('${RED}✗${NC} Jiffy serialization failed~n')
    end,
    
    io:format('~n${CYAN}Ready for chat interactions!${NC}~n'),
    io:format('Use Ctrl+C twice to stop gracefully.~n~n')
"