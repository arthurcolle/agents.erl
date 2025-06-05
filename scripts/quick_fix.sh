#!/bin/bash

echo "Applying quick fixes to running system..."

# Kill any existing beam processes to ensure clean start
echo "Stopping existing processes..."
pkill -f beam.smp || true
sleep 2

# Compile with the fixes
echo "Compiling with auto-healing modules..."
rebar3 compile

# Start with basic error handling
echo "Starting system with error resilience..."
erl -pa _build/default/lib/*/ebin \
    -name agents@localhost \
    -setcookie agents_cookie \
    -config config/sys.config \
    -eval '
        %% Load resilience modules
        code:load_file(error_resilience),
        code:load_file(self_healing_supervisor),
        code:load_file(auto_healing_startup),
        
        %% Start applications
        application:ensure_all_started(agent_web),
        application:ensure_all_started(agents),
        
        %% Apply auto-healing
        auto_healing_startup:ensure_all_systems_running(),
        
        io:format("~n~nSystem started with auto-healing enabled!~n"),
        io:format("All errors should now be automatically handled.~n~n").
    ' -noshell &

echo ""
echo "System is starting with auto-healing..."
echo "Check the web interface at http://localhost:8080"
echo ""
echo "To monitor the system:"
echo "  tail -f *.log | grep -E 'AUTO_HEALING|RESILIENCE|SELF_HEALING'"
echo ""