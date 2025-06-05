#\!/bin/bash

echo "=== DEMONSTRATING SELF-AWARE AGENTS & DYNAMIC SUPERVISORS ==="
echo ""
echo "This demonstration shows:"
echo "1. Agents that understand the system they're in"
echo "2. Dynamic supervisor creation at runtime"
echo "3. System introspection capabilities"
echo ""

# Compile
echo "Compiling..."
rebar3 compile

# Create a startup command file
cat > .demo_startup << 'STARTUP'
io:format("~n=== STARTING SELF-AWARE SYSTEM ===~n~n").
c(shell_demo).
shell_demo:run().
STARTUP

# Start the shell
echo ""
echo "Starting Erlang shell..."
echo "The demo will run automatically..."
echo ""

# Use erl directly with proper paths
erl -pa _build/default/lib/*/ebin -eval 'application:ensure_all_started(agents), io:format("~n=== STARTING SELF-AWARE SYSTEM ===~n~n"), c(shell_demo), shell_demo:run().'
