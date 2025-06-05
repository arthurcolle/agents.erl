#!/bin/bash
# Test deep reflection in rebar3 shell

cd /Users/agent/agents.erl

echo "Starting rebar3 shell to test deep reflection..."
echo ""
echo "Run these commands in the shell:"
echo "  c(quick_reflection_test)."
echo "  quick_reflection_test:simple_test()."
echo ""
echo "Or for full test (requires running applications):"
echo "  quick_reflection_test:test()."
echo ""

rebar3 shell