#!/bin/bash

echo "🚀 Starting Agents.erl System"
echo "============================="
echo ""

# Check if already running
if pgrep -f "beam.*agents" > /dev/null; then
    echo "⚠️  System appears to be already running"
    echo "   Use 'pkill -f \"beam.*agents\"' to stop it first"
    exit 1
fi

echo "📋 Pre-flight checks..."
echo "   Config: $([ -f config/sys.config ] && echo '✅' || echo '❌') sys.config"
echo "   Build:  $([ -d _build/default ] && echo '✅' || echo '❌') compiled code"
echo ""

echo "🔧 Starting system with proper configuration..."
echo "   Web interface will be available at: http://localhost:8080"
echo "   Press Ctrl+C to stop the system"
echo ""

# Start the system
exec ./rebar3 shell --config config/sys.config --eval "
    io:format('🔧 Initializing applications...~n'),
    
    % Start applications in order
    {ok, _} = application:ensure_all_started(openai),
    {ok, _} = application:ensure_all_started(agents), 
    {ok, _} = application:ensure_all_started(agent_web),
    
    io:format('✅ System started successfully!~n'),
    io:format('🌐 Web interface: http://localhost:8080~n'),
    io:format('⚡ Hot reload: Ready for zero-downtime updates~n'),
    io:format('💬 Streaming: Fixed to show readable text~n'),
    io:format('🏗️  Architecture: Modular OTP supervision~n'),
    io:format('~n'),
    io:format('🎯 System is now ready for use!~n'),
    io:format('   • Create agents and chat with them~n'),
    io:format('   • Test streaming with readable output~n'),
    io:format('   • Perform hot code reloads~n'),
    io:format('   • Monitor system health~n'),
    io:format('~n'),
    io:format('Press Ctrl+C to stop the system~n').
"