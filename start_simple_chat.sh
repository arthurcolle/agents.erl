#!/bin/bash

# start_simple_chat.sh
# Simple deployment script for basic agent chat functionality
# Focuses on core chat features with jiffy serialization

set -e

echo "🚀 Starting Simple Agent Chat System"
echo "===================================="

cd "$(dirname "$0")"

# Quick compilation check
echo "📦 Compiling..."
if rebar3 compile --quiet; then
    echo "✅ Compilation successful"
else
    echo "❌ Compilation failed"
    exit 1
fi

# Start with basic configuration
echo "🔧 Starting system..."
echo "💡 Web interface will be available at: http://localhost:8080"
echo "💡 Press Ctrl+C twice to exit"
echo ""

exec rebar3 shell --config config/sys.config --eval "
    io:format('🌟 Initializing Chat System...~n'),
    
    % Start core dependencies
    application:ensure_all_started(jiffy),
    application:ensure_all_started(jsx),
    application:ensure_all_started(gproc),
    
    % Start our applications
    application:ensure_all_started(openai),
    application:ensure_all_started(agents),
    application:ensure_all_started(agent_web),
    
    io:format('~n🎉 Chat System Ready!~n'),
    io:format('🌐 Web Interface: http://localhost:8080~n'),
    io:format('📡 WebSocket: ws://localhost:8080/ws~n'),
    io:format('~n'),
    
    % Test jiffy
    TestData = #{<<"message">> => <<"Hello from jiffy!">>, <<"timestamp">> => erlang:system_time(millisecond)},
    case jiffy:encode(TestData) of
        JsonBinary when is_binary(JsonBinary) ->
            io:format('✅ Jiffy JSON serialization working~n');
        _ ->
            io:format('❌ Jiffy serialization failed~n')
    end,
    
    io:format('~n💬 Ready for chat! Use the web interface or API.~n~n')
"