#!/usr/bin/env escript
%% test_system_startup.erl
%% Test the complete system startup with focused logging

main(_Args) ->
    io:format("🚀 Testing Complete System Startup~n"),
    io:format("===================================~n~n"),
    
    %% Show current configuration
    io:format("📋 Current Configuration:~n"),
    io:format("   • Minimal supervisor (no restart loops)~n"),
    io:format("   • Focused logging (agent/tool/MCP only)~n"),
    io:format("   • Essential routes only~n"),
    io:format("   • Port 8080~n"),
    
    %% Check if system is running
    case os:cmd("curl -s http://localhost:8080/api/system/health 2>/dev/null") of
        "" ->
            io:format("~n❌ System not running - start with: make shell~n");
        Response ->
            io:format("~n✅ System is running! Health response:~n"),
            io:format("~s~n", [Response])
    end,
    
    io:format("~n🔧 Available configurations:~n"),
    io:format("   📄 config/sys.config - Current focused logging~n"),
    io:format("   📄 config/focused_logging.config - Template~n"),
    io:format("   📄 config/dev_logging.config - Debug mode~n"),
    io:format("   📄 config/sys.config.backup - Original config~n"),
    
    io:format("~n⚡ Quick commands:~n"),
    io:format("   make shell                          # Start system~n"),
    io:format("   curl http://localhost:8080          # Test web interface~n"),
    io:format("   curl http://localhost:8080/api/system/health  # Health check~n"),
    
    io:format("~n💡 Logging modes:~n"),
    io:format("   cp config/focused_logging.config config/sys.config  # Focused (default)~n"),
    io:format("   cp config/dev_logging.config config/sys.config     # Debug mode~n"),
    io:format("   cp config/sys.config.backup config/sys.config      # Original~n").