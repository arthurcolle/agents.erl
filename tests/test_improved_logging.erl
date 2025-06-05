-module(test_improved_logging).
-export([test/0]).

test() ->
    io:format("~n=== Improved Colored Logger Demo ===~n~n"),
    
    % Show different log levels with clean formatting
    colored_logger:info("Application starting up...", []),
    colored_logger:success("Database connection established", []),
    colored_logger:warning("Cache size approaching limit: ~p MB", [487]),
    colored_logger:error("Failed to connect to external API", []),
    colored_logger:debug("Processing request ID: ~s", ["abc123"]),
    
    io:format("~n--- Tagged Logs ---~n"),
    colored_logger:log(info, "HTTP", "GET /api/users - 200 OK (125ms)", []),
    colored_logger:log(warning, "AUTH", "Token expiring in 5 minutes", []),
    colored_logger:log(error, "DB", "Query timeout after ~p seconds", [30]),
    
    io:format("~n--- Special Effects ---~n"),
    colored_logger:startup("🚀 Agent system initialized", []),
    colored_logger:critical("⚠️  Critical system alert!", []),
    colored_logger:api_call("GET", "/health", 42),
    
    io:format("~n--- Timestamp Formats ---~n"),
    colored_logger:info("Short timestamp format (default)", []),
    colored_logger:set_timestamp_format(full),
    colored_logger:info("Full timestamp format", []),
    colored_logger:set_timestamp_format(short),
    
    io:format("~n--- Theme Examples ---~n"),
    io:format("~s", [colored_logger:neural(active, "Neural network processing...")]),
    io:format("~n"),
    io:format("~s", [colored_logger:quantum(entangled, "Quantum state detected")]),
    io:format("~n"),
    io:format("~s", [colored_logger:matrix(green, "Matrix operations running")]),
    io:format("~n"),
    
    io:format("~n✅ Logging demonstration complete!~n~n"),
    ok.