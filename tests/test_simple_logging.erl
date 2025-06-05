-module(test_simple_logging).
-export([test/0]).

test() ->
    %% Test the simplified logging
    io:format("~n=== Testing Simplified Logging ===~n~n"),
    
    %% Install the new handler
    colored_logger_handler:install(),
    
    %% Test different log levels
    logger:info("This is an info message"),
    logger:warning("This is a warning message"),  
    logger:error("This is an error message"),
    logger:debug("This is a debug message"),
    
    %% Test with metadata
    logger:info("Processing request", #{module => test_module, line => 42}),
    
    %% Test formatted messages
    logger:info("User ~s logged in from IP ~s", ["john_doe", "192.168.1.1"]),
    logger:warning("Memory usage at ~p%", [85]),
    logger:error("Failed to connect to ~s:~p", ["localhost", 5432]),
    
    io:format("~n=== Test Complete ===~n"),
    ok.