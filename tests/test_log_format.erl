-module(test_log_format).
-export([test/0]).

test() ->
    colored_logger:info("Test info message", []),
    colored_logger:warning("Test warning message", []),
    colored_logger:error("Test error message", []),
    colored_logger:success("Test success message", []),
    colored_logger:debug("Test debug message", []),
    colored_logger:log(info, "agents", "Test with component tag", []),
    ok.