#!/usr/bin/env escript

main(_) ->
    %% Add paths and start applications
    [code:add_path(Path) || Path <- filelib:wildcard("_build/default/lib/*/ebin")],
    
    application:load(agent_web),
    application:load(agents),
    application:load(openai),
    
    %% Start required applications
    application:ensure_all_started(ranch),
    application:ensure_all_started(cowboy),
    application:ensure_all_started(jsx),
    application:ensure_all_started(gproc),
    application:ensure_all_started(mnesia),
    
    %% Install better logging
    agent_web_app:install_better_logging(),
    
    %% Test logging
    colored_logger:startup("Test startup message", []),
    colored_logger:success("Test success message", []),
    colored_logger:info("Test info message", []),
    colored_logger:warning("Test warning message", []),
    colored_logger:error("Test error message", []),
    
    timer:sleep(1000),
    io:format("Logging test completed.~n"),
    
    init:stop().