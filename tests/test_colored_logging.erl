#!/usr/bin/env escript
%% -*- erlang -*-
%%% @doc Test script for colored logging functionality
%%% This script demonstrates all the color features available.

-mode(compile).

main(_) ->
    %% Start the necessary applications
    application:start(color),
    application:start(agents),
    application:start(agent_web),

    %% Add the lib path
    code:add_patha("_build/default/lib/color/ebin"),
    code:add_patha("apps/agent_web/ebin"),

    %% Test basic colored logging
    io:format("~n=== Testing Basic Color Support ===~n"),
    
    %% Test all log levels
    colored_logger:error("This is an error message", []),
    colored_logger:warning("This is a warning message", []),
    colored_logger:success("This is a success message", []),
    colored_logger:info("This is an info message", []),
    colored_logger:debug("This is a debug message", []),
    colored_logger:critical("This is a critical message", []),
    colored_logger:health_check("System health check passed", []),
    colored_logger:startup("Application started successfully", []),
    
    %% Test with formatting
    io:format("~n=== Testing Formatted Messages ===~n"),
    colored_logger:error("Error processing ~s with code ~p", ["request", 500]),
    colored_logger:success("Successfully processed ~p items in ~pms", [100, 1250]),
    
    %% Test API call logging
    io:format("~n=== Testing API Call Logging ===~n"),
    colored_logger:api_call("GET", "/api/agents", 45),
    colored_logger:api_call("POST", "/api/chat", 120),
    
    %% Test tagged logging
    io:format("~n=== Testing Tagged Logging ===~n"),
    colored_logger:log(info, "SYSTEM", "All systems operational", []),
    colored_logger:log(warning, "AUTH", "Failed login attempt from ~s", ["192.168.1.100"]),
    colored_logger:log(error, "DATABASE", "Connection timeout after ~pms", [5000]),
    
    %% Test custom color functions
    io:format("~n=== Testing Custom Color Functions ===~n"),
    io:format("~s~n", [colored_logger:colorize(red, "This text is red")]),
    io:format("~s~n", [colored_logger:colorize(green, "This text is green")]),
    io:format("~s~n", [colored_logger:colorize(blue, "This text is blue")]),
    io:format("~s~n", [colored_logger:colorize(yellow, "This text is yellow")]),
    io:format("~s~n", [colored_logger:colorize(cyan, "This text is cyan")]),
    io:format("~s~n", [colored_logger:colorize(magenta, "This text is magenta")]),
    
    %% Test RGB colors (xterm 256 colors)
    io:format("~n=== Testing RGB Colors (xterm 256) ===~n"),
    io:format("~s~n", [colored_logger:rgb_color([5,0,0], "Dark red text")]),
    io:format("~s~n", [colored_logger:rgb_color([0,5,0], "Bright green text")]),
    io:format("~s~n", [colored_logger:rgb_color([0,0,5], "Bright blue text")]),
    io:format("~s~n", [colored_logger:rgb_color([5,5,0], "Yellow text")]),
    io:format("~s~n", [colored_logger:rgb_color([5,0,5], "Magenta text")]),
    io:format("~s~n", [colored_logger:rgb_color([0,5,5], "Cyan text")]),
    
    %% Test true 24-bit colors (if supported by terminal)
    io:format("~n=== Testing True 24-bit Colors ===~n"),
    io:format("~s~n", [colored_logger:true_color("FF0000", "True red text")]),
    io:format("~s~n", [colored_logger:true_color("00FF00", "True green text")]),
    io:format("~s~n", [colored_logger:true_color("0000FF", "True blue text")]),
    io:format("~s~n", [colored_logger:true_color("FFA500", "Orange text")]),
    io:format("~s~n", [colored_logger:true_color("800080", "Purple text")]),
    io:format("~s~n", [colored_logger:true_color("FF69B4", "Hot pink text")]),
    
    %% Test direct color library usage
    io:format("~n=== Testing Direct Color Library Usage ===~n"),
    io:format("~s~n", [color:red("Direct red text")]),
    io:format("~s~n", [color:on_blue(color:white("White text on blue background"))]),
    io:format("~s~n", [color:on_yellow(color:black("Black text on yellow background"))]),
    
    io:format("~n=== Color Testing Complete ===~n"),
    ok.