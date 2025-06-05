#!/usr/bin/env escript
%% -*- erlang -*-
%%% @doc Test colored logging with proper paths

-mode(compile).

main(_) ->
    %% Add color library to code path
    code:add_patha("_build/default/lib/color/ebin"),
    code:add_patha("apps/agent_web/ebin"),
    
    %% Test color library directly
    io:format("~n=== Testing Color Library ===~n"),
    
    try 
        %% Basic ANSI colors
        io:format("~s~n", [color:red("This is RED text")]),
        io:format("~s~n", [color:green("This is GREEN text")]),
        io:format("~s~n", [color:blue("This is BLUE text")]),
        io:format("~s~n", [color:yellow("This is YELLOW text")]),
        io:format("~s~n", [color:cyan("This is CYAN text")]),
        io:format("~s~n", [color:magenta("This is MAGENTA text")]),
        io:format("~s~n", [color:white("This is WHITE text")]),
        
        %% Background colors
        io:format("~s~n", [color:on_red(color:white("WHITE text on RED background"))]),
        io:format("~s~n", [color:on_green(color:black("BLACK text on GREEN background"))]),
        io:format("~s~n", [color:on_blue(color:white("WHITE text on BLUE background"))]),
        io:format("~s~n", [color:on_yellow(color:black("BLACK text on YELLOW background"))]),
        
        %% RGB colors (xterm 256)
        io:format("~s~n", [color:rgb([5,0,0], "Dark red (RGB 5,0,0)")]),
        io:format("~s~n", [color:rgb([0,5,0], "Bright green (RGB 0,5,0)")]),
        io:format("~s~n", [color:rgb([0,0,5], "Bright blue (RGB 0,0,5)")]),
        
        %% True 24-bit colors
        io:format("~s~n", [color:true("FF0000", "True red (#FF0000)")]),
        io:format("~s~n", [color:true("00FF00", "True green (#00FF00)")]),
        io:format("~s~n", [color:true("0000FF", "True blue (#0000FF)")]),
        io:format("~s~n", [color:true("FFA500", "Orange (#FFA500)")]),
        io:format("~s~n", [color:true("800080", "Purple (#800080)")]),
        
        io:format("~n=== Color library test SUCCESS! ===~n")
        
    catch
        Error:Reason ->
            io:format("Error: ~p:~p~n", [Error, Reason])
    end,
    
    %% Test our colored_logger module
    io:format("~n=== Testing Colored Logger Module ===~n"),
    try
        %% Test all log levels with colored_logger
        colored_logger:error("This is an ERROR message", []),
        colored_logger:warning("This is a WARNING message", []),
        colored_logger:success("This is a SUCCESS message", []),
        colored_logger:info("This is an INFO message", []),
        colored_logger:debug("This is a DEBUG message", []),
        colored_logger:critical("This is a CRITICAL message", []),
        colored_logger:health_check("Health check PASSED", []),
        colored_logger:startup("System STARTUP complete", []),
        
        %% Test API call logging
        colored_logger:api_call("GET", "/api/test", 42),
        colored_logger:api_call("POST", "/api/agents", 156),
        
        %% Test tagged logging
        colored_logger:log(info, "SYSTEM", "All systems operational", []),
        colored_logger:log(warning, "AUTH", "Invalid credentials", []),
        colored_logger:log(error, "DB", "Connection failed", []),
        
        %% Test custom color functions
        io:format("~s~n", [colored_logger:colorize(red, "Custom red")]),
        io:format("~s~n", [colored_logger:colorize(green, "Custom green")]),
        io:format("~s~n", [colored_logger:colorize(blue, "Custom blue")]),
        
        %% Test RGB and true colors
        io:format("~s~n", [colored_logger:rgb_color([3,3,0], "Custom RGB yellow")]),
        io:format("~s~n", [colored_logger:true_color("FF69B4", "Hot pink true color")]),
        
        io:format("~n=== Colored Logger test SUCCESS! ===~n")
        
    catch
        LogError:LogReason ->
            io:format("Logger Error: ~p:~p~n", [LogError, LogReason])
    end,
    
    ok.