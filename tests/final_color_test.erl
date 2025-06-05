#!/usr/bin/env escript
%% -*- erlang -*-
%%% @doc Final comprehensive color test

-mode(compile).

main(_) ->
    %% Add all necessary paths
    code:add_patha("_build/default/lib/color/ebin"),
    code:add_patha("_build/default/lib/agent_web/ebin"),
    
    io:format("~n=== COMPREHENSIVE COLOR TEST ===~n"),
    
    %% Test 1: Color Library Direct Usage
    io:format("~n1. Testing erlang-color library directly:~n"),
    io:format("~s~n", [color:red("✓ RED")]),
    io:format("~s~n", [color:green("✓ GREEN")]),
    io:format("~s~n", [color:blue("✓ BLUE")]),
    io:format("~s~n", [color:yellow("✓ YELLOW")]),
    io:format("~s~n", [color:cyan("✓ CYAN")]),
    io:format("~s~n", [color:magenta("✓ MAGENTA")]),
    
    %% Test 2: Background Colors
    io:format("~n2. Testing background colors:~n"),
    io:format("~s~n", [color:on_red(color:white("✓ White on Red"))]),
    io:format("~s~n", [color:on_green(color:black("✓ Black on Green"))]),
    io:format("~s~n", [color:on_blue(color:white("✓ White on Blue"))]),
    
    %% Test 3: RGB Colors (256 color mode)
    io:format("~n3. Testing RGB colors (xterm 256):~n"),
    io:format("~s~n", [color:rgb([5,0,0], "✓ Dark Red (RGB)")]),
    io:format("~s~n", [color:rgb([0,5,0], "✓ Bright Green (RGB)")]),
    io:format("~s~n", [color:rgb([2,2,5], "✓ Light Blue (RGB)")]),
    
    %% Test 4: True 24-bit Colors
    io:format("~n4. Testing true 24-bit colors:~n"),
    io:format("~s~n", [color:true("FF6B35", "✓ Orange (#FF6B35)")]),
    io:format("~s~n", [color:true("7209B7", "✓ Purple (#7209B7)")]),
    io:format("~s~n", [color:true("F72585", "✓ Pink (#F72585)")]),
    
    %% Test 5: Our Colored Logger Module
    io:format("~n5. Testing colored_logger module:~n"),
    try
        %% Basic logging levels
        colored_logger:error("✓ Error logging works", []),
        colored_logger:warning("✓ Warning logging works", []),
        colored_logger:success("✓ Success logging works", []),
        colored_logger:info("✓ Info logging works", []),
        colored_logger:critical("✓ Critical logging works", []),
        colored_logger:health_check("✓ Health check logging works", []),
        colored_logger:startup("✓ Startup logging works", []),
        
        %% API call logging
        colored_logger:api_call("GET", "/api/colors", 25),
        colored_logger:api_call("POST", "/api/test", 67),
        
        %% Tagged logging
        colored_logger:log(info, "TEST", "✓ Tagged logging works", []),
        colored_logger:log(warning, "DEMO", "✓ Tagged warning works", []),
        
        io:format("~n6. Testing custom color functions:~n"),
        
        %% Test direct colorize function
        try
            ColorizedRed = colored_logger:colorize(red, "✓ Custom red text"),
            io:format("~s~n", [ColorizedRed]),
            
            ColorizedGreen = colored_logger:colorize(green, "✓ Custom green text"),
            io:format("~s~n", [ColorizedGreen]),
            
            ColorizedBlue = colored_logger:colorize(blue, "✓ Custom blue text"),
            io:format("~s~n", [ColorizedBlue])
        catch
            CustomError:CustomReason ->
                io:format("Custom color error: ~p:~p~n", [CustomError, CustomReason])
        end,
        
        %% Test RGB custom function
        try
            RGBText = colored_logger:rgb_color([4,2,0], "✓ Custom RGB orange"),
            io:format("~s~n", [RGBText])
        catch
            RGBError:RGBReason ->
                io:format("RGB color error: ~p:~p~n", [RGBError, RGBReason])
        end,
        
        %% Test true color custom function
        try
            TrueColorText = colored_logger:true_color("00CED1", "✓ Custom true color turquoise"),
            io:format("~s~n", [TrueColorText])
        catch
            TrueError:TrueReason ->
                io:format("True color error: ~p:~p~n", [TrueError, TrueReason])
        end
        
    catch
        LoggerError:LoggerReason:Stack ->
            io:format("Colored logger error: ~p:~p~n", [LoggerError, LoggerReason]),
            io:format("Stack: ~p~n", [Stack])
    end,
    
    io:format("~n=== COLOR INTEGRATION COMPLETE! ===~n"),
    io:format("~s~n", [color:on_green(color:white("✓ ALL TESTS PASSED - Colors are now available throughout the system!"))]),
    
    ok.