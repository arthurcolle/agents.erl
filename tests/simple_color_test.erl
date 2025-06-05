#!/usr/bin/env escript
%% -*- erlang -*-
%%% @doc Simple test for colored logging

-mode(compile).

main(_) ->
    %% Test basic color functionality
    io:format("~n=== Testing erlang-color library directly ===~n"),
    
    %% Test if color module is available
    try
        RedText = color:red("This is red text"),
        io:format("~s~n", [RedText]),
        
        GreenText = color:green("This is green text"),
        io:format("~s~n", [GreenText]),
        
        BlueText = color:blue("This is blue text"),
        io:format("~s~n", [BlueText]),
        
        YellowText = color:yellow("This is yellow text"),
        io:format("~s~n", [YellowText]),
        
        CyanText = color:cyan("This is cyan text"),
        io:format("~s~n", [CyanText]),
        
        MagentaText = color:magenta("This is magenta text"),
        io:format("~s~n", [MagentaText]),
        
        %% Test background colors
        WhiteOnRed = color:on_red(color:white("White text on red background")),
        io:format("~s~n", [WhiteOnRed]),
        
        BlackOnYellow = color:on_yellow(color:black("Black text on yellow background")),
        io:format("~s~n", [BlackOnYellow]),
        
        %% Test RGB colors
        RGBGreen = color:rgb([0,5,0], "RGB green text"),
        io:format("~s~n", [RGBGreen]),
        
        %% Test true colors (24-bit)
        TrueRed = color:true("FF0000", "True color red"),
        io:format("~s~n", [TrueRed]),
        
        io:format("~n=== Color test completed successfully! ===~n")
        
    catch
        Error:Reason ->
            io:format("Error testing colors: ~p:~p~n", [Error, Reason])
    end,
    
    ok.