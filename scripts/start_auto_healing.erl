#!/usr/bin/env escript
%% -*- erlang -*-

main([]) ->
    io:format("Starting agents.erl with auto-healing enabled...~n"),
    
    %% Set up paths
    code:add_pathz("_build/default/lib/agent_web/ebin"),
    code:add_pathz("_build/default/lib/agents/ebin"),
    code:add_pathz("_build/default/lib/openai/ebin"),
    
    %% Load the auto-healing module
    case code:load_file(auto_healing_startup) of
        {module, auto_healing_startup} ->
            io:format("Auto-healing module loaded successfully~n"),
            
            %% Start with healing
            case auto_healing_startup:start_with_healing() of
                ok ->
                    io:format("System started with auto-healing enabled~n"),
                    io:format("All critical processes are being monitored~n"),
                    
                    %% Keep the script running
                    timer:sleep(infinity);
                Error ->
                    io:format("Failed to start: ~p~n", [Error]),
                    halt(1)
            end;
        Error ->
            io:format("Failed to load auto-healing module: ~p~n", [Error]),
            halt(1)
    end.
