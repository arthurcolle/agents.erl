#!/usr/bin/env escript
%%! -pa _build/default/lib/*/ebin

-module(test_agent_tools_fix).

main(_Args) ->
    io:format("Testing agent_tools:get_enhanced_tools/1 function...~n"),
    
    % Start the necessary applications
    application:ensure_all_started(ssl),
    application:ensure_all_started(inets),
    application:ensure_all_started(crypto),
    
    % Start agent_tools with options
    Options = #{
        register_predefined => true,
        default_tools => #{}
    },
    
    case agent_tools:start_link(Options) of
        {ok, _Pid} ->
            io:format("✓ agent_tools started successfully~n"),
            
            % Test get_enhanced_tools/1
            ToolNames = [system_metrics, alerting, logging],
            io:format("Testing get_enhanced_tools with: ~p~n", [ToolNames]),
            
            case agent_tools:get_enhanced_tools(ToolNames) of
                Tools when is_list(Tools) ->
                    io:format("✓ get_enhanced_tools/1 returned successfully~n"),
                    io:format("  Found ~p tools~n", [length(Tools)]),
                    ok;
                Error ->
                    io:format("✗ get_enhanced_tools/1 failed: ~p~n", [Error]),
                    halt(1)
            end;
        Error ->
            io:format("✗ Failed to start agent_tools: ~p~n", [Error]),
            halt(1)
    end,
    
    io:format("~nAll tests passed! The fix is working correctly.~n"),
    halt(0).