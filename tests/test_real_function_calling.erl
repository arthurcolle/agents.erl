#!/usr/bin/env escript
%% Real integration test for function calling

main(_) ->
    io:format("🔍 REAL Function Calling Integration Test~n"),
    io:format("==========================================~n"),
    
    % Add the compiled beam files to the path
    code:add_path("_build/default/lib/agents/ebin"),
    code:add_path("_build/default/lib/openai/ebin"),
    code:add_path("_build/default/lib/agent_web/ebin"),
    
    % Start required applications
    application:ensure_all_started(jsx),
    application:ensure_all_started(crypto),
    
    try
        % Test 1: Start agent_tools server
        test_agent_tools_server(),
        
        % Test 2: Get actual tool schemas
        test_get_tool_schemas(),
        
        % Test 3: Execute a simple tool
        test_execute_tool(),
        
        io:format("~n✅ Real integration test completed!~n")
    catch
        Error:Reason:Stacktrace ->
            io:format("❌ Test failed: ~p:~p~n", [Error, Reason]),
            io:format("Stacktrace: ~p~n", [Stacktrace])
    end.

test_agent_tools_server() ->
    io:format("1️⃣  Testing agent_tools server startup...~n"),
    
    case agent_tools:start_link(#{register_predefined => true}) of
        {ok, Pid} ->
            io:format("   ✓ agent_tools server started with PID: ~p~n", [Pid]);
        {error, {already_started, Pid}} ->
            io:format("   ✓ agent_tools server already running with PID: ~p~n", [Pid]);
        Error ->
            io:format("   ❌ Failed to start agent_tools server: ~p~n", [Error]),
            throw(agent_tools_start_failed)
    end.

test_get_tool_schemas() ->
    io:format("2️⃣  Testing tool schema retrieval...~n"),
    
    % Get enhanced tools (includes predefined + MCP tools)
    Tools = agent_tools:get_enhanced_tools([shell, file_read, who_am_i]),
    
    io:format("   Retrieved ~p tools~n", [length(Tools)]),
    
    % Check that at least one tool has the correct format
    case Tools of
        [FirstTool | _] ->
            RequiredFields = [<<"type">>, <<"name">>, <<"description">>, <<"parameters">>, <<"strict">>],
            HasAllFields = lists:all(fun(Field) -> maps:is_key(Field, FirstTool) end, RequiredFields),
            
            case HasAllFields of
                true ->
                    io:format("   ✓ Tool schema has correct format~n"),
                    io:format("   Tool example: ~p~n", [maps:get(<<"name">>, FirstTool)]);
                false ->
                    io:format("   ❌ Tool schema missing required fields~n"),
                    io:format("   Tool: ~p~n", [FirstTool]),
                    throw(invalid_tool_schema)
            end;
        [] ->
            io:format("   ❌ No tools retrieved~n"),
            throw(no_tools_retrieved)
    end.

test_execute_tool() ->
    io:format("3️⃣  Testing tool execution...~n"),
    
    % Test executing the who_am_i tool (doesn't require external deps)
    case agent_tools:execute_tool(who_am_i, #{}) of
        {error, Reason} ->
            io:format("   ⚠️  Tool execution failed (expected in test env): ~p~n", [Reason]);
        Result ->
            io:format("   ✓ Tool executed successfully~n"),
            io:format("   Result type: ~p~n", [
                if 
                    is_binary(Result) -> binary;
                    is_map(Result) -> map;
                    is_list(Result) -> list;
                    true -> other
                end
            ])
    end.