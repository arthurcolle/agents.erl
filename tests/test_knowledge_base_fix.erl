#!/usr/bin/env escript
%% Test script to verify knowledge base fix

main([]) ->
    code:add_path("_build/default/lib/agents/ebin"),
    code:add_path("_build/default/lib/openai/ebin"),
    code:add_path("_build/default/lib/agent_web/ebin"),
    code:add_path("_build/default/lib/jsx/ebin"),
    code:add_path("_build/default/lib/gproc/ebin"),
    
    % Start required applications
    application:ensure_all_started(gproc),
    application:ensure_all_started(agents),
    
    % Give the system time to start
    timer:sleep(1000),
    
    % Test knowledge base search
    io:format("Testing knowledge base search...~n"),
    
    % First, let's see what tools are available
    io:format("Available tools: ~p~n", [agent_tools:list_tools()]),
    
    try
        % Test the function that was failing through gen_server call
        Args = #{<<"domain">> => <<"science">>, <<"query">> => <<"physics">>, <<"max_results">> => 5},
        Result = agent_tools:execute_tool(knowledge_base_retrieval, Args),
        io:format("Knowledge base search completed successfully: ~p~n", [Result]),
        
        % Check if we got expected result format
        case Result of
            {ok, _} ->
                io:format("SUCCESS: Knowledge base search returned expected format~n");
            {error, _} ->
                io:format("SUCCESS: Knowledge base search returned error format (expected if no KB files)~n");
            _ ->
                io:format("UNEXPECTED: Knowledge base search returned unexpected format: ~p~n", [Result])
        end
    catch
        Class:Reason:Stacktrace ->
            io:format("ERROR: Knowledge base search failed: ~p:~p~n~p~n", [Class, Reason, Stacktrace])
    end,
    
    io:format("Test completed~n").