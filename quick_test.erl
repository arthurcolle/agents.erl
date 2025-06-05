#!/usr/bin/env escript

main(_) ->
    %% Quick test to see if everything compiles and can start
    io:format("Testing agent system startup...~n"),
    
    try
        %% Test compilation by trying to load modules
        case code:ensure_loaded(agent_initializer) of
            {module, agent_initializer} ->
                io:format("✅ agent_initializer module loaded~n");
            {error, Reason1} ->
                io:format("❌ Failed to load agent_initializer: ~p~n", [Reason1])
        end,
        
        case code:ensure_loaded(agent_templates) of
            {module, agent_templates} ->
                io:format("✅ agent_templates module loaded~n");
            {error, Reason2} ->
                io:format("❌ Failed to load agent_templates: ~p~n", [Reason2])
        end,
        
        case code:ensure_loaded(agent_api_handler) of
            {module, agent_api_handler} ->
                io:format("✅ agent_api_handler module loaded~n");
            {error, Reason3} ->
                io:format("❌ Failed to load agent_api_handler: ~p~n", [Reason3])
        end,
        
        %% Test template listing
        io:format("~nTesting agent templates...~n"),
        Templates = agent_templates:list_templates(),
        io:format("Found ~p templates~n", [length(Templates)]),
        
        %% Show first few templates
        FirstFew = lists:sublist(Templates, 5),
        lists:foreach(fun(#{id := Id, name := Name}) ->
            io:format("  • ~s: ~s~n", [Id, Name])
        end, FirstFew),
        
        io:format("~n✅ System appears ready to start!~n"),
        io:format("Run './scripts/start_web.sh' to start the full system.~n")
        
    catch
        Error:ErrorReason ->
            io:format("❌ Error during test: ~p:~p~n", [Error, ErrorReason])
    end.