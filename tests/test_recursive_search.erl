#!/usr/bin/env escript
%% Quick test of the recursive search functionality

main(_) ->
    % Add build path
    code:add_path("_build/default/lib/agents/ebin"),
    code:add_path("_build/default/lib/openai/ebin"),
    code:add_path("_build/default/lib/agent_web/ebin"),
    
    % Compile and load the agent_tools module
    case c:c("apps/agents/src/agent_tools.erl", [{outdir, "_build/default/lib/agents/ebin"}]) of
        {ok, agent_tools} ->
            io:format("Successfully compiled and loaded agent_tools~n"),
            
            % Test recursive search
            Args = #{
                <<"query">> => <<"current weather in Washington DC">>,
                <<"answer_type">> => <<"weather">>,
                <<"location">> => <<"Washington DC">>,
                <<"max_attempts">> => 3,
                <<"preferred_sites">> => [<<"weather.com">>]
            },
            
            io:format("Testing recursive search with args: ~p~n", [Args]),
            
            % This would test the function if the full system was running
            io:format("Recursive search functionality has been implemented and compiled successfully.~n"),
            io:format("To test fully, start the agent system and use the recursive_search_until_answer tool.~n");
        {error, Error} ->
            io:format("Failed to compile agent_tools: ~p~n", [Error])
    end.