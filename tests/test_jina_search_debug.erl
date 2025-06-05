-module(test_jina_search_debug).
-export([test/0]).

test() ->
    io:format("Testing jina_search tool directly...~n"),
    
    % Start required applications
    application:ensure_all_started(openai),
    application:ensure_all_started(agents),
    application:ensure_all_started(agent_web),
    
    % Test jina_search tool directly
    Args = #{
        <<"query">> => <<"weather Boston Massachusetts today">>,
        <<"num_results">> => 3
    },
    
    io:format("Calling agent_tools:execute_tool with args: ~p~n", [Args]),
    Result = agent_tools:execute_tool(<<"jina_search">>, Args),
    
    io:format("~n=== Direct Tool Result ===~n~p~n", [Result]),
    
    % Test jina_search_and_read tool
    io:format("~n=== Testing jina_search_and_read ===~n"),
    Args2 = #{
        <<"query">> => <<"weather Boston Massachusetts today">>,
        <<"num_results">> => 2
    },
    
    Result2 = agent_tools:execute_tool(<<"jina_search_and_read">>, Args2),
    io:format("jina_search_and_read result: ~p~n", [Result2]),
    
    io:format("Test completed!~n").