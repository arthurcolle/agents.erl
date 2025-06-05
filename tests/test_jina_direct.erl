-module(test_jina_direct).
-export([test/0]).

test() ->
    % Test jina_search directly
    Args = #{
        <<"query">> => <<"current weather in Washington DC">>,
        <<"site">> => <<"weather.com">>
    },
    
    io:format("Testing jina_search with args: ~p~n", [Args]),
    
    Result = try
        jina_tools:jina_search(Args)
    catch
        Error:Reason:Stack ->
            io:format("Error calling jina_search: ~p:~p~n", [Error, Reason]),
            io:format("Stack trace:~n"),
            lists:foreach(fun(Frame) ->
                io:format("  ~p~n", [Frame])
            end, Stack),
            {error, {Error, Reason}}
    end,
    
    io:format("Result: ~p~n", [Result]),
    Result.