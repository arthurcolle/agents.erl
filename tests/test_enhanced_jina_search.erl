-module(test_enhanced_jina_search).
-export([test/0, test_weather/0, test_search_and_read/0]).

%% Test the enhanced Jina search functionality
%% Tests both the enhanced jina_search with fetch_content=true
%% and the new jina_search_and_read tool

test() ->
    io:format("Starting enhanced Jina search tests...~n"),
    
    % Test the enhanced search tool
    test_enhanced_search(),
    
    % Test the search and read tool 
    test_search_and_read(),
    
    % Test weather specifically
    test_weather(),
    
    io:format("Enhanced Jina search tests completed!~n").

test_enhanced_search() ->
    io:format("~n=== Testing Enhanced Jina Search (fetch_content=true) ===~n"),
    
    % Test enhanced search with content fetching
    Args = #{
        <<"query">> => <<"current weather in San Francisco">>,
        <<"num_results">> => 2,
        <<"site">> => <<"weather.com">>,
        <<"fetch_content">> => true
    },
    
    case jina_tools:jina_search(Args) of
        {ok, #{<<"content">> := [#{<<"text">> := Text}]}} ->
            io:format("Enhanced search successful!~n"),
            io:format("Result length: ~p characters~n", [byte_size(Text)]),
            io:format("Preview: ~s~n", [binary:part(Text, 0, min(200, byte_size(Text)))]);
        {error, Error} ->
            io:format("Enhanced search failed: ~p~n", [Error])
    end.

test_search_and_read() ->
    io:format("~n=== Testing Jina Search and Read Tool ===~n"),
    
    % Test search and read tool
    Args = #{
        <<"query">> => <<"current weather in Washington DC">>,
        <<"num_results">> => 2,
        <<"site">> => <<"weather.com">>
    },
    
    case jina_tools:jina_search_and_read(Args) of
        {ok, #{<<"content">> := [#{<<"text">> := Text}]}} ->
            io:format("Search and read successful!~n"),
            io:format("Result length: ~p characters~n", [byte_size(Text)]),
            io:format("Preview: ~s~n", [binary:part(Text, 0, min(300, byte_size(Text)))]);
        {error, Error} ->
            io:format("Search and read failed: ~p~n", [Error])
    end.

test_weather() ->
    io:format("~n=== Testing Weather Retrieval ===~n"),
    
    % Test with multiple weather sources
    WeatherQueries = [
        {<<"current weather in New York City">>, <<"weather.com">>},
        {<<"current weather in London UK">>, <<"bbc.co.uk">>},
        {<<"current weather in Tokyo Japan">>, undefined}
    ],
    
    lists:foreach(fun({Query, Site}) ->
        io:format("~nTesting query: ~s~n", [Query]),
        
        Args = case Site of
            undefined -> #{
                <<"query">> => Query,
                <<"num_results">> => 1
            };
            _ -> #{
                <<"query">> => Query,
                <<"num_results">> => 1,
                <<"site">> => Site
            }
        end,
        
        case jina_tools:jina_search_and_read(Args) of
            {ok, #{<<"content">> := [#{<<"text">> := Text}]}} ->
                io:format("SUCCESS: Retrieved ~p characters~n", [byte_size(Text)]),
                % Check if we got actual weather data
                LowerText = string:lowercase(binary_to_list(Text)),
                HasWeatherTerms = lists:any(fun(Term) ->
                    string:str(LowerText, Term) > 0
                end, ["temperature", "temp", "degrees", "°", "weather", "forecast", "conditions"]),
                
                if 
                    HasWeatherTerms ->
                        io:format("✓ Contains weather information~n");
                    true ->
                        io:format("⚠ May not contain weather information~n")
                end;
            {error, Error} ->
                io:format("FAILED: ~p~n", [Error])
        end
    end, WeatherQueries).