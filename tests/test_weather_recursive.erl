%% Test the recursive weather search functionality
-module(test_weather_recursive).
-export([test/0]).

test() ->
    % Compile and start the necessary applications
    application:start(ssl),
    application:start(inets),
    application:start(jsx),
    
    % Start our applications
    application:start(openai),
    application:start(agents),
    
    % Wait for the agent_tools server to start
    timer:sleep(1000),
    
    % Test the recursive search
    Args = #{
        <<"query">> => <<"current weather in Washington DC">>,
        <<"answer_type">> => <<"weather">>,
        <<"location">> => <<"Washington DC">>,
        <<"max_attempts">> => 3,
        <<"preferred_sites">> => [<<"weather.com">>]
    },
    
    io:format("Testing recursive weather search...~n"),
    
    case agent_tools:execute_tool(recursive_search_until_answer, Args) of
        {ok, Result} ->
            io:format("Success! Result: ~p~n", [Result]);
        {error, Error} ->
            io:format("Error: ~p~n", [Error]);
        Other ->
            io:format("Unexpected result: ~p~n", [Other])
    end,
    
    % Stop applications
    application:stop(agents),
    application:stop(openai),
    halt().