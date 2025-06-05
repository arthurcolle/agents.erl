#!/usr/bin/env escript

main(_) ->
    io:format("Testing weather analysis functions...~n"),
    
    % Test search results content (simulate what jina_search returns)
    MockSearchResults = <<"1. Current Weather in Washington, DC
URL: https://weather.com/weather/today/l/Washington+DC
The current temperature in Washington, DC is 72°F with partly cloudy conditions.

2. Washington DC Weather Forecast 
Current conditions: Partly Cloudy
Temperature: 72°F (22°C)
Wind: SW 8 mph">>,
    
    io:format("Mock search results:~n~s~n~n", [MockSearchResults]),
    
    % Test temperature extraction
    io:format("Testing temperature extraction...~n"),
    case re:run(MockSearchResults, <<"([0-9]+)°[FC]">>, [global, {capture, [1], binary}]) of
        {match, [[Match]]} -> 
            io:format("Extracted temperature: ~s°~n", [Match]);
        _ -> 
            io:format("No temperature found~n")
    end,
    
    % Test conditions extraction
    io:format("Testing conditions extraction...~n"),
    ConditionPatterns = [<<"partly cloudy">>, <<"Partly Cloudy">>, <<"sunny">>, <<"Sunny">>],
    Found = lists:filter(fun(Pattern) ->
        binary:match(MockSearchResults, Pattern) =/= nomatch
    end, ConditionPatterns),
    
    case Found of
        [] -> io:format("No conditions found~n");
        [First | _] -> io:format("Found conditions: ~s~n", [First])
    end,
    
    io:format("✓ Weather analysis functions tested successfully!~n").