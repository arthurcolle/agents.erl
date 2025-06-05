-module(simple_weather_test).
-export([test/0]).

test() ->
    % Test the specific function that was causing the issue
    io:format("Testing weather response formatting...~n"),
    
    % Simulate tool calls and results like the ones that were problematic
    ToolCalls = [#{
        <<"type">> => <<"function_call">>,
        <<"call_id">> => <<"call_test">>,
        <<"name">> => <<"jina_search">>,
        <<"arguments">> => <<"{\"query\":\"current weather in Washington DC\",\"site\":\"weather.com\"}">>
    }],
    
    % Simulate a successful jina search result
    ToolResults = [{
        <<"call_test">>, 
        {ok, #{
            <<"content">> => [#{
                <<"text">> => <<"Weather Forecast and Conditions for Washington, DC - The Weather Channel | Weather.com\nCurrent weather: 75°F, partly cloudy\nToday's forecast: High 78°F, Low 62°F\nConditions: Partly cloudy with light winds">>
            }]
        }}
    }],
    
    % Test the formatting function with error protection
    try
        % This should now work without hanging
        State = #{
            id => <<"test">>,
            name => <<"Test Agent">>,
            type => ai,
            model => <<"gpt-4o-mini">>,
            tools => [jina_search],
            system_prompt => <<"Test agent">>,
            conversation_history => [],
            metrics => #{},
            created_at => erlang:timestamp(),
            last_activity => erlang:timestamp(),
            api_preference => responses,
            autonomous_mode => false,
            pending_function_calls => [],
            autonomous_context => #{},
            max_autonomous_turns => 5
        },
        
        % Call the function that was problematic
        Result = agent_instance:execute_tools_and_return_summary(ToolCalls, #{}, State),
        
        case Result of
            {ok, #{message := Message}, _UpdatedState} ->
                io:format("SUCCESS! Formatted message: ~p~n", [Message]);
            {error, Reason} ->
                io:format("ERROR! Reason: ~p~n", [Reason]);
            Other ->
                io:format("UNEXPECTED! Result: ~p~n", [Other])
        end
    catch
        Error:Reason:Stack ->
            io:format("EXCEPTION! ~p:~p~n~p~n", [Error, Reason, Stack])
    end,
    
    io:format("Test completed.~n").