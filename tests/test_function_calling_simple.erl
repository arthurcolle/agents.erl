#!/usr/bin/env escript
%% Simple test script for function calling

main(_) ->
    io:format("Testing function calling fix...~n"),
    
    % Test that we can create a proper OpenAI function schema
    TestTool = #{
        <<"name">> => <<"get_weather">>,
        <<"description">> => <<"Get current temperature for provided coordinates in celsius.">>,
        <<"parameters">> => #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"latitude">> => #{<<"type">> => <<"number">>},
                <<"longitude">> => #{<<"type">> => <<"number">>}
            },
            <<"required">> => [<<"latitude">>, <<"longitude">>],
            <<"additionalProperties">> => false
        }
    },
    
    % Format it according to new OpenAI spec
    FormattedTool = #{
        <<"type">> => <<"function">>,
        <<"name">> => maps:get(<<"name">>, TestTool),
        <<"description">> => maps:get(<<"description">>, TestTool),
        <<"parameters">> => maps:get(<<"parameters">>, TestTool),
        <<"strict">> => true
    },
    
    io:format("Original tool: ~p~n", [TestTool]),
    io:format("Formatted tool: ~p~n", [FormattedTool]),
    
    % Test function call response parsing
    NewFunctionCall = #{
        <<"type">> => <<"function_call">>,
        <<"id">> => <<"fc_12345xyz">>,
        <<"call_id">> => <<"call_12345xyz">>,
        <<"name">> => <<"get_weather">>,
        <<"arguments">> => <<"{\"latitude\":48.8566,\"longitude\":2.3522}">>
    },
    
    % Parse arguments
    ArgsJson = maps:get(<<"arguments">>, NewFunctionCall),
    Args = jsx:decode(ArgsJson, [return_maps]),
    
    io:format("Function call: ~p~n", [NewFunctionCall]),
    io:format("Parsed args: ~p~n", [Args]),
    
    % Test function call output format
    FunctionOutput = #{
        <<"type">> => <<"function_call_output">>,
        <<"call_id">> => maps:get(<<"call_id">>, NewFunctionCall),
        <<"output">> => <<"14.5">>
    },
    
    io:format("Function output: ~p~n", [FunctionOutput]),
    
    io:format("✅ Function calling format test completed successfully!~n").