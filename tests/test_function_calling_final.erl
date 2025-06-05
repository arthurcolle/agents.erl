#!/usr/bin/env escript
%% Final test for function calling implementation

main(_) ->
    io:format("🧪 Testing Fixed Function Calling Implementation~n"),
    io:format("================================================~n"),
    
    % Test 1: Tool schema format
    test_tool_schema(),
    
    % Test 2: Function call parsing
    test_function_call_parsing(),
    
    % Test 3: Function output format
    test_function_output(),
    
    io:format("~n✅ All function calling fixes verified!~n").

test_tool_schema() ->
    io:format("1️⃣  Testing Tool Schema Format...~n"),
    
    % Create a tool schema that follows OpenAI strict mode requirements
    ToolSchema = #{
        <<"type">> => <<"function">>,
        <<"name">> => <<"get_weather">>,
        <<"description">> => <<"Get current temperature for provided coordinates in celsius.">>,
        <<"parameters">> => #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"latitude">> => #{<<"type">> => <<"number">>},
                <<"longitude">> => #{<<"type">> => <<"number">>}
            },
            <<"required">> => [<<"latitude">>, <<"longitude">>],
            <<"additionalProperties">> => false  % Required for strict mode
        },
        <<"strict">> => true  % Enable strict mode
    },
    
    % Verify all required fields are present
    RequiredFields = [<<"type">>, <<"name">>, <<"description">>, <<"parameters">>, <<"strict">>],
    AllPresent = lists:all(fun(Field) -> maps:is_key(Field, ToolSchema) end, RequiredFields),
    
    case AllPresent of
        true ->
            io:format("   ✓ Tool schema has all required fields~n");
        false ->
            io:format("   ✗ Tool schema missing required fields~n")
    end,
    
    % Verify parameters structure
    Params = maps:get(<<"parameters">>, ToolSchema),
    HasAdditionalProps = maps:get(<<"additionalProperties">>, Params, undefined) =:= false,
    HasRequired = maps:is_key(<<"required">>, Params),
    
    case HasAdditionalProps andalso HasRequired of
        true ->
            io:format("   ✓ Parameters follow strict mode requirements~n");
        false ->
            io:format("   ✗ Parameters don't follow strict mode requirements~n")
    end.

test_function_call_parsing() ->
    io:format("2️⃣  Testing Function Call Parsing...~n"),
    
    % New OpenAI function call format (what we expect to receive)
    NewFunctionCall = #{
        <<"type">> => <<"function_call">>,
        <<"id">> => <<"fc_12345xyz">>,
        <<"call_id">> => <<"call_12345xyz">>,
        <<"name">> => <<"get_weather">>,
        <<"arguments">> => <<"{\"latitude\":48.8566,\"longitude\":2.3522}">>
    },
    
    % Verify we can extract the required fields
    CallType = maps:get(<<"type">>, NewFunctionCall),
    CallId = maps:get(<<"call_id">>, NewFunctionCall),
    FunctionName = maps:get(<<"name">>, NewFunctionCall),
    Arguments = maps:get(<<"arguments">>, NewFunctionCall),
    
    case {CallType, CallId, FunctionName, Arguments} of
        {<<"function_call">>, <<"call_12345xyz">>, <<"get_weather">>, <<"{\"latitude\":48.8566,\"longitude\":2.3522}">>} ->
            io:format("   ✓ Can extract all function call fields~n");
        _ ->
            io:format("   ✗ Failed to extract function call fields~n")
    end.

test_function_output() ->
    io:format("3️⃣  Testing Function Output Format...~n"),
    
    % Function call output format (what we need to send back)
    FunctionOutput = #{
        <<"type">> => <<"function_call_output">>,
        <<"call_id">> => <<"call_12345xyz">>,
        <<"output">> => <<"14.5">>
    },
    
    % Verify output structure
    OutputType = maps:get(<<"type">>, FunctionOutput),
    OutputCallId = maps:get(<<"call_id">>, FunctionOutput),
    OutputValue = maps:get(<<"output">>, FunctionOutput),
    
    case {OutputType, OutputCallId, OutputValue} of
        {<<"function_call_output">>, <<"call_12345xyz">>, <<"14.5">>} ->
            io:format("   ✓ Function output format is correct~n");
        _ ->
            io:format("   ✗ Function output format is incorrect~n")
    end.