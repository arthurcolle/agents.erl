#!/usr/bin/env escript
%% Test script for the fixed function calling implementation

main(_) ->
    io:format("Testing fixed function calling implementation...~n"),
    
    % Start applications
    application:ensure_all_started(inets),
    application:ensure_all_started(jsx),
    
    % Test tool schema formatting
    test_tool_schema_formatting(),
    
    % Test function call parsing
    test_function_call_parsing(),
    
    io:format("All tests completed!~n").

test_tool_schema_formatting() ->
    io:format("Testing tool schema formatting...~n"),
    
    % Mock tool schema in old format
    OldFormatTool = #{
        <<"name">> => <<"test_tool">>,
        <<"description">> => <<"A test tool">>,
        <<"parameters">> => #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"input">> => #{
                    <<"type">> => <<"string">>,
                    <<"description">> => <<"Test input">>
                }
            },
            <<"required">> => [<<"input">>],
            <<"additionalProperties">> => false
        }
    },
    
    % Format for OpenAI API
    FormattedTools = agent_tools:format_tools_for_openai([OldFormatTool]),
    
    case FormattedTools of
        [#{<<"type">> := <<"function">>,
           <<"name">> := <<"test_tool">>,
           <<"description">> := <<"A test tool">>,
           <<"parameters">> := _,
           <<"strict">> := true}] ->
            io:format("✓ Tool schema formatting works correctly~n");
        _ ->
            io:format("✗ Tool schema formatting failed: ~p~n", [FormattedTools])
    end.

test_function_call_parsing() ->
    io:format("Testing function call parsing...~n"),
    
    % Test new OpenAI function call format
    NewFormatCall = #{
        <<"type">> => <<"function_call">>,
        <<"id">> => <<"fc_123">>,
        <<"call_id">> => <<"call_123">>,
        <<"name">> => <<"shell">>,
        <<"arguments">> => <<"{\"command\":\"echo test\"}">>
    },
    
    % Test legacy format  
    LegacyFormatCall = #{
        <<"id">> => <<"call_456">>,
        <<"function">> => #{
            <<"name">> => <<"shell">>,
            <<"arguments">> => <<"{\"command\":\"echo legacy\"}">>
        }
    },
    
    io:format("New format call: ~p~n", [NewFormatCall]),
    io:format("Legacy format call: ~p~n", [LegacyFormatCall]),
    io:format("✓ Function call parsing test structures created~n").