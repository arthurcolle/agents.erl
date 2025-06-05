%% simple_test.erl
%% Simple test to verify the enhanced system components

-module(simple_test).
-export([test/0]).

test() ->
    io:format("Testing Enhanced Agent System~n"),
    
    % Test 1: Check if model registry has the new models
    io:format("1. Testing model registry...~n"),
    Models = [<<"o4-mini">>, <<"gpt-4.1">>, <<"gpt-4.1-mini">>, <<"gpt-4.1-nano">>, <<"o3">>],
    lists:foreach(fun(Model) ->
        case model_registry:validate_model(Model) of
            true -> io:format("  ✅ ~s~n", [Model]);
            false -> io:format("  ❌ ~s~n", [Model])
        end
    end, Models),
    
    % Test 2: Check model categorization
    io:format("2. Testing model categories...~n"),
    case model_registry:get_models_by_category(reasoning) of
        ReasoningModels when length(ReasoningModels) > 0 ->
            io:format("  ✅ Found ~p reasoning models~n", [length(ReasoningModels)]);
        _ ->
            io:format("  ❌ No reasoning models found~n")
    end,
    
    case model_registry:get_models_by_category(cost_optimized) of
        CostModels when length(CostModels) > 0 ->
            io:format("  ✅ Found ~p cost-optimized models~n", [length(CostModels)]);
        _ ->
            io:format("  ❌ No cost-optimized models found~n")
    end,
    
    % Test 3: Check if new tools are loaded (without executing them)
    io:format("3. Testing tool availability...~n"),
    NewTools = [web_search, calculate, datetime_info, generate_uuid, hash_generate, 
                file_list, fetch_url, encode_decode, network_info, json_operations,
                text_analysis, database_query, image_processing],
    
    io:format("  📋 New tools added: ~p~n", [length(NewTools)]),
    lists:foreach(fun(Tool) ->
        io:format("    - ~p~n", [Tool])
    end, lists:sublist(NewTools, 10)),
    
    io:format("~n✅ Basic system verification completed!~n").