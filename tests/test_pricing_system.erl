#!/usr/bin/env escript
%%% Test script for the comprehensive pricing system

main(_) ->
    io:format("=== Testing Comprehensive OpenAI Pricing System ===~n~n"),
    
    %% Test 1: Check if all modules compile
    io:format("1. Testing module compilation...~n"),
    test_compilation(),
    
    %% Test 2: Start the pricing system
    io:format("~n2. Starting pricing system...~n"),
    test_startup(),
    
    %% Test 3: Test model registry
    io:format("~n3. Testing model registry...~n"),
    test_model_registry(),
    
    %% Test 4: Test real-time pricing
    io:format("~n4. Testing real-time pricing...~n"),
    test_realtime_pricing(),
    
    %% Test 5: Test cost calculations
    io:format("~n5. Testing cost calculations...~n"),
    test_cost_calculations(),
    
    %% Test 6: Test pricing trends
    io:format("~n6. Testing pricing trends...~n"),
    test_pricing_trends(),
    
    io:format("~n=== All tests completed! ===~n").

test_compilation() ->
    Modules = [model_registry, realtime_pricing, cost_tracker, pricing_api_handler],
    lists:foreach(fun(Module) ->
        case compile:file(atom_to_list(Module), [binary, return_errors]) of
            {ok, Module, _Binary} ->
                io:format("  ✓ ~p compiled successfully~n", [Module]);
            {error, Errors, _Warnings} ->
                io:format("  ✗ ~p compilation failed: ~p~n", [Module, Errors])
        end
    end, Modules).

test_startup() ->
    try
        %% Start realtime pricing
        case realtime_pricing:start_link() of
            {ok, Pid} ->
                io:format("  ✓ Real-time pricing started (PID: ~p)~n", [Pid]);
            {error, {already_started, Pid}} ->
                io:format("  ✓ Real-time pricing already running (PID: ~p)~n", [Pid]);
            Error ->
                io:format("  ✗ Failed to start real-time pricing: ~p~n", [Error])
        end,
        
        %% Start cost tracker
        case cost_tracker:start_link() of
            {ok, Pid2} ->
                io:format("  ✓ Cost tracker started (PID: ~p)~n", [Pid2]);
            {error, {already_started, Pid2}} ->
                io:format("  ✓ Cost tracker already running (PID: ~p)~n", [Pid2]);
            Error2 ->
                io:format("  ✗ Failed to start cost tracker: ~p~n", [Error2])
        end
    catch
        _:StartupError ->
            io:format("  ✗ Startup error: ~p~n", [StartupError])
    end.

test_model_registry() ->
    try
        %% Test getting all models
        AllModels = model_registry:get_all_models(),
        io:format("  ✓ Retrieved ~p models from registry~n", [length(AllModels)]),
        
        %% Test specific model lookup
        case model_registry:get_model_info(<<"gpt-4.1">>) of
            {ok, ModelInfo} ->
                io:format("  ✓ Found GPT-4.1 model: ~p~n", [maps:get(name, ModelInfo)]);
            {error, Reason} ->
                io:format("  ✗ Failed to find GPT-4.1: ~p~n", [Reason])
        end,
        
        %% Test new models
        NewModels = [<<"gpt-4.5-preview">>, <<"o3">>, <<"o4-mini">>, <<"gpt-4.1-nano">>],
        lists:foreach(fun(Model) ->
            case model_registry:get_model_info(Model) of
                {ok, _} ->
                    io:format("  ✓ New model ~s found~n", [Model]);
                {error, _} ->
                    io:format("  ✗ New model ~s not found~n", [Model])
            end
        end, NewModels),
        
        %% Test model categories
        Categories = model_registry:get_model_categories(),
        io:format("  ✓ Available categories: ~p~n", [Categories])
    catch
        _:Error ->
            io:format("  ✗ Model registry error: ~p~n", [Error])
    end.

test_realtime_pricing() ->
    try
        %% Test getting all pricing
        case realtime_pricing:get_all_pricing() of
            {ok, AllPricing} ->
                io:format("  ✓ Retrieved pricing for ~p models~n", [maps:size(AllPricing)]);
            {error, Reason} ->
                io:format("  ✗ Failed to get all pricing: ~p~n", [Reason])
        end,
        
        %% Test specific model pricing
        TestModels = [<<"gpt-4.1">>, <<"gpt-4o">>, <<"o3">>, <<"gpt-4.1-mini">>],
        lists:foreach(fun(Model) ->
            case realtime_pricing:get_model_pricing(Model) of
                {ok, Pricing} ->
                    InputPrice = maps:get(input, Pricing, 0.0),
                    OutputPrice = maps:get(output, Pricing, 0.0),
                    io:format("  ✓ ~s: $~.2f input, $~.2f output per 1M tokens~n", 
                             [Model, InputPrice, OutputPrice]);
                {error, Reason} ->
                    io:format("  ✗ Failed to get pricing for ~s: ~p~n", [Model, Reason])
            end
        end, TestModels)
    catch
        _:PricingError ->
            io:format("  ✗ Real-time pricing error: ~p~n", [PricingError])
    end.

test_cost_calculations() ->
    try
        %% Test cost calculation for different models
        TestCases = [
            {<<"gpt-4.1">>, #{prompt_tokens => 1000, completion_tokens => 500}, #{}},
            {<<"gpt-4o">>, #{prompt_tokens => 2000, completion_tokens => 1000, cached_tokens => 500}, #{}},
            {<<"o3">>, #{prompt_tokens => 500, completion_tokens => 200}, #{batch => true}},
            {<<"gpt-4.1-mini">>, #{prompt_tokens => 5000, completion_tokens => 2000}, #{}}
        ],
        
        lists:foreach(fun({Model, Usage, Options}) ->
            case realtime_pricing:calculate_cost(Model, Usage, Options) of
                {ok, CostData} ->
                    TotalCost = maps:get(total_cost, CostData),
                    InputTokens = maps:get(input_tokens, CostData),
                    OutputTokens = maps:get(output_tokens, CostData),
                    io:format("  ✓ ~s: ~p input + ~p output tokens = $~.6f~n", 
                             [Model, InputTokens, OutputTokens, TotalCost]);
                {error, Reason} ->
                    io:format("  ✗ Cost calculation failed for ~s: ~p~n", [Model, Reason])
            end
        end, TestCases)
    catch
        _:Error ->
            io:format("  ✗ Cost calculation error: ~p~n", [Error])
    end.

test_pricing_trends() ->
    try
        %% Test price trends (will show no data for new installation)
        TestModels = [<<"gpt-4.1">>, <<"gpt-4o">>, <<"o3">>],
        lists:foreach(fun(Model) ->
            case realtime_pricing:get_price_trends(Model) of
                {ok, Trends} ->
                    Trend = maps:get(trend, Trends, no_data),
                    io:format("  ✓ ~s price trend: ~p~n", [Model, Trend]);
                {error, Reason} ->
                    io:format("  ✗ Failed to get trends for ~s: ~p~n", [Model, Reason])
            end
        end, TestModels),
        
        %% Test price history
        case realtime_pricing:get_price_history(<<"gpt-4.1">>) of
            {ok, History} ->
                io:format("  ✓ Price history entries: ~p~n", [length(History)]);
            {error, Reason} ->
                io:format("  ✗ Failed to get price history: ~p~n", [Reason])
        end
    catch
        _:Error ->
            io:format("  ✗ Pricing trends error: ~p~n", [Error])
    end.