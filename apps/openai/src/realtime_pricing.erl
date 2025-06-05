%%%-------------------------------------------------------------------
%%% @doc Real-time Pricing Manager
%%% Comprehensive pricing tracking and updates for all OpenAI models
%%% @end
%%%-------------------------------------------------------------------
-module(realtime_pricing).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    get_model_pricing/1,
    get_all_pricing/0,
    update_pricing/1,
    calculate_cost/3,
    get_price_history/1,
    get_price_trends/1,
    export_pricing_data/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-record(state, {
    pricing_data = #{},
    price_history = [],
    last_update = undefined,
    update_interval = 3600000  % 1 hour in milliseconds
}).

-record(price_entry, {
    timestamp,
    model,
    pricing_data,
    source = manual
}).

%% Complete OpenAI pricing data (as of pricing page)
-define(CURRENT_PRICING, #{
    %% Text tokens - Price per 1M tokens
    
    %% GPT-4.1 Series
    <<"gpt-4.1">> => #{
        input => 2.00,
        cached_input => 0.50,
        output => 8.00,
        batch_input => 1.00,
        batch_cached_input => 0.25,
        batch_output => 4.00
    },
    <<"gpt-4.1-2025-04-14">> => #{
        input => 2.00,
        cached_input => 0.50,
        output => 8.00,
        batch_input => 1.00,
        batch_cached_input => 0.25,
        batch_output => 4.00
    },
    <<"gpt-4.1-mini">> => #{
        input => 0.40,
        cached_input => 0.10,
        output => 1.60,
        batch_input => 0.20,
        batch_cached_input => 0.05,
        batch_output => 0.80
    },
    <<"gpt-4.1-mini-2025-04-14">> => #{
        input => 0.40,
        cached_input => 0.10,
        output => 1.60,
        batch_input => 0.20,
        batch_cached_input => 0.05,
        batch_output => 0.80
    },
    <<"gpt-4.1-nano">> => #{
        input => 0.10,
        cached_input => 0.025,
        output => 0.40,
        batch_input => 0.05,
        batch_cached_input => 0.0125,
        batch_output => 0.20
    },
    <<"gpt-4.1-nano-2025-04-14">> => #{
        input => 0.10,
        cached_input => 0.025,
        output => 0.40,
        batch_input => 0.05,
        batch_cached_input => 0.0125,
        batch_output => 0.20
    },
    
    %% GPT-4.5 Series
    <<"gpt-4.5-preview">> => #{
        input => 75.00,
        cached_input => 37.50,
        output => 150.00,
        batch_input => 37.50,
        batch_cached_input => 18.75,
        batch_output => 75.00
    },
    <<"gpt-4.5-preview-2025-02-27">> => #{
        input => 75.00,
        cached_input => 37.50,
        output => 150.00,
        batch_input => 37.50,
        batch_cached_input => 18.75,
        batch_output => 75.00
    },
    
    %% GPT-4o Series
    <<"gpt-4o">> => #{
        input => 2.50,
        cached_input => 1.25,
        output => 10.00,
        batch_input => 1.25,
        batch_cached_input => 0.625,
        batch_output => 5.00
    },
    <<"gpt-4o-2024-08-06">> => #{
        input => 2.50,
        cached_input => 1.25,
        output => 10.00,
        batch_input => 1.25,
        batch_cached_input => 0.625,
        batch_output => 5.00
    },
    <<"gpt-4o-audio-preview">> => #{
        input => 2.50,
        output => 10.00,
        audio_input => 40.00,
        audio_output => 80.00
    },
    <<"gpt-4o-audio-preview-2024-12-17">> => #{
        input => 2.50,
        output => 10.00,
        audio_input => 40.00,
        audio_output => 80.00
    },
    <<"gpt-4o-realtime-preview">> => #{
        input => 5.00,
        cached_input => 2.50,
        output => 20.00,
        audio_input => 40.00,
        audio_output => 80.00
    },
    <<"gpt-4o-realtime-preview-2024-12-17">> => #{
        input => 5.00,
        cached_input => 2.50,
        output => 20.00,
        audio_input => 40.00,
        audio_output => 80.00
    },
    <<"gpt-4o-mini">> => #{
        input => 0.15,
        cached_input => 0.075,
        output => 0.60,
        batch_input => 0.075,
        batch_cached_input => 0.0375,
        batch_output => 0.30
    },
    <<"gpt-4o-mini-2024-07-18">> => #{
        input => 0.15,
        cached_input => 0.075,
        output => 0.60,
        batch_input => 0.075,
        batch_cached_input => 0.0375,
        batch_output => 0.30
    },
    <<"gpt-4o-mini-audio-preview">> => #{
        input => 0.15,
        output => 0.60,
        audio_input => 10.00,
        audio_output => 20.00
    },
    <<"gpt-4o-mini-audio-preview-2024-12-17">> => #{
        input => 0.15,
        output => 0.60,
        audio_input => 10.00,
        audio_output => 20.00
    },
    <<"gpt-4o-mini-realtime-preview">> => #{
        input => 0.60,
        cached_input => 0.30,
        output => 2.40,
        audio_input => 10.00,
        audio_output => 20.00
    },
    <<"gpt-4o-mini-realtime-preview-2024-12-17">> => #{
        input => 0.60,
        cached_input => 0.30,
        output => 2.40,
        audio_input => 10.00,
        audio_output => 20.00
    },
    
    %% o-series Reasoning Models
    <<"o1">> => #{
        input => 15.00,
        cached_input => 7.50,
        output => 60.00,
        batch_input => 7.50,
        batch_cached_input => 3.75,
        batch_output => 30.00
    },
    <<"o1-2024-12-17">> => #{
        input => 15.00,
        cached_input => 7.50,
        output => 60.00,
        batch_input => 7.50,
        batch_cached_input => 3.75,
        batch_output => 30.00
    },
    <<"o1-pro">> => #{
        input => 150.00,
        output => 600.00
    },
    <<"o1-pro-2025-03-19">> => #{
        input => 150.00,
        output => 600.00
    },
    <<"o3">> => #{
        input => 10.00,
        cached_input => 2.50,
        output => 40.00,
        batch_input => 5.00,
        batch_cached_input => 1.25,
        batch_output => 20.00,
        flex_input => 5.00,
        flex_cached_input => 1.25,
        flex_output => 20.00
    },
    <<"o3-2025-04-16">> => #{
        input => 10.00,
        cached_input => 2.50,
        output => 40.00,
        batch_input => 5.00,
        batch_cached_input => 1.25,
        batch_output => 20.00,
        flex_input => 5.00,
        flex_cached_input => 1.25,
        flex_output => 20.00
    },
    <<"o4-mini">> => #{
        input => 1.10,
        cached_input => 0.275,
        output => 4.40,
        batch_input => 0.55,
        batch_cached_input => 0.138,
        batch_output => 2.20,
        flex_input => 0.55,
        flex_cached_input => 0.138,
        flex_output => 2.20
    },
    <<"o4-mini-2025-04-16">> => #{
        input => 1.10,
        cached_input => 0.275,
        output => 4.40,
        batch_input => 0.55,
        batch_cached_input => 0.138,
        batch_output => 2.20,
        flex_input => 0.55,
        flex_cached_input => 0.138,
        flex_output => 2.20
    },
    <<"o3-mini">> => #{
        input => 1.10,
        cached_input => 0.55,
        output => 4.40,
        batch_input => 0.55,
        batch_cached_input => 0.275,
        batch_output => 2.20
    },
    <<"o3-mini-2025-01-31">> => #{
        input => 1.10,
        cached_input => 0.55,
        output => 4.40,
        batch_input => 0.55,
        batch_cached_input => 0.275,
        batch_output => 2.20
    },
    <<"o1-mini">> => #{
        input => 1.10,
        cached_input => 0.55,
        output => 4.40,
        batch_input => 0.55,
        batch_cached_input => 0.275,
        batch_output => 2.20
    },
    <<"o1-mini-2024-09-12">> => #{
        input => 1.10,
        cached_input => 0.55,
        output => 4.40,
        batch_input => 0.55,
        batch_cached_input => 0.275,
        batch_output => 2.20
    },
    
    %% Specialized Models
    <<"codex-mini-latest">> => #{
        input => 1.50,
        cached_input => 0.375,
        output => 6.00,
        batch_input => 0.75,
        batch_cached_input => 0.1875,
        batch_output => 3.00
    },
    <<"gpt-4o-mini-search-preview">> => #{
        input => 0.15,
        output => 0.60
    },
    <<"gpt-4o-mini-search-preview-2025-03-11">> => #{
        input => 0.15,
        output => 0.60
    },
    <<"gpt-4o-search-preview">> => #{
        input => 2.50,
        output => 10.00
    },
    <<"gpt-4o-search-preview-2025-03-11">> => #{
        input => 2.50,
        output => 10.00
    },
    <<"computer-use-preview">> => #{
        input => 3.00,
        output => 12.00
    },
    <<"computer-use-preview-2025-03-11">> => #{
        input => 3.00,
        output => 12.00
    },
    
    %% Image Generation (GPT Image 1)
    <<"gpt-image-1">> => #{
        input => 5.00,
        cached_input => 1.25,
        image_input => 10.00,   % per 1M image tokens
        image_output => 40.00   % per 1M image tokens
    },
    
    %% TTS Models
    <<"gpt-4o-mini-tts">> => #{
        input => 0.60,
        audio_output => 12.00   % per 1M audio tokens
    },
    
    %% Transcription Models
    <<"gpt-4o-transcribe">> => #{
        input => 2.50,
        output => 10.00,
        audio_input => 6.00
    },
    <<"gpt-4o-mini-transcribe">> => #{
        input => 1.25,
        output => 5.00,
        audio_input => 3.00
    },
    
    %% Legacy TTS/Audio Models (per minute rates)
    <<"whisper-1">> => #{
        per_minute => 0.006
    },
    <<"tts-1">> => #{
        per_character => 0.000015  % $15 per 1M characters
    },
    <<"tts-1-hd">> => #{
        per_character => 0.00003   % $30 per 1M characters
    },
    
    %% Embeddings
    <<"text-embedding-3-small">> => #{
        input => 0.02,
        batch_input => 0.01
    },
    <<"text-embedding-3-large">> => #{
        input => 0.13,
        batch_input => 0.065
    },
    <<"text-embedding-ada-002">> => #{
        input => 0.10,
        batch_input => 0.05
    },
    
    %% Moderation (Free)
    <<"omni-moderation-latest">> => #{
        input => 0.0
    },
    <<"omni-moderation-2024-09-26">> => #{
        input => 0.0
    },
    <<"text-moderation-latest">> => #{
        input => 0.0
    },
    <<"text-moderation-007">> => #{
        input => 0.0
    },
    
    %% Legacy Chat Models
    <<"chatgpt-4o-latest">> => #{
        input => 5.00,
        output => 15.00,
        batch_input => 2.50,
        batch_output => 7.50
    },
    <<"gpt-4-turbo">> => #{
        input => 10.00,
        output => 30.00,
        batch_input => 5.00,
        batch_output => 15.00
    },
    <<"gpt-4-turbo-2024-04-09">> => #{
        input => 10.00,
        output => 30.00,
        batch_input => 5.00,
        batch_output => 15.00
    },
    <<"gpt-4">> => #{
        input => 30.00,
        output => 60.00,
        batch_input => 15.00,
        batch_output => 30.00
    },
    <<"gpt-4-0613">> => #{
        input => 30.00,
        output => 60.00,
        batch_input => 15.00,
        batch_output => 30.00
    },
    <<"gpt-4-32k">> => #{
        input => 60.00,
        output => 120.00,
        batch_input => 30.00,
        batch_output => 60.00
    },
    <<"gpt-3.5-turbo">> => #{
        input => 0.50,
        output => 1.50,
        batch_input => 0.25,
        batch_output => 0.75
    },
    <<"gpt-3.5-turbo-0125">> => #{
        input => 0.50,
        output => 1.50,
        batch_input => 0.25,
        batch_output => 0.75
    },
    <<"gpt-3.5-turbo-instruct">> => #{
        input => 1.50,
        output => 2.00,
        batch_input => 0.75,
        batch_output => 1.00
    },
    <<"gpt-3.5-turbo-16k-0613">> => #{
        input => 3.00,
        output => 4.00,
        batch_input => 1.50,
        batch_output => 2.00
    },
    <<"davinci-002">> => #{
        input => 2.00,
        output => 2.00,
        batch_input => 1.00,
        batch_output => 1.00
    },
    <<"babbage-002">> => #{
        input => 0.40,
        output => 0.40,
        batch_input => 0.20,
        batch_output => 0.20
    }
}).

%% Tool and feature pricing
-define(TOOL_PRICING, #{
    code_interpreter => 0.03,  % per container
    file_search_storage => 0.10,  % per GB/day (1GB free)
    file_search_calls => 2.50,    % per 1k calls (Responses API only)
    
    %% Web search pricing by model and context size
    web_search_gpt4_low => 30.00,      % per 1k calls
    web_search_gpt4_medium => 35.00,   % per 1k calls (default)
    web_search_gpt4_high => 50.00,     % per 1k calls
    web_search_gpt4mini_low => 25.00,  % per 1k calls
    web_search_gpt4mini_medium => 27.50, % per 1k calls (default)
    web_search_gpt4mini_high => 30.00  % per 1k calls
}).

%% Image generation pricing (per image)
-define(IMAGE_PRICING, #{
    %% GPT Image 1
    <<"gpt-image-1">> => #{
        low_1024x1024 => 0.011,
        low_1024x1536 => 0.016,
        low_1536x1024 => 0.016,
        medium_1024x1024 => 0.042,
        medium_1024x1536 => 0.063,
        medium_1536x1024 => 0.063,
        high_1024x1024 => 0.167,
        high_1024x1536 => 0.25,
        high_1536x1024 => 0.25
    },
    %% DALL-E 3
    <<"dall-e-3">> => #{
        standard_1024x1024 => 0.04,
        standard_1024x1792 => 0.08,
        standard_1792x1024 => 0.08,
        hd_1024x1024 => 0.08,
        hd_1024x1792 => 0.12,
        hd_1792x1024 => 0.12
    },
    %% DALL-E 2
    <<"dall-e-2">> => #{
        standard_256x256 => 0.016,
        standard_512x512 => 0.018,
        standard_1024x1024 => 0.02
    }
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Get pricing for a specific model
get_model_pricing(Model) ->
    gen_server:call(?MODULE, {get_model_pricing, Model}).

%% Get all current pricing data
get_all_pricing() ->
    gen_server:call(?MODULE, get_all_pricing).

%% Update pricing data (manual or from external source)
update_pricing(PricingData) ->
    gen_server:cast(?MODULE, {update_pricing, PricingData}).

%% Calculate cost for a specific usage
calculate_cost(Model, Usage, Options) ->
    gen_server:call(?MODULE, {calculate_cost, Model, Usage, Options}).

%% Get price history for a model
get_price_history(Model) ->
    gen_server:call(?MODULE, {get_price_history, Model}).

%% Get price trends (increase/decrease over time)
get_price_trends(Model) ->
    gen_server:call(?MODULE, {get_price_trends, Model}).

%% Export all pricing data for analysis
export_pricing_data() ->
    gen_server:call(?MODULE, export_pricing_data).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% Initialize with current pricing data
    PricingData = ?CURRENT_PRICING,
    Now = erlang:system_time(second),
    
    %% Create initial price history entries
    InitialHistory = maps:fold(fun(Model, Pricing, Acc) ->
        Entry = #price_entry{
            timestamp = Now,
            model = Model,
            pricing_data = Pricing,
            source = initial
        },
        [Entry | Acc]
    end, [], PricingData),
    
    %% Schedule periodic pricing updates (check every hour)
    erlang:send_after(3600000, self(), check_pricing_updates),
    
    {ok, #state{
        pricing_data = PricingData,
        price_history = InitialHistory,
        last_update = Now
    }}.

handle_call({get_model_pricing, Model}, _From, State) ->
    Result = case maps:get(Model, State#state.pricing_data, undefined) of
        undefined -> {error, model_not_found};
        Pricing -> {ok, Pricing}
    end,
    {reply, Result, State};

handle_call(get_all_pricing, _From, State) ->
    {reply, {ok, State#state.pricing_data}, State};

handle_call({calculate_cost, Model, Usage, Options}, _From, State) ->
    Result = calculate_usage_cost(Model, Usage, Options, State#state.pricing_data),
    {reply, Result, State};

handle_call({get_price_history, Model}, _From, State) ->
    History = lists:filter(fun(#price_entry{model = M}) -> 
        M =:= Model 
    end, State#state.price_history),
    {reply, {ok, History}, State};

handle_call({get_price_trends, Model}, _From, State) ->
    Trends = calculate_price_trends(Model, State#state.price_history),
    {reply, {ok, Trends}, State};

handle_call(export_pricing_data, _From, State) ->
    ExportData = #{
        current_pricing => State#state.pricing_data,
        price_history => State#state.price_history,
        last_update => State#state.last_update,
        tool_pricing => ?TOOL_PRICING,
        image_pricing => ?IMAGE_PRICING
    },
    {reply, {ok, ExportData}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({update_pricing, NewPricingData}, State) ->
    Now = erlang:system_time(second),
    
    %% Add entries to price history for changed models
    NewHistory = maps:fold(fun(Model, NewPricing, Acc) ->
        case maps:get(Model, State#state.pricing_data, undefined) of
            NewPricing -> Acc;  % No change
            _OldPricing -> 
                Entry = #price_entry{
                    timestamp = Now,
                    model = Model,
                    pricing_data = NewPricing,
                    source = update
                },
                [Entry | Acc]
        end
    end, State#state.price_history, NewPricingData),
    
    UpdatedPricing = maps:merge(State#state.pricing_data, NewPricingData),
    
    {noreply, State#state{
        pricing_data = UpdatedPricing,
        price_history = NewHistory,
        last_update = Now
    }};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_pricing_updates, State) ->
    %% In a real implementation, this would fetch from OpenAI API
    %% For now, we just schedule the next check
    erlang:send_after(State#state.update_interval, self(), check_pricing_updates),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

calculate_usage_cost(Model, Usage, Options, PricingData) ->
    case maps:get(Model, PricingData, undefined) of
        undefined -> 
            {error, model_not_found};
        Pricing ->
            try
                InputTokens = maps:get(prompt_tokens, Usage, 0),
                OutputTokens = maps:get(completion_tokens, Usage, 0),
                CachedTokens = maps:get(cached_tokens, Usage, 0),
                AudioInputTokens = maps:get(audio_input_tokens, Usage, 0),
                AudioOutputTokens = maps:get(audio_output_tokens, Usage, 0),
                ImageInputTokens = maps:get(image_input_tokens, Usage, 0),
                ImageOutputTokens = maps:get(image_output_tokens, Usage, 0),
                
                %% Check if using batch API
                IsBatch = maps:get(batch, Options, false),
                IsFlex = maps:get(flex, Options, false),
                
                %% Calculate costs based on token types
                InputCost = calculate_token_cost(InputTokens, get_input_price(Pricing, IsBatch, IsFlex)),
                OutputCost = calculate_token_cost(OutputTokens, get_output_price(Pricing, IsBatch, IsFlex)),
                CachedCost = calculate_token_cost(CachedTokens, get_cached_input_price(Pricing, IsBatch, IsFlex)),
                AudioInputCost = calculate_token_cost(AudioInputTokens, maps:get(audio_input, Pricing, 0.0)),
                AudioOutputCost = calculate_token_cost(AudioOutputTokens, maps:get(audio_output, Pricing, 0.0)),
                ImageInputCost = calculate_token_cost(ImageInputTokens, maps:get(image_input, Pricing, 0.0)),
                ImageOutputCost = calculate_token_cost(ImageOutputTokens, maps:get(image_output, Pricing, 0.0)),
                
                %% Calculate tool costs
                ToolCosts = calculate_tool_costs(maps:get(tools_used, Usage, []), Options),
                
                TotalCost = InputCost + OutputCost + CachedCost + AudioInputCost + 
                           AudioOutputCost + ImageInputCost + ImageOutputCost + ToolCosts,
                
                {ok, #{
                    total_cost => TotalCost,
                    input_cost => InputCost,
                    output_cost => OutputCost,
                    cached_cost => CachedCost,
                    audio_input_cost => AudioInputCost,
                    audio_output_cost => AudioOutputCost,
                    image_input_cost => ImageInputCost,
                    image_output_cost => ImageOutputCost,
                    tool_cost => ToolCosts,
                    input_tokens => InputTokens,
                    output_tokens => OutputTokens,
                    cached_tokens => CachedTokens,
                    pricing_used => Pricing
                }}
            catch
                _:Error ->
                    {error, {calculation_failed, Error}}
            end
    end.

get_input_price(Pricing, true, false) -> 
    maps:get(batch_input, Pricing, maps:get(input, Pricing, 0.0));
get_input_price(Pricing, false, true) ->
    maps:get(flex_input, Pricing, maps:get(input, Pricing, 0.0));
get_input_price(Pricing, _, _) -> 
    maps:get(input, Pricing, 0.0).

get_output_price(Pricing, true, false) -> 
    maps:get(batch_output, Pricing, maps:get(output, Pricing, 0.0));
get_output_price(Pricing, false, true) ->
    maps:get(flex_output, Pricing, maps:get(output, Pricing, 0.0));
get_output_price(Pricing, _, _) -> 
    maps:get(output, Pricing, 0.0).

get_cached_input_price(Pricing, true, false) -> 
    maps:get(batch_cached_input, Pricing, maps:get(cached_input, Pricing, 0.0));
get_cached_input_price(Pricing, false, true) ->
    maps:get(flex_cached_input, Pricing, maps:get(cached_input, Pricing, 0.0));
get_cached_input_price(Pricing, _, _) -> 
    maps:get(cached_input, Pricing, 0.0).

calculate_token_cost(Tokens, PricePerMillion) ->
    (Tokens / 1000000) * PricePerMillion.

calculate_tool_costs(Tools, Options) ->
    ToolPricing = ?TOOL_PRICING,
    lists:foldl(fun(Tool, Acc) ->
        ToolType = maps:get(type, Tool, unknown),
        ToolCost = case ToolType of
            web_search ->
                Model = maps:get(model, Options, <<"gpt-4o">>),
                Context = maps:get(web_search_context, Options, medium),
                get_web_search_cost(Model, Context, ToolPricing);
            code_interpreter ->
                maps:get(code_interpreter, ToolPricing, 0.0);
            file_search ->
                maps:get(file_search_calls, ToolPricing, 0.0) / 1000;
            _ ->
                0.0
        end,
        Acc + ToolCost
    end, 0.0, Tools).

get_web_search_cost(Model, Context, ToolPricing) ->
    Key = case {is_gpt4_mini_model(Model), Context} of
        {true, low} -> web_search_gpt4mini_low;
        {true, medium} -> web_search_gpt4mini_medium;
        {true, high} -> web_search_gpt4mini_high;
        {false, low} -> web_search_gpt4_low;
        {false, medium} -> web_search_gpt4_medium;
        {false, high} -> web_search_gpt4_high
    end,
    maps:get(Key, ToolPricing, 0.0) / 1000.

is_gpt4_mini_model(Model) ->
    binary:match(Model, <<"mini">>) =/= nomatch.

calculate_price_trends(Model, PriceHistory) ->
    ModelHistory = lists:filter(fun(#price_entry{model = M}) -> 
        M =:= Model 
    end, PriceHistory),
    
    SortedHistory = lists:sort(fun(#price_entry{timestamp = T1}, #price_entry{timestamp = T2}) ->
        T1 =< T2
    end, ModelHistory),
    
    case SortedHistory of
        [] -> #{trend => no_data};
        [_] -> #{trend => insufficient_data};
        [First | _] = History ->
            Last = lists:last(History),
            InputTrend = calculate_trend(
                maps:get(input, First#price_entry.pricing_data, 0.0),
                maps:get(input, Last#price_entry.pricing_data, 0.0)
            ),
            OutputTrend = calculate_trend(
                maps:get(output, First#price_entry.pricing_data, 0.0),
                maps:get(output, Last#price_entry.pricing_data, 0.0)
            ),
            #{
                trend => overall_trend(InputTrend, OutputTrend),
                input_trend => InputTrend,
                output_trend => OutputTrend,
                first_update => First#price_entry.timestamp,
                last_update => Last#price_entry.timestamp,
                total_updates => length(History)
            }
    end.

calculate_trend(OldPrice, NewPrice) when OldPrice > 0 ->
    Change = ((NewPrice - OldPrice) / OldPrice) * 100,
    if
        Change > 5 -> increasing;
        Change < -5 -> decreasing;
        true -> stable
    end;
calculate_trend(_, _) ->
    unknown.

overall_trend(increasing, increasing) -> increasing;
overall_trend(decreasing, decreasing) -> decreasing;
overall_trend(stable, stable) -> stable;
overall_trend(_, _) -> mixed.