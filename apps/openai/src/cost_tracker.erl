%%%-------------------------------------------------------------------
%%% @doc Cost Tracking Module
%%% Comprehensive cost tracking for all AI model usage
%%% @end
%%%-------------------------------------------------------------------
-module(cost_tracker).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    track_usage/3,
    track_usage/4,
    get_cost_summary/0,
    get_cost_summary/1,
    get_agent_costs/1,
    get_model_costs/1,
    get_costs_by_timerange/2,
    get_realtime_pricing/1,
    get_all_model_pricing/0,
    reset_costs/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-record(state, {
    costs = [],        % List of cost records
    total_cost = 0.0,  % Total accumulated cost
    start_time         % When tracking started
}).

-record(cost_record, {
    timestamp,
    agent_id,
    model,
    input_tokens = 0,
    output_tokens = 0,
    cached_input_tokens = 0,
    tool_calls = 0,
    input_cost = 0.0,
    output_cost = 0.0,
    cached_cost = 0.0,
    tool_cost = 0.0,
    total_cost = 0.0,
    metadata = #{}
}).

%% Model pricing per 1M tokens (in USD)
-define(MODEL_PRICING, #{
    %% GPT-4.1 Series
    <<"gpt-4.1">> => #{
        input => 2.00,
        cached_input => 0.50,
        output => 8.00
    },
    <<"gpt-4.1-mini">> => #{
        input => 0.10,
        cached_input => 0.025,
        output => 0.30
    },
    <<"gpt-4.1-nano">> => #{
        input => 0.05,
        cached_input => 0.0125,
        output => 0.15
    },
    
    %% GPT-4o Series  
    <<"gpt-4o">> => #{
        input => 2.50,
        cached_input => 1.25,
        output => 10.00
    },
    <<"gpt-4o-mini">> => #{
        input => 0.15,
        cached_input => 0.075,
        output => 0.60
    },
    <<"gpt-4o-audio-preview">> => #{
        input => 2.50,
        cached_input => 1.25,
        output => 10.00,
        audio_input => 100.00,  % per 1M audio tokens
        audio_output => 200.00  % per 1M audio tokens
    },
    
    %% Reasoning Models
    <<"o4-mini">> => #{
        input => 3.00,
        cached_input => 0.75,
        output => 12.00
    },
    <<"o3">> => #{
        input => 15.00,
        cached_input => 3.75,
        output => 60.00
    },
    <<"o3-mini">> => #{
        input => 1.10,
        cached_input => 0.275,
        output => 4.40
    },
    <<"o1">> => #{
        input => 15.00,
        cached_input => 3.75,
        output => 60.00
    },
    <<"o1-mini">> => #{
        input => 3.00,
        cached_input => 0.75,
        output => 12.00
    },
    <<"o1-pro">> => #{
        input => 100.00,
        cached_input => 25.00,
        output => 400.00
    },
    
    %% Legacy/Other
    <<"gpt-3.5-turbo">> => #{
        input => 0.50,
        cached_input => 0.25,
        output => 1.50
    }
}).

%% Tool pricing (per call)
-define(TOOL_PRICING, #{
    web_search => 2.00,    % $2 per 1000 calls
    computer_use => 5.00   % $5 per 1000 calls  
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Track usage for a model
track_usage(AgentId, Model, Usage) ->
    track_usage(AgentId, Model, Usage, #{}).

track_usage(AgentId, Model, Usage, Metadata) ->
    gen_server:cast(?MODULE, {track_usage, AgentId, Model, Usage, Metadata}).

%% Get cost summary for all agents
get_cost_summary() ->
    gen_server:call(?MODULE, get_cost_summary).

%% Get cost summary for a specific period (in seconds)
get_cost_summary(Period) ->
    gen_server:call(?MODULE, {get_cost_summary, Period}).

%% Get costs for a specific agent
get_agent_costs(AgentId) ->
    gen_server:call(?MODULE, {get_agent_costs, AgentId}).

%% Get costs for a specific model
get_model_costs(Model) ->
    gen_server:call(?MODULE, {get_model_costs, Model}).

%% Get costs within a time range
get_costs_by_timerange(StartTime, EndTime) ->
    gen_server:call(?MODULE, {get_costs_by_timerange, StartTime, EndTime}).

%% Get real-time pricing for a model
get_realtime_pricing(Model) ->
    gen_server:call(?MODULE, {get_realtime_pricing, Model}).

%% Get all current model pricing
get_all_model_pricing() ->
    gen_server:call(?MODULE, get_all_model_pricing).

%% Reset all cost tracking
reset_costs() ->
    gen_server:call(?MODULE, reset_costs).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% Start realtime pricing module if not already running
    case whereis(realtime_pricing) of
        undefined ->
            case realtime_pricing:start_link() of
                {ok, _} -> ok;
                {error, {already_started, _}} -> ok;
                Error -> 
                    io:format("[WARNING] Failed to start realtime_pricing: ~p~n", [Error])
            end;
        _ -> ok
    end,
    {ok, #state{start_time = erlang:system_time(second)}}.

handle_call(get_cost_summary, _From, State) ->
    Summary = calculate_summary(State#state.costs),
    {reply, {ok, Summary}, State};

handle_call({get_cost_summary, Period}, _From, State) ->
    Now = erlang:system_time(second),
    FilteredCosts = lists:filter(fun(#cost_record{timestamp = T}) ->
        (Now - T) =< Period
    end, State#state.costs),
    Summary = calculate_summary(FilteredCosts),
    {reply, {ok, Summary}, State};

handle_call({get_agent_costs, AgentId}, _From, State) ->
    AgentCosts = lists:filter(fun(#cost_record{agent_id = A}) ->
        A =:= AgentId
    end, State#state.costs),
    Summary = calculate_summary(AgentCosts),
    {reply, {ok, Summary}, State};

handle_call({get_model_costs, Model}, _From, State) ->
    ModelCosts = lists:filter(fun(#cost_record{model = M}) ->
        M =:= Model
    end, State#state.costs),
    Summary = calculate_summary(ModelCosts),
    {reply, {ok, Summary}, State};

handle_call({get_costs_by_timerange, StartTime, EndTime}, _From, State) ->
    RangeCosts = lists:filter(fun(#cost_record{timestamp = T}) ->
        T >= StartTime andalso T =< EndTime
    end, State#state.costs),
    Summary = calculate_summary(RangeCosts),
    {reply, {ok, Summary}, State};

handle_call({get_realtime_pricing, Model}, _From, State) ->
    Result = case whereis(realtime_pricing) of
        undefined -> 
            {error, realtime_pricing_not_available};
        _ ->
            realtime_pricing:get_model_pricing(Model)
    end,
    {reply, Result, State};

handle_call(get_all_model_pricing, _From, State) ->
    Result = case whereis(realtime_pricing) of
        undefined -> 
            {error, realtime_pricing_not_available};
        _ ->
            realtime_pricing:get_all_pricing()
    end,
    {reply, Result, State};

handle_call(reset_costs, _From, _State) ->
    NewState = #state{start_time = erlang:system_time(second)},
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({track_usage, AgentId, Model, Usage, Metadata}, State) ->
    CostRecord = calculate_cost_record(AgentId, Model, Usage, Metadata),
    NewCosts = [CostRecord | State#state.costs],
    NewTotal = State#state.total_cost + CostRecord#cost_record.total_cost,
    
    %% Log significant costs
    if
        CostRecord#cost_record.total_cost > 1.0 ->
            io:format("[COST WARNING] High cost usage: Agent ~p, Model ~p, Cost: $~.4f~n", 
                     [AgentId, Model, CostRecord#cost_record.total_cost]);
        true -> ok
    end,
    
    NewState = State#state{
        costs = NewCosts,
        total_cost = NewTotal
    },
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

calculate_cost_record(AgentId, Model, Usage, Metadata) ->
    %% Try to get real-time pricing first, fallback to static pricing
    Pricing = case whereis(realtime_pricing) of
        undefined -> 
            maps:get(Model, ?MODEL_PRICING, #{
                input => 0.0,
                output => 0.0,
                cached_input => 0.0
            });
        _ ->
            case realtime_pricing:get_model_pricing(Model) of
                {ok, RealTimePricing} -> RealTimePricing;
                {error, _} -> 
                    maps:get(Model, ?MODEL_PRICING, #{
                        input => 0.0,
                        output => 0.0,
                        cached_input => 0.0
                    })
            end
    end,
    
    %% Extract token counts
    InputTokens = maps:get(prompt_tokens, Usage, 0),
    OutputTokens = maps:get(completion_tokens, Usage, 0),
    CachedTokens = maps:get(cached_tokens, Usage, 0),
    ToolCalls = extract_tool_calls(Usage),
    
    %% Calculate costs (per million tokens)
    InputCost = (InputTokens / 1000000) * maps:get(input, Pricing, 0.0),
    OutputCost = (OutputTokens / 1000000) * maps:get(output, Pricing, 0.0),
    CachedCost = (CachedTokens / 1000000) * maps:get(cached_input, Pricing, 0.0),
    ToolCost = calculate_tool_cost(ToolCalls),
    
    TotalCost = InputCost + OutputCost + CachedCost + ToolCost,
    
    #cost_record{
        timestamp = erlang:system_time(second),
        agent_id = AgentId,
        model = Model,
        input_tokens = InputTokens,
        output_tokens = OutputTokens,
        cached_input_tokens = CachedTokens,
        tool_calls = length(ToolCalls),
        input_cost = InputCost,
        output_cost = OutputCost,
        cached_cost = CachedCost,
        tool_cost = ToolCost,
        total_cost = TotalCost,
        metadata = Metadata
    }.

extract_tool_calls(Usage) ->
    maps:get(tool_calls, Usage, []).

calculate_tool_cost(ToolCalls) ->
    lists:foldl(fun(Tool, Acc) ->
        ToolName = maps:get(name, Tool, unknown),
        ToolPrice = maps:get(ToolName, ?TOOL_PRICING, 0.0),
        Acc + (ToolPrice / 1000)  % Price is per 1000 calls
    end, 0.0, ToolCalls).

calculate_summary(CostRecords) ->
    {TotalCost, TotalInput, TotalOutput, TotalCached, TotalTools, ModelBreakdown, AgentBreakdown} = 
        lists:foldl(fun(#cost_record{} = R, {TC, TI, TO, TCA, TT, MB, AB}) ->
            %% Update model breakdown
            ModelCosts = maps:get(R#cost_record.model, MB, #{
                total_cost => 0.0,
                input_tokens => 0,
                output_tokens => 0,
                calls => 0
            }),
            NewModelCosts = ModelCosts#{
                total_cost => maps:get(total_cost, ModelCosts) + R#cost_record.total_cost,
                input_tokens => maps:get(input_tokens, ModelCosts) + R#cost_record.input_tokens,
                output_tokens => maps:get(output_tokens, ModelCosts) + R#cost_record.output_tokens,
                calls => maps:get(calls, ModelCosts) + 1
            },
            
            %% Update agent breakdown
            AgentCosts = maps:get(R#cost_record.agent_id, AB, #{
                total_cost => 0.0,
                calls => 0
            }),
            NewAgentCosts = AgentCosts#{
                total_cost => maps:get(total_cost, AgentCosts) + R#cost_record.total_cost,
                calls => maps:get(calls, AgentCosts) + 1
            },
            
            {TC + R#cost_record.total_cost,
             TI + R#cost_record.input_tokens,
             TO + R#cost_record.output_tokens,
             TCA + R#cost_record.cached_input_tokens,
             TT + R#cost_record.tool_calls,
             maps:put(R#cost_record.model, NewModelCosts, MB),
             maps:put(R#cost_record.agent_id, NewAgentCosts, AB)}
        end, {0.0, 0, 0, 0, 0, #{}, #{}}, CostRecords),
    
    #{
        total_cost => TotalCost,
        total_input_tokens => TotalInput,
        total_output_tokens => TotalOutput,
        total_cached_tokens => TotalCached,
        total_tool_calls => TotalTools,
        total_calls => length(CostRecords),
        average_cost_per_call => case length(CostRecords) of
            0 -> 0.0;
            N -> TotalCost / N
        end,
        model_breakdown => ModelBreakdown,
        agent_breakdown => AgentBreakdown,
        cost_records => length(CostRecords)
    }.