%%%-------------------------------------------------------------------
%%% @doc Real-time Metrics Engine
%%% Comprehensive real-time statistics and Q-table for reinforcement learning
%%% @end
%%%-------------------------------------------------------------------
-module(realtime_metrics_engine).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    record_agent_interaction/3,
    record_function_call/4,
    record_multi_turn_conversation/3,
    get_realtime_stats/0,
    get_agent_performance/1,
    get_q_table/0,
    update_q_value/4,
    get_system_health/0,
    get_performance_trends/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-record(state, {
    interaction_metrics = #{},
    agent_performance = #{},
    q_table = #{},
    system_metrics = #{},
    performance_history = [],
    websocket_clients = []
}).

-record(interaction_metric, {
    timestamp,
    agent_id,
    conversation_id,
    turn_count,
    response_time,
    function_calls,
    success,
    user_satisfaction,
    tokens_used,
    cost
}).

-record(agent_performance, {
    agent_id,
    total_interactions,
    success_rate,
    average_response_time,
    function_call_success_rate,
    multi_turn_proficiency,
    learning_progress,
    specialization_score,
    user_ratings,
    cost_efficiency,
    last_updated
}).

-record(q_state, {
    state_features,
    action_space,
    q_values,
    visits,
    last_updated
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Record an agent interaction for metrics
-spec record_agent_interaction(binary(), map(), map()) -> ok.
record_agent_interaction(AgentId, InteractionData, Result) ->
    gen_server:cast(?MODULE, {record_interaction, AgentId, InteractionData, Result}).

%% @doc Record a function call execution
-spec record_function_call(binary(), binary(), map(), boolean()) -> ok.
record_function_call(AgentId, FunctionName, Parameters, Success) ->
    gen_server:cast(?MODULE, {record_function_call, AgentId, FunctionName, Parameters, Success}).

%% @doc Record multi-turn conversation metrics
-spec record_multi_turn_conversation(binary(), integer(), map()) -> ok.
record_multi_turn_conversation(AgentId, TurnCount, ConversationMetrics) ->
    gen_server:cast(?MODULE, {record_multi_turn, AgentId, TurnCount, ConversationMetrics}).

%% @doc Get real-time system statistics
-spec get_realtime_stats() -> {ok, map()}.
get_realtime_stats() ->
    gen_server:call(?MODULE, get_realtime_stats).

%% @doc Get performance metrics for a specific agent
-spec get_agent_performance(binary()) -> {ok, map()} | {error, not_found}.
get_agent_performance(AgentId) ->
    gen_server:call(?MODULE, {get_agent_performance, AgentId}).

%% @doc Get the Q-table for reinforcement learning
-spec get_q_table() -> {ok, map()}.
get_q_table() ->
    gen_server:call(?MODULE, get_q_table).

%% @doc Update Q-value for reinforcement learning
-spec update_q_value(map(), binary(), float(), map()) -> ok.
update_q_value(State, Action, Reward, NextState) ->
    gen_server:cast(?MODULE, {update_q_value, State, Action, Reward, NextState}).

%% @doc Get overall system health metrics
-spec get_system_health() -> {ok, map()}.
get_system_health() ->
    gen_server:call(?MODULE, get_system_health).

%% @doc Get performance trends over time
-spec get_performance_trends(integer()) -> {ok, [map()]}.
get_performance_trends(TimePeriodSeconds) ->
    gen_server:call(?MODULE, {get_performance_trends, TimePeriodSeconds}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% Schedule periodic metrics collection
    erlang:send_after(1000, self(), collect_system_metrics),
    erlang:send_after(5000, self(), broadcast_metrics),
    
    %% Initialize Q-table with basic states
    InitialQTable = initialize_q_table(),
    
    {ok, #state{
        q_table = InitialQTable,
        system_metrics = #{
            start_time => erlang:system_time(second),
            total_interactions => 0,
            active_agents => 0,
            successful_interactions => 0,
            total_function_calls => 0,
            average_response_time => 0.0
        }
    }}.

handle_call(get_realtime_stats, _From, State) ->
    Stats = calculate_realtime_stats(State),
    {reply, {ok, Stats}, State};

handle_call({get_agent_performance, AgentId}, _From, State) ->
    case maps:get(AgentId, State#state.agent_performance, undefined) of
        undefined -> {reply, {error, not_found}, State};
        Performance -> {reply, {ok, format_agent_performance(Performance)}, State}
    end;

handle_call(get_q_table, _From, State) ->
    FormattedQTable = format_q_table(State#state.q_table),
    {reply, {ok, FormattedQTable}, State};

handle_call(get_system_health, _From, State) ->
    Health = calculate_system_health(State),
    {reply, {ok, Health}, State};

handle_call({get_performance_trends, TimePeriod}, _From, State) ->
    Now = erlang:system_time(second),
    CutoffTime = Now - TimePeriod,
    
    Trends = lists:filter(fun(Metric) ->
        maps:get(timestamp, Metric, 0) >= CutoffTime
    end, State#state.performance_history),
    
    {reply, {ok, Trends}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({record_interaction, AgentId, InteractionData, Result}, State) ->
    %% Update interaction metrics
    Metric = create_interaction_metric(AgentId, InteractionData, Result),
    
    %% Update agent performance
    UpdatedPerformance = update_agent_performance(AgentId, Metric, State#state.agent_performance),
    
    %% Update system metrics
    UpdatedSystemMetrics = update_system_metrics(Metric, State#state.system_metrics),
    
    %% Record for Q-learning
    QState = extract_state_features(InteractionData),
    Action = extract_action(InteractionData),
    Reward = calculate_reward(Result),
    
    NewState = State#state{
        agent_performance = UpdatedPerformance,
        system_metrics = UpdatedSystemMetrics
    },
    
    %% Update Q-table
    handle_cast({update_q_value, QState, Action, Reward, #{}}, NewState);

handle_cast({record_function_call, AgentId, FunctionName, Parameters, Success}, State) ->
    %% Update function call metrics
    FunctionMetric = #{
        timestamp => erlang:system_time(second),
        agent_id => AgentId,
        function_name => FunctionName,
        parameters => Parameters,
        success => Success
    },
    
    %% Update agent performance with function call data
    UpdatedPerformance = update_function_call_performance(AgentId, FunctionMetric, State#state.agent_performance),
    
    NewState = State#state{agent_performance = UpdatedPerformance},
    {noreply, NewState};

handle_cast({record_multi_turn, AgentId, TurnCount, ConversationMetrics}, State) ->
    %% Update multi-turn conversation metrics
    MultiTurnMetric = #{
        timestamp => erlang:system_time(second),
        agent_id => AgentId,
        turn_count => TurnCount,
        conversation_metrics => ConversationMetrics
    },
    
    %% Update agent performance with multi-turn data
    UpdatedPerformance = update_multi_turn_performance(AgentId, MultiTurnMetric, State#state.agent_performance),
    
    NewState = State#state{agent_performance = UpdatedPerformance},
    {noreply, NewState};

handle_cast({update_q_value, StateFeatures, Action, Reward, NextState}, State) ->
    UpdatedQTable = update_q_table(StateFeatures, Action, Reward, NextState, State#state.q_table),
    NewState = State#state{q_table = UpdatedQTable},
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(collect_system_metrics, State) ->
    %% Collect system-wide metrics
    SystemMetrics = collect_current_system_metrics(),
    
    %% Add to performance history
    HistoryEntry = SystemMetrics#{timestamp => erlang:system_time(second)},
    UpdatedHistory = [HistoryEntry | lists:sublist(State#state.performance_history, 99)],
    
    %% Schedule next collection
    erlang:send_after(1000, self(), collect_system_metrics),
    
    NewState = State#state{
        system_metrics = maps:merge(State#state.system_metrics, SystemMetrics),
        performance_history = UpdatedHistory
    },
    {noreply, NewState};

handle_info(broadcast_metrics, State) ->
    %% Broadcast metrics to WebSocket clients
    Metrics = calculate_realtime_stats(State),
    broadcast_to_websocket_clients(Metrics, State#state.websocket_clients),
    
    %% Schedule next broadcast
    erlang:send_after(5000, self(), broadcast_metrics),
    
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

initialize_q_table() ->
    %% Initialize with common state-action pairs for agent behavior
    States = [
        #{context => <<"technical_question">>, complexity => low, tools_available => true},
        #{context => <<"technical_question">>, complexity => medium, tools_available => true},
        #{context => <<"technical_question">>, complexity => high, tools_available => true},
        #{context => <<"creative_task">>, complexity => low, tools_available => false},
        #{context => <<"creative_task">>, complexity => medium, tools_available => false},
        #{context => <<"analytical_task">>, complexity => high, tools_available => true},
        #{context => <<"conversation">>, complexity => low, tools_available => false},
        #{context => <<"multi_turn">>, complexity => medium, tools_available => true}
    ],
    
    Actions = [
        <<"direct_response">>,
        <<"use_function_call">>,
        <<"ask_clarification">>,
        <<"escalate_to_specialist">>,
        <<"provide_examples">>,
        <<"request_more_context">>
    ],
    
    %% Initialize Q-values
    maps:from_list([
        {StateActionKey, #q_state{
            state_features = State,
            action_space = Actions,
            q_values = maps:from_list([{Action, 0.0} || Action <- Actions]),
            visits = 0,
            last_updated = erlang:system_time(second)
        }}
        || State <- States,
           StateActionKey <- [create_state_key(State)]
    ]).

create_interaction_metric(AgentId, InteractionData, Result) ->
    #interaction_metric{
        timestamp = erlang:system_time(second),
        agent_id = AgentId,
        conversation_id = maps:get(conversation_id, InteractionData, <<"unknown">>),
        turn_count = maps:get(turn_count, InteractionData, 1),
        response_time = maps:get(response_time, Result, 0),
        function_calls = length(maps:get(function_calls, Result, [])),
        success = maps:get(success, Result, true),
        user_satisfaction = maps:get(user_satisfaction, Result, 0.5),
        tokens_used = maps:get(tokens_used, Result, 0),
        cost = maps:get(cost, Result, 0.0)
    }.

update_agent_performance(AgentId, Metric, CurrentPerformance) ->
    Existing = maps:get(AgentId, CurrentPerformance, #agent_performance{
        agent_id = AgentId,
        total_interactions = 0,
        success_rate = 1.0,
        average_response_time = 0.0,
        function_call_success_rate = 1.0,
        multi_turn_proficiency = 0.5,
        learning_progress = 0.0,
        specialization_score = 0.5,
        user_ratings = [],
        cost_efficiency = 1.0,
        last_updated = erlang:system_time(second)
    }),
    
    NewTotal = Existing#agent_performance.total_interactions + 1,
    NewSuccessRate = calculate_running_average(
        Existing#agent_performance.success_rate,
        case Metric#interaction_metric.success of true -> 1.0; false -> 0.0 end,
        Existing#agent_performance.total_interactions,
        NewTotal
    ),
    
    NewAvgResponseTime = calculate_running_average(
        Existing#agent_performance.average_response_time,
        Metric#interaction_metric.response_time,
        Existing#agent_performance.total_interactions,
        NewTotal
    ),
    
    Updated = Existing#agent_performance{
        total_interactions = NewTotal,
        success_rate = NewSuccessRate,
        average_response_time = NewAvgResponseTime,
        user_ratings = [Metric#interaction_metric.user_satisfaction | 
                       lists:sublist(Existing#agent_performance.user_ratings, 99)],
        last_updated = erlang:system_time(second)
    },
    
    maps:put(AgentId, Updated, CurrentPerformance).

update_function_call_performance(AgentId, FunctionMetric, CurrentPerformance) ->
    case maps:get(AgentId, CurrentPerformance, undefined) of
        undefined -> CurrentPerformance;
        Existing ->
            NewFunctionSuccessRate = case maps:get(success, FunctionMetric) of
                true -> min(1.0, Existing#agent_performance.function_call_success_rate + 0.01);
                false -> max(0.0, Existing#agent_performance.function_call_success_rate - 0.02)
            end,
            
            Updated = Existing#agent_performance{
                function_call_success_rate = NewFunctionSuccessRate,
                last_updated = erlang:system_time(second)
            },
            
            maps:put(AgentId, Updated, CurrentPerformance)
    end.

update_multi_turn_performance(AgentId, MultiTurnMetric, CurrentPerformance) ->
    case maps:get(AgentId, CurrentPerformance, undefined) of
        undefined -> CurrentPerformance;
        Existing ->
            TurnCount = maps:get(turn_count, MultiTurnMetric),
            ProficiencyBoost = min(0.05, TurnCount * 0.01),
            
            NewProficiency = min(1.0, 
                Existing#agent_performance.multi_turn_proficiency + ProficiencyBoost),
            
            Updated = Existing#agent_performance{
                multi_turn_proficiency = NewProficiency,
                last_updated = erlang:system_time(second)
            },
            
            maps:put(AgentId, Updated, CurrentPerformance)
    end.

update_system_metrics(Metric, CurrentMetrics) ->
    CurrentMetrics#{
        total_interactions => maps:get(total_interactions, CurrentMetrics, 0) + 1,
        successful_interactions => maps:get(successful_interactions, CurrentMetrics, 0) + 
            case Metric#interaction_metric.success of true -> 1; false -> 0 end,
        total_function_calls => maps:get(total_function_calls, CurrentMetrics, 0) + 
            Metric#interaction_metric.function_calls,
        last_interaction => erlang:system_time(second)
    }.

collect_current_system_metrics() ->
    %% Collect real-time system metrics
    {TotalMemory, AllocatedMemory, _} = agent_memsup:get_memory_data(),
    CpuUtilization = case agent_cpu_sup:util([per_cpu]) of
        {error, _} -> 0.0;
        CpuData -> lists:sum([Util || {_, Util, _, _} <- CpuData]) / length(CpuData)
    end,
    
    ProcessCount = erlang:system_info(process_count),
    ProcessLimit = erlang:system_info(process_limit),
    
    #{
        memory_usage => #{
            total => TotalMemory,
            allocated => AllocatedMemory,
            utilization => (AllocatedMemory / TotalMemory) * 100
        },
        cpu_utilization => CpuUtilization,
        process_metrics => #{
            count => ProcessCount,
            limit => ProcessLimit,
            utilization => (ProcessCount / ProcessLimit) * 100
        },
        node_uptime => erlang:statistics(wall_clock),
        reduction_count => element(1, erlang:statistics(reductions))
    }.

update_q_table(StateFeatures, Action, Reward, NextState, QTable) ->
    StateKey = create_state_key(StateFeatures),
    
    case maps:get(StateKey, QTable, undefined) of
        undefined ->
            %% Create new Q-state entry
            QState = #q_state{
                state_features = StateFeatures,
                action_space = [Action],
                q_values = #{Action => Reward},
                visits = 1,
                last_updated = erlang:system_time(second)
            },
            maps:put(StateKey, QState, QTable);
        
        ExistingQState ->
            %% Update existing Q-state using Q-learning formula
            Alpha = 0.1,  % Learning rate
            Gamma = 0.9,  % Discount factor
            
            CurrentQ = maps:get(Action, ExistingQState#q_state.q_values, 0.0),
            
            %% Simple Q-learning update (without next state max for now)
            NewQ = CurrentQ + Alpha * (Reward - CurrentQ),
            
            UpdatedQValues = maps:put(Action, NewQ, ExistingQState#q_state.q_values),
            UpdatedQState = ExistingQState#q_state{
                q_values = UpdatedQValues,
                visits = ExistingQState#q_state.visits + 1,
                last_updated = erlang:system_time(second)
            },
            
            maps:put(StateKey, UpdatedQState, QTable)
    end.

create_state_key(StateFeatures) ->
    Context = maps:get(context, StateFeatures, <<"unknown">>),
    Complexity = maps:get(complexity, StateFeatures, medium),
    ToolsAvailable = maps:get(tools_available, StateFeatures, false),
    
    <<Context/binary, "_", (atom_to_binary(Complexity))/binary, "_", 
      (atom_to_binary(ToolsAvailable))/binary>>.

extract_state_features(InteractionData) ->
    %% Extract features from interaction data to create state representation
    #{
        context => maps:get(message_type, InteractionData, <<"conversation">>),
        complexity => determine_complexity(InteractionData),
        tools_available => maps:get(tools_available, InteractionData, false),
        turn_count => maps:get(turn_count, InteractionData, 1)
    }.

determine_complexity(InteractionData) ->
    MessageLength = byte_size(maps:get(message, InteractionData, <<>>)),
    if
        MessageLength < 50 -> low;
        MessageLength < 200 -> medium;
        true -> high
    end.

extract_action(InteractionData) ->
    %% Determine what action was taken based on interaction data
    case {maps:get(function_calls, InteractionData, []), 
          maps:get(clarification_requested, InteractionData, false)} of
        {[], false} -> <<"direct_response">>;
        {[_|_], false} -> <<"use_function_call">>;
        {_, true} -> <<"ask_clarification">>;
        _ -> <<"direct_response">>
    end.

calculate_reward(Result) ->
    %% Calculate reward based on interaction result
    BaseReward = case maps:get(success, Result, true) of
        true -> 1.0;
        false -> -0.5
    end,
    
    %% Adjust based on response time (faster = better)
    ResponseTime = maps:get(response_time, Result, 1000),
    TimeReward = case ResponseTime of
        T when T < 500 -> 0.2;
        T when T < 1000 -> 0.1;
        T when T < 2000 -> 0.0;
        _ -> -0.1
    end,
    
    %% Adjust based on user satisfaction
    SatisfactionReward = (maps:get(user_satisfaction, Result, 0.5) - 0.5) * 0.5,
    
    BaseReward + TimeReward + SatisfactionReward.

calculate_running_average(OldAvg, NewValue, OldCount, NewCount) ->
    (OldAvg * OldCount + NewValue) / NewCount.

calculate_realtime_stats(State) ->
    Now = erlang:system_time(second),
    
    %% Calculate aggregate performance metrics
    AllPerformance = maps:values(State#state.agent_performance),
    
    AvgSuccessRate = case AllPerformance of
        [] -> 0.0;
        _ -> lists:sum([P#agent_performance.success_rate || P <- AllPerformance]) / length(AllPerformance)
    end,
    
    AvgResponseTime = case AllPerformance of
        [] -> 0.0;
        _ -> lists:sum([P#agent_performance.average_response_time || P <- AllPerformance]) / length(AllPerformance)
    end,
    
    %% Q-table statistics
    QTableStats = analyze_q_table(State#state.q_table),
    
    #{
        timestamp => Now,
        system_metrics => State#state.system_metrics,
        agent_metrics => #{
            total_agents => length(AllPerformance),
            average_success_rate => AvgSuccessRate,
            average_response_time => AvgResponseTime,
            total_interactions => lists:sum([P#agent_performance.total_interactions || P <- AllPerformance])
        },
        q_learning_metrics => QTableStats,
        performance_trends => lists:sublist(State#state.performance_history, 20)
    }.

format_agent_performance(Performance) ->
    #{
        agent_id => Performance#agent_performance.agent_id,
        total_interactions => Performance#agent_performance.total_interactions,
        success_rate => Performance#agent_performance.success_rate,
        average_response_time => Performance#agent_performance.average_response_time,
        function_call_success_rate => Performance#agent_performance.function_call_success_rate,
        multi_turn_proficiency => Performance#agent_performance.multi_turn_proficiency,
        learning_progress => Performance#agent_performance.learning_progress,
        specialization_score => Performance#agent_performance.specialization_score,
        average_user_rating => case Performance#agent_performance.user_ratings of
            [] -> 0.0;
            Ratings -> lists:sum(Ratings) / length(Ratings)
        end,
        cost_efficiency => Performance#agent_performance.cost_efficiency,
        last_updated => Performance#agent_performance.last_updated
    }.

format_q_table(QTable) ->
    maps:map(fun(_StateKey, QState) ->
        #{
            state_features => QState#q_state.state_features,
            q_values => QState#q_state.q_values,
            visits => QState#q_state.visits,
            best_action => find_best_action(QState#q_state.q_values),
            last_updated => QState#q_state.last_updated
        }
    end, QTable).

find_best_action(QValues) ->
    case maps:size(QValues) of
        0 -> <<"none">>;
        _ ->
            {BestAction, _BestValue} = lists:max([{Action, Value} || {Action, Value} <- maps:to_list(QValues)]),
            BestAction
    end.

analyze_q_table(QTable) ->
    QStates = maps:values(QTable),
    
    case QStates of
        [] ->
            #{
                total_states => 0,
                total_visits => 0,
                average_q_value => 0.0,
                exploration_rate => 0.0
            };
        _ ->
            TotalVisits = lists:sum([QS#q_state.visits || QS <- QStates]),
            AllQValues = lists:flatten([maps:values(QS#q_state.q_values) || QS <- QStates]),
            
            AvgQValue = case AllQValues of
                [] -> 0.0;
                _ -> lists:sum(AllQValues) / length(AllQValues)
            end,
            
            %% Calculate exploration rate (states with low visit counts)
            LowVisitStates = length([QS || QS <- QStates, QS#q_state.visits < 5]),
            ExplorationRate = case length(QStates) of
                0 -> 0.0;
                Total -> LowVisitStates / Total
            end,
            
            #{
                total_states => length(QStates),
                total_visits => TotalVisits,
                average_q_value => AvgQValue,
                exploration_rate => ExplorationRate,
                most_visited_state => find_most_visited_state(QStates),
                highest_q_value => case AllQValues of [] -> 0.0; _ -> lists:max(AllQValues) end
            }
    end.

find_most_visited_state(QStates) ->
    case QStates of
        [] -> <<"none">>;
        _ ->
            MostVisited = lists:max(QStates, fun(A, B) -> 
                A#q_state.visits >= B#q_state.visits 
            end),
            create_state_key(MostVisited#q_state.state_features)
    end.

calculate_system_health(State) ->
    SystemMetrics = State#state.system_metrics,
    
    %% Calculate health scores
    MemoryHealth = case maps:get(memory_usage, SystemMetrics, #{}) of
        #{utilization := MemUtil} when MemUtil < 70 -> good;
        #{utilization := MemUtil} when MemUtil < 85 -> warning;
        _ -> critical
    end,
    
    CpuHealth = case maps:get(cpu_utilization, SystemMetrics, 0) of
        CpuUtil when CpuUtil < 60 -> good;
        CpuUtil when CpuUtil < 80 -> warning;
        _ -> critical
    end,
    
    ProcessHealth = case maps:get(process_metrics, SystemMetrics, #{}) of
        #{utilization := ProcUtil} when ProcUtil < 70 -> good;
        #{utilization := ProcUtil} when ProcUtil < 85 -> warning;
        _ -> critical
    end,
    
    %% Overall system health
    OverallHealth = case {MemoryHealth, CpuHealth, ProcessHealth} of
        {good, good, good} -> excellent;
        {_, _, _} when MemoryHealth =/= critical, CpuHealth =/= critical, ProcessHealth =/= critical -> good;
        _ -> degraded
    end,
    
    #{
        overall_health => OverallHealth,
        memory_health => MemoryHealth,
        cpu_health => CpuHealth,
        process_health => ProcessHealth,
        system_metrics => SystemMetrics,
        timestamp => erlang:system_time(second)
    }.

broadcast_to_websocket_clients(_Metrics, []) ->
    ok;
broadcast_to_websocket_clients(Metrics, Clients) ->
    Message = jsx:encode(#{
        type => <<"realtime_metrics">>,
        data => Metrics,
        timestamp => erlang:system_time(second)
    }),
    
    lists:foreach(fun(Client) ->
        try
            Client ! {websocket_message, Message}
        catch
            _:_ -> ok  % Client might be disconnected
        end
    end, Clients).