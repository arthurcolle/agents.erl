%%%-------------------------------------------------------------------
%%% @doc Meta-Layer Coordinator
%%% Central coordination hub for all meta-systems. Manages interactions
%%% between self-healing, self-scaffolding, meta-cognitive engines,
%%% error monitoring, and hot code reloading systems.
%%% @end
%%%-------------------------------------------------------------------
-module(meta_layer_coordinator).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([register_meta_system/2, 
         unregister_meta_system/1,
         broadcast_meta_event/2,
         request_meta_action/2,
         get_meta_insights/0,
         get_meta_insights/1,
         coordinate_cross_system_action/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    meta_systems = #{},        % Registry of meta-systems
    feedback_loops = #{},      % Active feedback loops
    optimization_queue = [],   % Queue of optimization tasks
    meta_insights = #{},       % Collected insights from systems
    event_history = [],        % Recent meta-events
    cross_system_actions = #{} % Coordinated cross-system actions
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Register a meta-system with the coordinator
register_meta_system(SystemName, SystemPid) ->
    gen_server:call(?SERVER, {register_system, SystemName, SystemPid}).

%% @doc Unregister a meta-system
unregister_meta_system(SystemName) ->
    gen_server:call(?SERVER, {unregister_system, SystemName}).

%% @doc Broadcast an event to all meta-systems
broadcast_meta_event(EventType, EventData) ->
    gen_server:cast(?SERVER, {broadcast_event, EventType, EventData}).

%% @doc Request a meta-action from specific systems
request_meta_action(Action, Systems) ->
    gen_server:call(?SERVER, {request_action, Action, Systems}).

%% @doc Get insights from all meta-systems
get_meta_insights() ->
    gen_server:call(?SERVER, get_all_insights).

%% @doc Get insights from specific meta-system
get_meta_insights(SystemName) ->
    gen_server:call(?SERVER, {get_insights, SystemName}).

%% @doc Coordinate an action across multiple systems
coordinate_cross_system_action(ActionType, Parameters) ->
    gen_server:call(?SERVER, {coordinate_action, ActionType, Parameters}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Register with existing meta-systems
    self() ! connect_to_existing_systems,
    
    %% Start periodic optimization
    erlang:send_after(5000, self(), optimize_meta_systems),
    
    {ok, #state{}}.

handle_call({register_system, SystemName, SystemPid}, _From, State) ->
    NewSystems = maps:put(SystemName, SystemPid, State#state.meta_systems),
    
    %% Set up monitoring
    erlang:monitor(process, SystemPid),
    
    %% Initialize feedback loop
    FeedbackLoops = maps:put(SystemName, #{
        last_feedback => erlang:system_time(millisecond),
        feedback_count => 0
    }, State#state.feedback_loops),
    
    {reply, ok, State#state{
        meta_systems = NewSystems,
        feedback_loops = FeedbackLoops
    }};

handle_call({unregister_system, SystemName}, _From, State) ->
    NewSystems = maps:remove(SystemName, State#state.meta_systems),
    NewLoops = maps:remove(SystemName, State#state.feedback_loops),
    
    {reply, ok, State#state{
        meta_systems = NewSystems,
        feedback_loops = NewLoops
    }};

handle_call({request_action, Action, Systems}, _From, State) ->
    Results = perform_meta_action(Action, Systems, State),
    {reply, {ok, Results}, State};

handle_call(get_all_insights, _From, State) ->
    {reply, {ok, State#state.meta_insights}, State};

handle_call({get_insights, SystemName}, _From, State) ->
    Insights = maps:get(SystemName, State#state.meta_insights, #{}),
    {reply, {ok, Insights}, State};

handle_call({coordinate_action, ActionType, Parameters}, _From, State) ->
    %% Coordinate action across relevant systems
    Result = coordinate_action_impl(ActionType, Parameters, State),
    
    %% Record the coordinated action
    NewActions = maps:put(
        erlang:unique_integer(),
        #{
            type => ActionType,
            parameters => Parameters,
            timestamp => erlang:system_time(millisecond),
            result => Result
        },
        State#state.cross_system_actions
    ),
    
    {reply, Result, State#state{cross_system_actions = NewActions}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({broadcast_event, EventType, EventData}, State) ->
    %% Broadcast to all registered meta-systems
    maps:foreach(fun(_SystemName, SystemPid) ->
        SystemPid ! {meta_event, EventType, EventData}
    end, State#state.meta_systems),
    
    %% Record event
    NewHistory = [{EventType, EventData, erlang:system_time(millisecond)} 
                  | lists:sublist(State#state.event_history, 100)],
    
    {noreply, State#state{event_history = NewHistory}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(connect_to_existing_systems, State) ->
    %% Connect to known meta-systems
    NewState = connect_to_systems([
        {self_healing_supervisor, self_healing_supervisor},
        {auto_error_fixer, auto_error_fixer},
        {hot_code_reloader, hot_code_reloader},
        {meta_cognitive_awareness_engine, meta_cognitive_awareness_engine},
        {self_evolving_error_handler, self_evolving_error_handler}
    ], State),
    
    {noreply, NewState};

handle_info(optimize_meta_systems, State) ->
    %% Perform periodic optimization
    NewState = optimize_systems(State),
    
    %% Schedule next optimization
    erlang:send_after(10000, self(), optimize_meta_systems),
    
    {noreply, NewState};

handle_info({'DOWN', _MonitorRef, process, Pid, _Reason}, State) ->
    %% Handle crashed meta-system
    SystemName = find_system_by_pid(Pid, State#state.meta_systems),
    NewSystems = maps:remove(SystemName, State#state.meta_systems),
    
    %% Notify other systems
    broadcast_meta_event(system_down, #{system => SystemName}),
    
    {noreply, State#state{meta_systems = NewSystems}};

handle_info({meta_insight, SystemName, Insight}, State) ->
    %% Store insight from meta-system
    CurrentInsights = maps:get(SystemName, State#state.meta_insights, #{}),
    UpdatedInsights = maps:merge(CurrentInsights, Insight),
    
    NewInsights = maps:put(SystemName, UpdatedInsights, State#state.meta_insights),
    
    {noreply, State#state{meta_insights = NewInsights}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

connect_to_systems([], State) ->
    State;
connect_to_systems([{Name, Module} | Rest], State) ->
    case erlang:whereis(Module) of
        undefined ->
            connect_to_systems(Rest, State);
        Pid ->
            NewState = case register_meta_system(Name, Pid) of
                ok ->
                    State;
                _ ->
                    State
            end,
            connect_to_systems(Rest, NewState)
    end.

perform_meta_action(Action, Systems, State) ->
    %% Execute action on specified systems
    SystemPids = case Systems of
        all ->
            maps:values(State#state.meta_systems);
        List when is_list(List) ->
            [maps:get(S, State#state.meta_systems) || S <- List,
             maps:is_key(S, State#state.meta_systems)]
    end,
    
    %% Send action requests and collect results
    Results = lists:map(fun(Pid) ->
        try
            gen_server:call(Pid, {meta_action, Action}, 5000)
        catch
            _:_ -> {error, system_unavailable}
        end
    end, SystemPids),
    
    Results.

coordinate_action_impl(error_recovery, #{error := Error}, State) ->
    %% Coordinate error recovery across systems
    Actions = [
        {auto_error_fixer, {analyze_error, Error}},
        {self_healing_supervisor, {heal_error, Error}},
        {predictive_error_prevention, {prevent_similar, Error}}
    ],
    
    execute_coordinated_actions(Actions, State);

coordinate_action_impl(code_modification, #{module := Module, changes := Changes}, State) ->
    %% Coordinate code modification
    Actions = [
        {hot_code_reloader, {prepare_reload, Module}},
        {meta_cognitive_awareness_engine, {analyze_changes, Changes}},
        {self_evolving_error_handler, {update_patterns, Module}}
    ],
    
    execute_coordinated_actions(Actions, State);

coordinate_action_impl(system_optimization, #{target := Target}, State) ->
    %% Coordinate system optimization
    Actions = [
        {meta_ai_optimizer, {optimize, Target}},
        {cross_system_intelligence, {analyze_optimization, Target}},
        {unified_feedback_system, {collect_metrics, Target}}
    ],
    
    execute_coordinated_actions(Actions, State);

coordinate_action_impl(_, _, _) ->
    {error, unknown_action_type}.

execute_coordinated_actions(Actions, State) ->
    Results = lists:map(fun({SystemName, Action}) ->
        case maps:get(SystemName, State#state.meta_systems, undefined) of
            undefined ->
                {SystemName, {error, system_not_registered}};
            Pid ->
                try
                    Result = gen_server:call(Pid, Action, 5000),
                    {SystemName, Result}
                catch
                    _:_ ->
                        {SystemName, {error, action_failed}}
                end
        end
    end, Actions),
    
    {ok, Results}.

optimize_systems(State) ->
    %% Analyze feedback loops
    OptimizationTasks = analyze_feedback_loops(State#state.feedback_loops),
    
    %% Queue optimization tasks
    NewQueue = State#state.optimization_queue ++ OptimizationTasks,
    
    %% Process queue
    process_optimization_queue(State#state{optimization_queue = NewQueue}).

analyze_feedback_loops(FeedbackLoops) ->
    maps:fold(fun(SystemName, LoopData, Acc) ->
        case should_optimize(LoopData) of
            true ->
                [{optimize_system, SystemName} | Acc];
            false ->
                Acc
        end
    end, [], FeedbackLoops).

should_optimize(#{feedback_count := Count}) when Count > 100 ->
    true;
should_optimize(_) ->
    false.

process_optimization_queue(#state{optimization_queue = []} = State) ->
    State;
process_optimization_queue(#state{optimization_queue = [Task | Rest]} = State) ->
    %% Process one optimization task
    perform_optimization(Task, State),
    
    process_optimization_queue(State#state{optimization_queue = Rest}).

perform_optimization({optimize_system, SystemName}, State) ->
    case maps:get(SystemName, State#state.meta_systems, undefined) of
        undefined ->
            ok;
        Pid ->
            Pid ! {optimize_request, self()}
    end.

find_system_by_pid(Pid, Systems) ->
    case maps:to_list(Systems) of
        [] -> undefined;
        List ->
            case lists:keyfind(Pid, 2, List) of
                {Name, Pid} -> Name;
                false -> undefined
            end
    end.