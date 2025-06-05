%%%-------------------------------------------------------------------
%%% @doc Bulk Agent Creator
%%% System for creating multiple AI agents with various configurations
%%% @end
%%%-------------------------------------------------------------------
-module(bulk_agent_creator).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    create_diverse_agent_fleet/1,
    create_agents_by_category/2,
    create_random_agents/1,
    get_fleet_status/0,
    scale_agents/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-record(state, {
    created_agents = [],
    creation_queue = [],
    active_creations = 0,
    max_concurrent = 10
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Create a diverse fleet of 50-100 agents
-spec create_diverse_agent_fleet(integer()) -> {ok, [map()]} | {error, term()}.
create_diverse_agent_fleet(Count) when Count > 0, Count =< 200 ->
    gen_server:call(?MODULE, {create_diverse_fleet, Count}, 30000).

%% @doc Create agents by specific categories
-spec create_agents_by_category([atom()], integer()) -> {ok, [map()]} | {error, term()}.
create_agents_by_category(Categories, CountPerCategory) ->
    gen_server:call(?MODULE, {create_by_category, Categories, CountPerCategory}, 30000).

%% @doc Create random agents for testing
-spec create_random_agents(integer()) -> {ok, [map()]} | {error, term()}.
create_random_agents(Count) ->
    gen_server:call(?MODULE, {create_random, Count}, 30000).

%% @doc Get status of all created agents
-spec get_fleet_status() -> {ok, map()}.
get_fleet_status() ->
    gen_server:call(?MODULE, get_fleet_status).

%% @doc Scale agents up or down
-spec scale_agents(up | down, integer()) -> {ok, map()}.
scale_agents(Direction, Count) ->
    gen_server:call(?MODULE, {scale_agents, Direction, Count}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({create_diverse_fleet, Count}, _From, State) ->
    try
        Agents = create_diverse_fleet_internal(Count),
        RegisteredAgents = register_agents_with_supervisor(Agents),
        NewState = State#state{created_agents = RegisteredAgents ++ State#state.created_agents},
        {reply, {ok, RegisteredAgents}, NewState}
    catch
        _:Error ->
            {reply, {error, Error}, State}
    end;

handle_call({create_by_category, Categories, CountPerCategory}, _From, State) ->
    try
        Agents = create_agents_by_categories(Categories, CountPerCategory),
        RegisteredAgents = register_agents_with_supervisor(Agents),
        NewState = State#state{created_agents = RegisteredAgents ++ State#state.created_agents},
        {reply, {ok, RegisteredAgents}, NewState}
    catch
        _:Error ->
            {reply, {error, Error}, State}
    end;

handle_call({create_random, Count}, _From, State) ->
    try
        Agents = create_random_agents_internal(Count),
        RegisteredAgents = register_agents_with_supervisor(Agents),
        NewState = State#state{created_agents = RegisteredAgents ++ State#state.created_agents},
        {reply, {ok, RegisteredAgents}, NewState}
    catch
        _:Error ->
            {reply, {error, Error}, State}
    end;

handle_call(get_fleet_status, _From, State) ->
    Status = calculate_fleet_status(State#state.created_agents),
    {reply, {ok, Status}, State};

handle_call({scale_agents, Direction, Count}, _From, State) ->
    try
        Result = case Direction of
            up ->
                NewAgents = create_random_agents_internal(Count),
                RegisteredAgents = register_agents_with_supervisor(NewAgents),
                UpdatedState = State#state{created_agents = RegisteredAgents ++ State#state.created_agents},
                {ok, #{action => scaled_up, count => length(RegisteredAgents)}};
            down ->
                {RemovedAgents, RemainingAgents} = remove_agents(State#state.created_agents, Count),
                lists:foreach(fun(Agent) ->
                    AgentId = maps:get(id, Agent),
                    agent_supervisor:stop_agent(AgentId)
                end, RemovedAgents),
                UpdatedState = State#state{created_agents = RemainingAgents},
                {ok, #{action => scaled_down, count => length(RemovedAgents)}}
        end,
        {reply, Result, UpdatedState}
    catch
        _:Error ->
            {reply, {error, Error}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

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

create_diverse_fleet_internal(Count) ->
    Templates = specialized_agent_templates:get_agent_templates(),
    
    %% Calculate how many agents per template type
    TemplateCount = length(Templates),
    AgentsPerTemplate = max(1, Count div TemplateCount),
    ExtraAgents = Count rem TemplateCount,
    
    %% Create agents with variations
    {MainAgents, _} = lists:foldl(fun(Template, {Acc, Remaining}) ->
        NumToCreate = case Remaining > 0 of
            true -> AgentsPerTemplate + 1;
            false -> AgentsPerTemplate
        end,
        
        Variations = create_template_variations(Template, NumToCreate),
        {Acc ++ Variations, max(0, Remaining - 1)}
    end, {[], ExtraAgents}, Templates),
    
    %% Ensure we have exactly the requested count
    lists:sublist(MainAgents, Count).

create_agents_by_categories(Categories, CountPerCategory) ->
    Templates = specialized_agent_templates:get_agent_templates(),
    
    lists:flatten(lists:map(fun(Category) ->
        CategoryTemplates = lists:filter(fun(Template) ->
            maps:get(specialization, Template) =:= Category
        end, Templates),
        
        case CategoryTemplates of
            [] -> [];
            _ ->
                lists:flatten(lists:map(fun(Template) ->
                    create_template_variations(Template, CountPerCategory)
                end, CategoryTemplates))
        end
    end, Categories)).

create_random_agents_internal(Count) ->
    Templates = specialized_agent_templates:get_agent_templates(),
    
    lists:map(fun(_) ->
        Template = lists:nth(rand:uniform(length(Templates)), Templates),
        create_agent_variation(Template)
    end, lists:seq(1, Count)).

create_template_variations(Template, Count) ->
    lists:map(fun(Index) ->
        create_agent_variation_with_index(Template, Index)
    end, lists:seq(1, Count)).

create_agent_variation(Template) ->
    create_agent_variation_with_index(Template, 1).

create_agent_variation_with_index(Template, Index) ->
    BaseName = maps:get(name, Template),
    VariationName = case Index of
        1 -> BaseName;
        _ -> <<BaseName/binary, " ", (integer_to_binary(Index))/binary>>
    end,
    
    %% Add some variation to the agent
    VariationTemp = add_temperature_variation(maps:get(temperature, Template, 0.5)),
    VariationTokens = add_token_variation(maps:get(max_tokens, Template, 2000)),
    
    #{
        id => generate_unique_agent_id(),
        name => VariationName,
        type => maps:get(type, Template),
        description => maps:get(description, Template),
        model => select_model_for_template(Template),
        temperature => VariationTemp,
        max_tokens => VariationTokens,
        tools => maps:get(tools, Template, []),
        system_prompt => enhance_system_prompt(maps:get(system_prompt, Template), Index),
        capabilities => maps:get(capabilities, Template, []),
        specialization => maps:get(specialization, Template),
        complexity_level => maps:get(complexity_level, Template, medium),
        status => <<"idle">>,
        created_at => erlang:system_time(second),
        last_activity => erlang:system_time(second),
        conversation_count => 0,
        success_rate => 1.0,
        average_response_time => 0,
        multi_turn_capable => true,
        function_calling_enabled => length(maps:get(tools, Template, [])) > 0,
        learning_rate => rand:uniform() * 0.1 + 0.01,  % For Q-learning
        exploration_rate => rand:uniform() * 0.3 + 0.1
    }.

add_temperature_variation(BaseTemp) ->
    Variation = (rand:uniform() - 0.5) * 0.2,  % ±0.1 variation
    max(0.0, min(1.0, BaseTemp + Variation)).

add_token_variation(BaseTokens) ->
    Variation = round((rand:uniform() - 0.5) * 1000),  % ±500 tokens
    max(1000, min(8000, BaseTokens + Variation)).

select_model_for_template(Template) ->
    ComplexityLevel = maps:get(complexity_level, Template, medium),
    case ComplexityLevel of
        expert -> 
            Models = [<<"gpt-4o">>, <<"gpt-4.1">>, <<"o3">>, <<"gpt-4o-2024-08-06">>],
            lists:nth(rand:uniform(length(Models)), Models);
        high -> 
            Models = [<<"gpt-4o">>, <<"gpt-4o-mini">>, <<"gpt-4.1-mini">>],
            lists:nth(rand:uniform(length(Models)), Models);
        medium -> 
            Models = [<<"gpt-4o-mini">>, <<"gpt-4.1-mini">>, <<"gpt-4.1-nano">>],
            lists:nth(rand:uniform(length(Models)), Models);
        low -> 
            Models = [<<"gpt-4o-mini">>, <<"gpt-4.1-nano">>, <<"gpt-3.5-turbo">>],
            lists:nth(rand:uniform(length(Models)), Models)
    end.

enhance_system_prompt(BasePrompt, Index) ->
    Enhancements = [
        <<" You are agent #", (integer_to_binary(Index))/binary, " in a multi-agent system.">>,
        <<" You can engage in multi-turn conversations and use function calling when appropriate.">>,
        <<" You learn from each interaction to improve your responses.">>,
        <<" You work collaboratively with other specialized agents when needed.">>
    ],
    
    Enhancement = lists:nth(rand:uniform(length(Enhancements)), Enhancements),
    <<BasePrompt/binary, Enhancement/binary>>.

generate_unique_agent_id() ->
    Timestamp = integer_to_binary(erlang:system_time(microsecond)),
    Random = integer_to_binary(rand:uniform(999999)),
    NodeHash = integer_to_binary(erlang:phash2(node())),
    <<Timestamp/binary, "_", Random/binary, "_", NodeHash/binary>>.

register_agents_with_supervisor(Agents) ->
    lists:map(fun(Agent) ->
        AgentId = maps:get(id, Agent),
        case agent_supervisor:start_agent(AgentId, Agent) of
            {ok, _Pid} ->
                Agent#{status => <<"active">>};
            {error, {already_started, _Pid}} ->
                Agent#{status => <<"active">>};
            {error, _Reason} ->
                Agent#{status => <<"error">>}
        end
    end, Agents).

calculate_fleet_status(Agents) ->
    Total = length(Agents),
    StatusCounts = lists:foldl(fun(Agent, Acc) ->
        Status = maps:get(status, Agent, <<"unknown">>),
        maps:update_with(Status, fun(Count) -> Count + 1 end, 1, Acc)
    end, #{}, Agents),
    
    SpecializationCounts = lists:foldl(fun(Agent, Acc) ->
        Spec = maps:get(specialization, Agent, unknown),
        maps:update_with(Spec, fun(Count) -> Count + 1 end, 1, Acc)
    end, #{}, Agents),
    
    ComplexityCounts = lists:foldl(fun(Agent, Acc) ->
        Level = maps:get(complexity_level, Agent, medium),
        maps:update_with(Level, fun(Count) -> Count + 1 end, 1, Acc)
    end, #{}, Agents),
    
    #{
        total_agents => Total,
        status_breakdown => StatusCounts,
        specialization_breakdown => SpecializationCounts,
        complexity_breakdown => ComplexityCounts,
        multi_turn_capable => length([A || A <- Agents, maps:get(multi_turn_capable, A, false)]),
        function_calling_enabled => length([A || A <- Agents, maps:get(function_calling_enabled, A, false)]),
        average_success_rate => case Total of
            0 -> 0.0;
            _ -> lists:sum([maps:get(success_rate, A, 0.0) || A <- Agents]) / Total
        end,
        created_at => erlang:system_time(second)
    }.

remove_agents(Agents, Count) ->
    ToRemove = lists:sublist(Agents, Count),
    Remaining = lists:nthtail(min(Count, length(Agents)), Agents),
    {ToRemove, Remaining}.