%% agent_learning_protocol.erl
%% Low-level call-and-response protocol for agents to learn about the system
-module(agent_learning_protocol).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    register_agent/2,
    initiate_discovery/1,
    broadcast_query/2,
    respond_to_query/3,
    get_learning_history/1,
    get_system_knowledge/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Records
-record(state, {
    agents = #{} :: map(),  % agent_id => {pid, capabilities}
    queries = [] :: list(), % [{query_id, from_agent, query, timestamp}]
    responses = #{} :: map(), % query_id => [{from_agent, response, timestamp}]
    knowledge_graph = #{} :: map(), % entity => attributes
    learning_sessions = [] :: list()
}).

-record(query, {
    id :: binary(),
    type :: atom(),
    from :: binary(),
    content :: binary(),
    timestamp :: erlang:timestamp()
}).

-record(response, {
    query_id :: binary(),
    from :: binary(),
    content :: map(),
    confidence :: float(),
    timestamp :: erlang:timestamp()
}).

%% Logging macros
-define(LOG_INFO(Msg), colored_logger:data(processed, Msg)).
-define(LOG_INFO(Msg, Args), colored_logger:data(processed, io_lib:format(Msg, Args))).
-define(LOG_SUCCESS(Msg), colored_logger:complete(success, Msg)).
-define(LOG_ERROR(Msg), colored_logger:fire(inferno, Msg)).
-define(LOG_DISCOVERY(Msg), colored_logger:ocean(surface, Msg)).

%% API Implementation

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register_agent(AgentId, Capabilities) ->
    gen_server:call(?MODULE, {register_agent, AgentId, self(), Capabilities}).

initiate_discovery(AgentId) ->
    gen_server:call(?MODULE, {initiate_discovery, AgentId}).

broadcast_query(AgentId, Query) ->
    gen_server:cast(?MODULE, {broadcast_query, AgentId, Query}).

respond_to_query(QueryId, AgentId, Response) ->
    gen_server:cast(?MODULE, {respond_to_query, QueryId, AgentId, Response}).

get_learning_history(AgentId) ->
    gen_server:call(?MODULE, {get_learning_history, AgentId}).

get_system_knowledge(AgentId) ->
    gen_server:call(?MODULE, {get_system_knowledge, AgentId}).

%% gen_server callbacks

init([]) ->
    ?LOG_INFO("[LEARNING] Agent Learning Protocol initialized"),
    
    % Start periodic knowledge consolidation
    erlang:send_after(30000, self(), consolidate_knowledge),
    
    {ok, #state{}}.

handle_call({register_agent, AgentId, Pid, Capabilities}, _From, State) ->
    ?LOG_INFO("[LEARNING] Registering agent ~s with capabilities: ~p", [AgentId, Capabilities]),
    
    NewAgents = maps:put(AgentId, {Pid, Capabilities}, State#state.agents),
    
    % Broadcast new agent arrival to existing agents
    broadcast_event({agent_joined, AgentId, Capabilities}, State),
    
    {reply, ok, State#state{agents = NewAgents}};

handle_call({initiate_discovery, AgentId}, _From, State) ->
    ?LOG_DISCOVERY(io_lib:format("[DISCOVERY] Agent ~s initiating system discovery", [AgentId])),
    
    % Create discovery queries
    Queries = [
        {system_architecture, <<"What is the overall system architecture?">>},
        {agent_capabilities, <<"What capabilities do other agents have?">>},
        {communication_protocols, <<"What communication protocols are available?">>},
        {data_sources, <<"What data sources can I access?">>},
        {system_state, <<"What is the current system state?">>}
    ],
    
    % Send queries to all other agents
    QueryIds = lists:map(fun({Type, Content}) ->
        QueryId = generate_query_id(),
        Query = #query{
            id = QueryId,
            type = Type,
            from = AgentId,
            content = Content,
            timestamp = erlang:timestamp()
        },
        
        % Broadcast to all agents except the sender
        broadcast_query_internal(Query, AgentId, State),
        
        QueryId
    end, Queries),
    
    {reply, {ok, QueryIds}, State};

handle_call({get_learning_history, AgentId}, _From, State) ->
    % Get all queries and responses involving this agent
    AgentQueries = lists:filter(fun(#query{from = From}) ->
        From =:= AgentId
    end, State#state.queries),
    
    AgentResponses = maps:fold(fun(QueryId, Responses, Acc) ->
        AgentResps = lists:filter(fun(#response{from = From}) ->
            From =:= AgentId
        end, Responses),
        case AgentResps of
            [] -> Acc;
            _ -> [{QueryId, AgentResps} | Acc]
        end
    end, [], State#state.responses),
    
    History = #{
        queries_sent => length(AgentQueries),
        responses_given => length(AgentResponses),
        recent_queries => lists:sublist(lists:reverse(AgentQueries), 10),
        recent_responses => lists:sublist(lists:reverse(AgentResponses), 10)
    },
    
    {reply, {ok, History}, State};

handle_call({get_system_knowledge, AgentId}, _From, State) ->
    % Return the current knowledge graph
    {reply, {ok, State#state.knowledge_graph}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({broadcast_query, AgentId, QueryContent}, State) ->
    Query = #query{
        id = generate_query_id(),
        type = custom,
        from = AgentId,
        content = QueryContent,
        timestamp = erlang:timestamp()
    },
    
    broadcast_query_internal(Query, AgentId, State),
    NewQueries = [Query | State#state.queries],
    
    {noreply, State#state{queries = NewQueries}};

handle_cast({respond_to_query, QueryId, AgentId, ResponseContent}, State) ->
    ?LOG_INFO("[LEARNING] Agent ~s responding to query ~s", [AgentId, QueryId]),
    
    Response = #response{
        query_id = QueryId,
        from = AgentId,
        content = ResponseContent,
        confidence = maps:get(confidence, ResponseContent, 1.0),
        timestamp = erlang:timestamp()
    },
    
    % Store response
    CurrentResponses = maps:get(QueryId, State#state.responses, []),
    NewResponses = maps:put(QueryId, [Response | CurrentResponses], State#state.responses),
    
    % Update knowledge graph
    NewKnowledge = update_knowledge_graph(Response, State#state.knowledge_graph),
    
    % Notify WebSocket clients about the learning interaction
    notify_learning_event({
        response_received,
        QueryId,
        AgentId,
        ResponseContent
    }),
    
    {noreply, State#state{
        responses = NewResponses,
        knowledge_graph = NewKnowledge
    }};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(consolidate_knowledge, State) ->
    ?LOG_INFO("[LEARNING] Consolidating system knowledge"),
    
    % Analyze responses and update knowledge graph
    ConsolidatedKnowledge = consolidate_responses(State#state.responses, State#state.knowledge_graph),
    
    % Schedule next consolidation
    erlang:send_after(30000, self(), consolidate_knowledge),
    
    {noreply, State#state{knowledge_graph = ConsolidatedKnowledge}};

handle_info({agent_query, Query}, State) ->
    % Handle incoming query from another agent
    #query{id = QueryId, type = Type, from = FromAgent, content = Content} = Query,
    
    ?LOG_INFO("[LEARNING] Received query from ~s: ~s", [FromAgent, Content]),
    
    % Generate response based on query type and current knowledge
    Response = generate_response(Type, Content, State),
    
    % Send response back
    respond_to_query(QueryId, self_agent_id(), Response),
    
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

generate_query_id() ->
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    iolist_to_binary(io_lib:format("query_~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b", 
                                  [A, B, C, D, E])).

broadcast_query_internal(Query, SenderAgentId, State) ->
    % Send query to all registered agents except sender
    maps:foreach(fun(AgentId, {Pid, _Capabilities}) ->
        case AgentId =/= SenderAgentId of
            true ->
                % Send query to agent for processing
                Pid ! {agent_query, Query},
                
                % Also notify WebSocket for UI visualization
                notify_learning_event({
                    query_sent,
                    Query#query.id,
                    SenderAgentId,
                    AgentId,
                    Query#query.content
                });
            false ->
                ok
        end
    end, State#state.agents).

broadcast_event(Event, State) ->
    maps:foreach(fun(_AgentId, {Pid, _}) ->
        Pid ! {learning_protocol_event, Event}
    end, State#state.agents).

generate_response(system_architecture, _Content, _State) ->
    #{
        type => system_architecture,
        content => <<"Multi-agent Erlang system with distributed capabilities">>,
        details => #{
            core_components => [agent_instance, agent_supervisor, agent_registry],
            communication => [gen_server, message_passing, websocket],
            tools => [web_search, weather, calculations]
        },
        confidence => 0.95
    };

generate_response(agent_capabilities, _Content, State) ->
    Capabilities = maps:fold(fun(AgentId, {_Pid, Caps}, Acc) ->
        maps:put(AgentId, Caps, Acc)
    end, #{}, State#state.agents),
    
    #{
        type => agent_capabilities,
        content => <<"System has multiple agents with diverse capabilities">>,
        agents => Capabilities,
        confidence => 1.0
    };

generate_response(communication_protocols, _Content, _State) ->
    #{
        type => communication_protocols,
        content => <<"Agents communicate via Erlang message passing and WebSocket">>,
        protocols => [
            #{name => message_passing, description => <<"Direct Erlang process messages">>},
            #{name => gen_server, description => <<"Synchronous and asynchronous calls">>},
            #{name => websocket, description => <<"Real-time browser communication">>},
            #{name => streaming, description => <<"Token-by-token response streaming">>}
        ],
        confidence => 1.0
    };

generate_response(data_sources, _Content, _State) ->
    #{
        type => data_sources,
        content => <<"Multiple data sources available through tools">>,
        sources => [
            #{name => web_search, type => external, description => <<"Internet search capability">>},
            #{name => weather_api, type => external, description => <<"Weather information">>},
            #{name => knowledge_base, type => internal, description => <<"Stored system knowledge">>},
            #{name => conversation_history, type => internal, description => <<"Past interactions">>}
        ],
        confidence => 0.9
    };

generate_response(system_state, _Content, State) ->
    #{
        type => system_state,
        content => <<"System operational with active agents">>,
        metrics => #{
            active_agents => maps:size(State#state.agents),
            total_queries => length(State#state.queries),
            knowledge_entries => maps:size(State#state.knowledge_graph)
        },
        confidence => 1.0
    };

generate_response(custom, Content, State) ->
    % For custom queries, use knowledge graph to generate response
    #{
        type => custom,
        content => <<"Processing custom query based on current knowledge">>,
        query => Content,
        knowledge_used => maps:size(State#state.knowledge_graph),
        confidence => 0.7
    }.

update_knowledge_graph(#response{content = Content}, KnowledgeGraph) ->
    % Extract knowledge from response and update graph
    case maps:get(type, Content, undefined) of
        undefined -> KnowledgeGraph;
        Type ->
            TypeAtom = if
                is_binary(Type) -> binary_to_atom(Type, utf8);
                is_atom(Type) -> Type;
                true -> unknown
            end,
            
            CurrentKnowledge = maps:get(TypeAtom, KnowledgeGraph, #{}),
            UpdatedKnowledge = maps:merge(CurrentKnowledge, Content),
            maps:put(TypeAtom, UpdatedKnowledge, KnowledgeGraph)
    end.

consolidate_responses(ResponsesMap, KnowledgeGraph) ->
    % Analyze all responses and extract common patterns
    maps:fold(fun(_QueryId, Responses, AccGraph) ->
        lists:foldl(fun(Response, Graph) ->
            update_knowledge_graph(Response, Graph)
        end, AccGraph, Responses)
    end, KnowledgeGraph, ResponsesMap).

self_agent_id() ->
    % Get the ID of the current agent process
    case get(agent_id) of
        undefined -> <<"learning_protocol">>;
        Id -> Id
    end.

notify_learning_event(Event) ->
    % Send learning events to WebSocket for UI visualization
    case whereis(agent_ws_handler) of
        undefined ->
            % Try broadcasting instead
            agent_ws_handler:broadcast(#{
                type => <<"learning_event">>,
                event => format_learning_event(Event)
            });
        _Pid ->
            agent_ws_handler:broadcast(#{
                type => <<"learning_event">>,
                event => format_learning_event(Event)
            })
    end.

format_learning_event({query_sent, QueryId, From, To, Content}) ->
    #{
        action => <<"query_sent">>,
        query_id => QueryId,
        from_agent => From,
        to_agent => To,
        content => Content,
        timestamp => erlang:system_time(millisecond)
    };

format_learning_event({response_received, QueryId, From, Content}) ->
    #{
        action => <<"response_received">>,
        query_id => QueryId,
        from_agent => From,
        content => Content,
        timestamp => erlang:system_time(millisecond)
    };

format_learning_event({agent_joined, AgentId, Capabilities}) ->
    #{
        action => <<"agent_joined">>,
        agent_id => AgentId,
        capabilities => Capabilities,
        timestamp => erlang:system_time(millisecond)
    }.