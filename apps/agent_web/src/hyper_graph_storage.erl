-module(hyper_graph_storage).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([create_node/2, create_edge/3, create_hyperedge/2]).
-export([get_node/1, get_edge/1, get_hyperedge/1]).
-export([find_nodes/1, find_edges/1, traverse_graph/2]).
-export([create_symbol/2, get_symbol/1, link_symbols/2]).
-export([vector_search/2, semantic_query/1, compute_embeddings/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    nodes = #{} :: map(),
    edges = #{} :: map(),
    hyperedges = #{} :: map(),
    symbols = #{} :: map(),
    embeddings = #{} :: map(),
    indices = #{} :: map(),
    storage_path = "priv/data/hypergraph.json" :: string()
}).

-record(hyper_node, {
    id :: binary(),
    type :: binary(),
    properties = #{} :: map(),
    metadata = #{} :: map(),
    created_at :: integer(),
    updated_at :: integer()
}).

-record(hyper_edge, {
    id :: binary(),
    source_id :: binary(),
    target_id :: binary(),
    type :: binary(),
    properties = #{} :: map(),
    weight = 1.0 :: float(),
    created_at :: integer()
}).

-record(hyper_hyperedge, {
    id :: binary(),
    node_ids = [] :: list(binary()),
    type :: binary(),
    properties = #{} :: map(),
    weight = 1.0 :: float(),
    created_at :: integer()
}).

-record(hyper_symbol, {
    id :: binary(),
    name :: binary(),
    symbol_type :: binary(),
    content :: term(),
    embeddings = #{} :: map(),
    linked_symbols = [] :: list(binary()),
    metadata = #{} :: map(),
    created_at :: integer(),
    updated_at :: integer()
}).

%%% API Functions

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

create_node(Type, Properties) ->
    gen_server:call(?MODULE, {create_node, Type, Properties}).

create_edge(SourceId, TargetId, Type) ->
    gen_server:call(?MODULE, {create_edge, SourceId, TargetId, Type}).

create_hyperedge(NodeIds, Type) ->
    gen_server:call(?MODULE, {create_hyperedge, NodeIds, Type}).

get_node(NodeId) ->
    gen_server:call(?MODULE, {get_node, NodeId}).

get_edge(EdgeId) ->
    gen_server:call(?MODULE, {get_edge, EdgeId}).

get_hyperedge(HyperedgeId) ->
    gen_server:call(?MODULE, {get_hyperedge, HyperedgeId}).

find_nodes(Query) ->
    gen_server:call(?MODULE, {find_nodes, Query}).

find_edges(Query) ->
    gen_server:call(?MODULE, {find_edges, Query}).

traverse_graph(StartNodeId, TraversalType) ->
    gen_server:call(?MODULE, {traverse_graph, StartNodeId, TraversalType}).

create_symbol(Name, Content) ->
    gen_server:call(?MODULE, {create_symbol, Name, Content}).

get_symbol(SymbolId) ->
    gen_server:call(?MODULE, {get_symbol, SymbolId}).

link_symbols(SymbolId1, SymbolId2) ->
    gen_server:call(?MODULE, {link_symbols, SymbolId1, SymbolId2}).

vector_search(QueryVector, TopK) ->
    gen_server:call(?MODULE, {vector_search, QueryVector, TopK}).

semantic_query(Query) ->
    gen_server:call(?MODULE, {semantic_query, Query}).

compute_embeddings(Content, Model) ->
    gen_server:call(?MODULE, {compute_embeddings, Content, Model}).

%%% GenServer Callbacks

init([]) ->
    ?LOG_INFO("Starting HyperGraph Storage System"),
    StoragePath = "apps/agent_web/priv/data/hypergraph.json",
    State = load_hypergraph(StoragePath),
    {ok, State}.

handle_call({create_node, Type, Properties}, _From, State) ->
    try
        NodeId = generate_id(<<"node">>),
        Now = erlang:system_time(second),
        
        Node = #hyper_node{
            id = NodeId,
            type = Type,
            properties = Properties,
            metadata = #{},
            created_at = Now,
            updated_at = Now
        },
        
        NewNodes = maps:put(NodeId, Node, State#state.nodes),
        NewState = State#state{nodes = NewNodes},
        save_hypergraph(NewState),
        
        ?LOG_INFO("Created node: ~p", [NodeId]),
        {reply, {ok, NodeId}, NewState}
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to create node: ~p:~p", [Error, Reason]),
            {reply, {error, {creation_failed, Reason}}, State}
    end;

handle_call({create_edge, SourceId, TargetId, Type}, _From, State) ->
    case {maps:find(SourceId, State#state.nodes), maps:find(TargetId, State#state.nodes)} of
        {{ok, _}, {ok, _}} ->
            try
                EdgeId = generate_id(<<"edge">>),
                Now = erlang:system_time(second),
                
                Edge = #hyper_edge{
                    id = EdgeId,
                    source_id = SourceId,
                    target_id = TargetId,
                    type = Type,
                    properties = #{},
                    weight = 1.0,
                    created_at = Now
                },
                
                NewEdges = maps:put(EdgeId, Edge, State#state.edges),
                NewState = State#state{edges = NewEdges},
                save_hypergraph(NewState),
                
                ?LOG_INFO("Created edge: ~p", [EdgeId]),
                {reply, {ok, EdgeId}, NewState}
            catch
                Error:Reason ->
                    ?LOG_ERROR("Failed to create edge: ~p:~p", [Error, Reason]),
                    {reply, {error, {creation_failed, Reason}}, State}
            end;
        _ ->
            {reply, {error, invalid_node_ids}, State}
    end;

handle_call({create_hyperedge, NodeIds, Type}, _From, State) ->
    ValidNodes = lists:all(fun(NodeId) ->
        maps:is_key(NodeId, State#state.nodes)
    end, NodeIds),
    
    case ValidNodes of
        true ->
            try
                HyperedgeId = generate_id(<<"hyperedge">>),
                Now = erlang:system_time(second),
                
                Hyperedge = #hyper_hyperedge{
                    id = HyperedgeId,
                    node_ids = NodeIds,
                    type = Type,
                    properties = #{},
                    weight = 1.0,
                    created_at = Now
                },
                
                NewHyperedges = maps:put(HyperedgeId, Hyperedge, State#state.hyperedges),
                NewState = State#state{hyperedges = NewHyperedges},
                save_hypergraph(NewState),
                
                ?LOG_INFO("Created hyperedge: ~p", [HyperedgeId]),
                {reply, {ok, HyperedgeId}, NewState}
            catch
                Error:Reason ->
                    ?LOG_ERROR("Failed to create hyperedge: ~p:~p", [Error, Reason]),
                    {reply, {error, {creation_failed, Reason}}, State}
            end;
        false ->
            {reply, {error, invalid_node_ids}, State}
    end;

handle_call({get_node, NodeId}, _From, State) ->
    case maps:find(NodeId, State#state.nodes) of
        {ok, Node} ->
            {reply, {ok, node_to_map(Node)}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_edge, EdgeId}, _From, State) ->
    case maps:find(EdgeId, State#state.edges) of
        {ok, Edge} ->
            {reply, {ok, edge_to_map(Edge)}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_hyperedge, HyperedgeId}, _From, State) ->
    case maps:find(HyperedgeId, State#state.hyperedges) of
        {ok, Hyperedge} ->
            {reply, {ok, hyperedge_to_map(Hyperedge)}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({find_nodes, Query}, _From, State) ->
    try
        MatchingNodes = find_matching_nodes(Query, State#state.nodes),
        {reply, {ok, MatchingNodes}, State}
    catch
        Error:Reason ->
            ?LOG_ERROR("Node search failed: ~p:~p", [Error, Reason]),
            {reply, {error, {search_failed, Reason}}, State}
    end;

handle_call({find_edges, Query}, _From, State) ->
    try
        MatchingEdges = find_matching_edges(Query, State#state.edges),
        {reply, {ok, MatchingEdges}, State}
    catch
        Error:Reason ->
            ?LOG_ERROR("Edge search failed: ~p:~p", [Error, Reason]),
            {reply, {error, {search_failed, Reason}}, State}
    end;

handle_call({traverse_graph, StartNodeId, TraversalType}, _From, State) ->
    case maps:find(StartNodeId, State#state.nodes) of
        {ok, _} ->
            try
                TraversalResult = perform_traversal(StartNodeId, TraversalType, State),
                {reply, {ok, TraversalResult}, State}
            catch
                Error:Reason ->
                    ?LOG_ERROR("Graph traversal failed: ~p:~p", [Error, Reason]),
                    {reply, {error, {traversal_failed, Reason}}, State}
            end;
        error ->
            {reply, {error, node_not_found}, State}
    end;

handle_call({create_symbol, Name, Content}, _From, State) ->
    try
        SymbolId = generate_id(<<"symbol">>),
        Now = erlang:system_time(second),
        
        Symbol = #hyper_symbol{
            id = SymbolId,
            name = Name,
            symbol_type = infer_symbol_type(Content),
            content = Content,
            embeddings = #{},
            linked_symbols = [],
            metadata = #{},
            created_at = Now,
            updated_at = Now
        },
        
        NewSymbols = maps:put(SymbolId, Symbol, State#state.symbols),
        NewState = State#state{symbols = NewSymbols},
        save_hypergraph(NewState),
        
        ?LOG_INFO("Created symbol: ~p", [SymbolId]),
        {reply, {ok, SymbolId}, NewState}
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to create symbol: ~p:~p", [Error, Reason]),
            {reply, {error, {creation_failed, Reason}}, State}
    end;

handle_call({get_symbol, SymbolId}, _From, State) ->
    case maps:find(SymbolId, State#state.symbols) of
        {ok, Symbol} ->
            {reply, {ok, symbol_to_map(Symbol)}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({link_symbols, SymbolId1, SymbolId2}, _From, State) ->
    case {maps:find(SymbolId1, State#state.symbols), maps:find(SymbolId2, State#state.symbols)} of
        {{ok, Symbol1}, {ok, Symbol2}} ->
            try
                UpdatedSymbol1 = Symbol1#hyper_symbol{
                    linked_symbols = [SymbolId2 | Symbol1#hyper_symbol.linked_symbols],
                    updated_at = erlang:system_time(second)
                },
                UpdatedSymbol2 = Symbol2#hyper_symbol{
                    linked_symbols = [SymbolId1 | Symbol2#hyper_symbol.linked_symbols],
                    updated_at = erlang:system_time(second)
                },
                
                NewSymbols = maps:put(SymbolId1, UpdatedSymbol1,
                              maps:put(SymbolId2, UpdatedSymbol2, State#state.symbols)),
                NewState = State#state{symbols = NewSymbols},
                save_hypergraph(NewState),
                
                ?LOG_INFO("Linked symbols: ~p <-> ~p", [SymbolId1, SymbolId2]),
                {reply, ok, NewState}
            catch
                Error:Reason ->
                    ?LOG_ERROR("Failed to link symbols: ~p:~p", [Error, Reason]),
                    {reply, {error, {linking_failed, Reason}}, State}
            end;
        _ ->
            {reply, {error, symbol_not_found}, State}
    end;

handle_call({vector_search, QueryVector, TopK}, _From, State) ->
    try
        Results = perform_vector_search(QueryVector, TopK, State#state.embeddings),
        {reply, {ok, Results}, State}
    catch
        Error:Reason ->
            ?LOG_ERROR("Vector search failed: ~p:~p", [Error, Reason]),
            {reply, {error, {search_failed, Reason}}, State}
    end;

handle_call({semantic_query, Query}, _From, State) ->
    try
        Results = perform_semantic_query(Query, State),
        {reply, {ok, Results}, State}
    catch
        Error:Reason ->
            ?LOG_ERROR("Semantic query failed: ~p:~p", [Error, Reason]),
            {reply, {error, {query_failed, Reason}}, State}
    end;

handle_call({compute_embeddings, Content, Model}, _From, State) ->
    try
        Embeddings = compute_content_embeddings(Content, Model),
        {reply, {ok, Embeddings}, State}
    catch
        Error:Reason ->
            ?LOG_ERROR("Embedding computation failed: ~p:~p", [Error, Reason]),
            {reply, {error, {computation_failed, Reason}}, State}
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

%%% Internal Functions

generate_id(Prefix) ->
    Timestamp = integer_to_binary(erlang:system_time(microsecond)),
    Random = integer_to_binary(rand:uniform(999999)),
    <<Prefix/binary, "_", Timestamp/binary, "_", Random/binary>>.

node_to_map(#hyper_node{} = Node) ->
    #{
        id => Node#hyper_node.id,
        type => Node#hyper_node.type,
        properties => Node#hyper_node.properties,
        metadata => Node#hyper_node.metadata,
        created_at => Node#hyper_node.created_at,
        updated_at => Node#hyper_node.updated_at
    }.

edge_to_map(#hyper_edge{} = Edge) ->
    #{
        id => Edge#hyper_edge.id,
        source_id => Edge#hyper_edge.source_id,
        target_id => Edge#hyper_edge.target_id,
        type => Edge#hyper_edge.type,
        properties => Edge#hyper_edge.properties,
        weight => Edge#hyper_edge.weight,
        created_at => Edge#hyper_edge.created_at
    }.

hyperedge_to_map(#hyper_hyperedge{} = Hyperedge) ->
    #{
        id => Hyperedge#hyper_hyperedge.id,
        node_ids => Hyperedge#hyper_hyperedge.node_ids,
        type => Hyperedge#hyper_hyperedge.type,
        properties => Hyperedge#hyper_hyperedge.properties,
        weight => Hyperedge#hyper_hyperedge.weight,
        created_at => Hyperedge#hyper_hyperedge.created_at
    }.

symbol_to_map(#hyper_symbol{} = Symbol) ->
    #{
        id => Symbol#hyper_symbol.id,
        name => Symbol#hyper_symbol.name,
        symbol_type => Symbol#hyper_symbol.symbol_type,
        content => Symbol#hyper_symbol.content,
        embeddings => Symbol#hyper_symbol.embeddings,
        linked_symbols => Symbol#hyper_symbol.linked_symbols,
        metadata => Symbol#hyper_symbol.metadata,
        created_at => Symbol#hyper_symbol.created_at,
        updated_at => Symbol#hyper_symbol.updated_at
    }.

find_matching_nodes(Query, Nodes) ->
    maps:fold(fun(_Id, Node, Acc) ->
        case match_node(Node, Query) of
            true -> [node_to_map(Node) | Acc];
            false -> Acc
        end
    end, [], Nodes).

find_matching_edges(Query, Edges) ->
    maps:fold(fun(_Id, Edge, Acc) ->
        case match_edge(Edge, Query) of
            true -> [edge_to_map(Edge) | Acc];
            false -> Acc
        end
    end, [], Edges).

match_node(Node, Query) ->
    case maps:get(type, Query, undefined) of
        undefined -> true;
        Type -> Node#hyper_node.type =:= Type
    end.

match_edge(Edge, Query) ->
    case maps:get(type, Query, undefined) of
        undefined -> true;
        Type -> Edge#hyper_edge.type =:= Type
    end.

perform_traversal(StartNodeId, breadth_first, State) ->
    breadth_first_search(StartNodeId, State);
perform_traversal(StartNodeId, depth_first, State) ->
    depth_first_search(StartNodeId, State);
perform_traversal(StartNodeId, hyperedge, State) ->
    hyperedge_traversal(StartNodeId, State).

breadth_first_search(StartNodeId, State) ->
    bfs_helper([StartNodeId], [], sets:new(), State).

bfs_helper([], Visited, _VisitedSet, _State) ->
    lists:reverse(Visited);
bfs_helper([NodeId | Queue], Visited, VisitedSet, State) ->
    case sets:is_element(NodeId, VisitedSet) of
        true ->
            bfs_helper(Queue, Visited, VisitedSet, State);
        false ->
            Neighbors = get_node_neighbors(NodeId, State),
            NewQueue = Queue ++ Neighbors,
            NewVisited = [NodeId | Visited],
            NewVisitedSet = sets:add_element(NodeId, VisitedSet),
            bfs_helper(NewQueue, NewVisited, NewVisitedSet, State)
    end.

depth_first_search(StartNodeId, State) ->
    dfs_helper(StartNodeId, [], sets:new(), State).

dfs_helper(NodeId, Visited, VisitedSet, State) ->
    case sets:is_element(NodeId, VisitedSet) of
        true ->
            Visited;
        false ->
            NewVisited = [NodeId | Visited],
            NewVisitedSet = sets:add_element(NodeId, VisitedSet),
            Neighbors = get_node_neighbors(NodeId, State),
            lists:foldl(fun(Neighbor, Acc) ->
                dfs_helper(Neighbor, Acc, NewVisitedSet, State)
            end, NewVisited, Neighbors)
    end.

hyperedge_traversal(StartNodeId, State) ->
    ConnectedHyperedges = find_connected_hyperedges(StartNodeId, State#state.hyperedges),
    ConnectedNodes = lists:foldl(fun(Hyperedge, Acc) ->
        Hyperedge#hyper_hyperedge.node_ids ++ Acc
    end, [], ConnectedHyperedges),
    lists:usort([StartNodeId | ConnectedNodes]).

get_node_neighbors(NodeId, State) ->
    OutgoingEdges = maps:fold(fun(_EdgeId, Edge, Acc) ->
        case Edge#hyper_edge.source_id of
            NodeId -> [Edge#hyper_edge.target_id | Acc];
            _ -> Acc
        end
    end, [], State#state.edges),
    
    IncomingEdges = maps:fold(fun(_EdgeId, Edge, Acc) ->
        case Edge#hyper_edge.target_id of
            NodeId -> [Edge#hyper_edge.source_id | Acc];
            _ -> Acc
        end
    end, [], State#state.edges),
    
    lists:usort(OutgoingEdges ++ IncomingEdges).

find_connected_hyperedges(NodeId, Hyperedges) ->
    maps:fold(fun(_HyperedgeId, Hyperedge, Acc) ->
        case lists:member(NodeId, Hyperedge#hyper_hyperedge.node_ids) of
            true -> [Hyperedge | Acc];
            false -> Acc
        end
    end, [], Hyperedges).

infer_symbol_type(Content) when is_binary(Content) ->
    <<"text">>;
infer_symbol_type(Content) when is_number(Content) ->
    <<"numeric">>;
infer_symbol_type(Content) when is_list(Content) ->
    <<"list">>;
infer_symbol_type(Content) when is_map(Content) ->
    <<"object">>;
infer_symbol_type(_) ->
    <<"unknown">>.

perform_vector_search(QueryVector, TopK, Embeddings) ->
    Similarities = maps:fold(fun(Id, Vector, Acc) ->
        Similarity = cosine_similarity(QueryVector, Vector),
        [{Id, Similarity} | Acc]
    end, [], Embeddings),
    
    SortedSimilarities = lists:sort(fun({_, Sim1}, {_, Sim2}) ->
        Sim1 >= Sim2
    end, Similarities),
    
    lists:sublist(SortedSimilarities, TopK).

cosine_similarity(Vector1, Vector2) when length(Vector1) =:= length(Vector2) ->
    DotProduct = lists:sum([X * Y || {X, Y} <- lists:zip(Vector1, Vector2)]),
    Magnitude1 = math:sqrt(lists:sum([X * X || X <- Vector1])),
    Magnitude2 = math:sqrt(lists:sum([X * X || X <- Vector2])),
    
    case {Magnitude1, Magnitude2} of
        {0.0, _} -> 0.0;
        {_, 0.0} -> 0.0;
        _ -> DotProduct / (Magnitude1 * Magnitude2)
    end;
cosine_similarity(_, _) ->
    0.0.

perform_semantic_query(Query, State) ->
    TextNodes = find_text_nodes(State#state.nodes),
    ScoredNodes = score_nodes_for_query(Query, TextNodes),
    lists:sublist(ScoredNodes, 10).

find_text_nodes(Nodes) ->
    maps:fold(fun(_Id, Node, Acc) ->
        case has_text_content(Node) of
            true -> [Node | Acc];
            false -> Acc
        end
    end, [], Nodes).

has_text_content(Node) ->
    Properties = Node#hyper_node.properties,
    maps:fold(fun(_Key, Value, Acc) ->
        Acc orelse is_binary(Value)
    end, false, Properties).

score_nodes_for_query(Query, Nodes) ->
    QueryTerms = binary:split(Query, <<" ">>, [global]),
    ScoredNodes = lists:map(fun(Node) ->
        Score = calculate_node_score(Node, QueryTerms),
        {node_to_map(Node), Score}
    end, Nodes),
    
    lists:sort(fun({_, Score1}, {_, Score2}) ->
        Score1 >= Score2
    end, ScoredNodes).

calculate_node_score(Node, QueryTerms) ->
    Properties = Node#hyper_node.properties,
    TotalScore = maps:fold(fun(_Key, Value, Acc) ->
        case is_binary(Value) of
            true ->
                TermScore = lists:sum([term_frequency(Term, Value) || Term <- QueryTerms]),
                Acc + TermScore;
            false ->
                Acc
        end
    end, 0, Properties),
    TotalScore.

term_frequency(Term, Text) ->
    LowerTerm = string:lowercase(Term),
    LowerText = string:lowercase(Text),
    case binary:match(LowerText, LowerTerm) of
        nomatch -> 0;
        _ -> 1
    end.

compute_content_embeddings(Content, <<"text-embedding-3-small">>) ->
    mock_embedding_vector(384);
compute_content_embeddings(Content, <<"text-embedding-3-large">>) ->
    mock_embedding_vector(3072);
compute_content_embeddings(Content, _Model) ->
    mock_embedding_vector(768).

mock_embedding_vector(Dimensions) ->
    [rand:uniform() - 0.5 || _ <- lists:seq(1, Dimensions)].

load_hypergraph(StoragePath) ->
    case file:read_file(StoragePath) of
        {ok, Data} ->
            try
                DecodedData = jsx:decode(Data, [return_maps]),
                #state{
                    nodes = load_entities(maps:get(<<"nodes">>, DecodedData, #{}), fun map_to_node/1),
                    edges = load_entities(maps:get(<<"edges">>, DecodedData, #{}), fun map_to_edge/1),
                    hyperedges = load_entities(maps:get(<<"hyperedges">>, DecodedData, #{}), fun map_to_hyperedge/1),
                    symbols = load_entities(maps:get(<<"symbols">>, DecodedData, #{}), fun map_to_symbol/1),
                    embeddings = maps:get(<<"embeddings">>, DecodedData, #{}),
                    indices = maps:get(<<"indices">>, DecodedData, #{}),
                    storage_path = StoragePath
                }
            catch
                _:_ ->
                    ?LOG_WARNING("Failed to load hypergraph from ~p, starting with empty state", [StoragePath]),
                    #state{storage_path = StoragePath}
            end;
        {error, enoent} ->
            ?LOG_INFO("Hypergraph file ~p does not exist, starting with empty state", [StoragePath]),
            #state{storage_path = StoragePath};
        {error, Reason} ->
            ?LOG_ERROR("Failed to read hypergraph file ~p: ~p", [StoragePath, Reason]),
            #state{storage_path = StoragePath}
    end.

load_entities(EntitiesMap, ConvertFun) ->
    maps:fold(fun(Id, EntityData, Acc) ->
        Entity = ConvertFun(EntityData),
        maps:put(Id, Entity, Acc)
    end, #{}, EntitiesMap).

save_hypergraph(State) ->
    HypergraphData = #{
        <<"nodes">> => maps:fold(fun(Id, Node, Acc) ->
            maps:put(Id, node_to_map(Node), Acc)
        end, #{}, State#state.nodes),
        <<"edges">> => maps:fold(fun(Id, Edge, Acc) ->
            maps:put(Id, edge_to_map(Edge), Acc)
        end, #{}, State#state.edges),
        <<"hyperedges">> => maps:fold(fun(Id, Hyperedge, Acc) ->
            maps:put(Id, hyperedge_to_map(Hyperedge), Acc)
        end, #{}, State#state.hyperedges),
        <<"symbols">> => maps:fold(fun(Id, Symbol, Acc) ->
            maps:put(Id, symbol_to_map(Symbol), Acc)
        end, #{}, State#state.symbols),
        <<"embeddings">> => State#state.embeddings,
        <<"indices">> => State#state.indices
    },
    
    Data = jsx:encode(HypergraphData, [space, {indent, 2}]),
    
    ok = filelib:ensure_dir(State#state.storage_path),
    case file:write_file(State#state.storage_path, Data) of
        ok ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR("Failed to save hypergraph to ~p: ~p", [State#state.storage_path, Reason]),
            error
    end.

map_to_node(NodeData) ->
    #hyper_node{
        id = maps:get(id, NodeData),
        type = maps:get(type, NodeData),
        properties = maps:get(properties, NodeData, #{}),
        metadata = maps:get(metadata, NodeData, #{}),
        created_at = maps:get(created_at, NodeData, 0),
        updated_at = maps:get(updated_at, NodeData, 0)
    }.

map_to_edge(EdgeData) ->
    #hyper_edge{
        id = maps:get(id, EdgeData),
        source_id = maps:get(source_id, EdgeData),
        target_id = maps:get(target_id, EdgeData),
        type = maps:get(type, EdgeData),
        properties = maps:get(properties, EdgeData, #{}),
        weight = maps:get(weight, EdgeData, 1.0),
        created_at = maps:get(created_at, EdgeData, 0)
    }.

map_to_hyperedge(HyperedgeData) ->
    #hyper_hyperedge{
        id = maps:get(id, HyperedgeData),
        node_ids = maps:get(node_ids, HyperedgeData, []),
        type = maps:get(type, HyperedgeData),
        properties = maps:get(properties, HyperedgeData, #{}),
        weight = maps:get(weight, HyperedgeData, 1.0),
        created_at = maps:get(created_at, HyperedgeData, 0)
    }.

map_to_symbol(SymbolData) ->
    #hyper_symbol{
        id = maps:get(id, SymbolData),
        name = maps:get(name, SymbolData),
        symbol_type = maps:get(symbol_type, SymbolData),
        content = maps:get(content, SymbolData),
        embeddings = maps:get(embeddings, SymbolData, #{}),
        linked_symbols = maps:get(linked_symbols, SymbolData, []),
        metadata = maps:get(metadata, SymbolData, #{}),
        created_at = maps:get(created_at, SymbolData, 0),
        updated_at = maps:get(updated_at, SymbolData, 0)
    }.