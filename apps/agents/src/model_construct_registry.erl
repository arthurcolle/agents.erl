%% model_construct_registry.erl
%% Registry for different model constructs (Tool, Memory, File, Conversation, Message, etc.)
-module(model_construct_registry).
-behaviour(gen_server).

-export([
    start_link/0,
    register_tool/3,
    register_memory/3,
    register_file/3,
    register_conversation/3,
    register_message/3,
    register_custom_construct/4,
    get_construct/2,
    query_constructs/2,
    list_constructs_by_type/1,
    list_constructs_by_owner/1,
    discover_constructs/2,
    update_construct/3,
    delete_construct/2,
    get_construct_stats/0
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

-record(tool_construct, {
    id :: binary(),
    name :: binary(),
    description :: binary(),
    schema :: map(),
    implementation :: binary() | function(),
    owner_agent :: binary(),
    access_level :: atom(), % public | private | shared
    tags :: [binary()],
    version :: binary(),
    dependencies :: [binary()],
    performance_metrics :: map(),
    created_at :: integer(),
    last_used :: integer()
}).

-record(memory_construct, {
    id :: binary(),
    type :: atom(), % episodic | semantic | working | procedural
    content :: term(),
    owner_agent :: binary(),
    access_level :: atom(),
    retention_policy :: map(), % ttl, max_size, etc.
    associations :: [binary()], % related memory IDs
    confidence :: float(),
    tags :: [binary()],
    created_at :: integer(),
    last_accessed :: integer()
}).

-record(file_construct, {
    id :: binary(),
    path :: binary(),
    type :: binary(), % mime type
    size :: integer(),
    owner_agent :: binary(),
    access_level :: atom(),
    permissions :: map(),
    content_hash :: binary(),
    metadata :: map(),
    tags :: [binary()],
    created_at :: integer(),
    last_modified :: integer()
}).

-record(conversation_construct, {
    id :: binary(),
    participants :: [binary()], % agent IDs
    topic :: binary(),
    owner_agent :: binary(),
    access_level :: atom(),
    message_count :: integer(),
    last_activity :: integer(),
    tags :: [binary()],
    metadata :: map(),
    created_at :: integer()
}).

-record(message_construct, {
    id :: binary(),
    conversation_id :: binary(),
    sender_agent :: binary(),
    recipient_agents :: [binary()],
    content :: binary(),
    message_type :: atom(), % text | function_call | system | error
    metadata :: map(),
    reply_to :: binary() | undefined,
    timestamp :: integer(),
    status :: atom() % sent | delivered | read | processed
}).

-record(custom_construct, {
    id :: binary(),
    type :: atom(),
    data :: term(),
    owner_agent :: binary(),
    access_level :: atom(),
    schema :: map(),
    metadata :: map(),
    tags :: [binary()],
    created_at :: integer(),
    last_modified :: integer()
}).

-record(state, {
    tools = #{} :: #{binary() => #tool_construct{}},
    memories = #{} :: #{binary() => #memory_construct{}},
    files = #{} :: #{binary() => #file_construct{}},
    conversations = #{} :: #{binary() => #conversation_construct{}},
    messages = #{} :: #{binary() => #message_construct{}},
    custom_constructs = #{} :: #{binary() => #custom_construct{}},
    indexes = #{} :: map(), % type => owner => [construct_ids]
    search_index = #{} :: map(), % tag/keyword => [construct_ids]
    stats = #{} :: map()
}).

%% API Functions
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Register different types of constructs
register_tool(ToolId, ToolInfo, OwnerAgent) ->
    gen_server:call(?SERVER, {register_tool, ToolId, ToolInfo, OwnerAgent}).

register_memory(MemoryId, MemoryInfo, OwnerAgent) ->
    gen_server:call(?SERVER, {register_memory, MemoryId, MemoryInfo, OwnerAgent}).

register_file(FileId, FileInfo, OwnerAgent) ->
    gen_server:call(?SERVER, {register_file, FileId, FileInfo, OwnerAgent}).

register_conversation(ConversationId, ConversationInfo, OwnerAgent) ->
    gen_server:call(?SERVER, {register_conversation, ConversationId, ConversationInfo, OwnerAgent}).

register_message(MessageId, MessageInfo, OwnerAgent) ->
    gen_server:call(?SERVER, {register_message, MessageId, MessageInfo, OwnerAgent}).

register_custom_construct(ConstructId, ConstructType, ConstructInfo, OwnerAgent) ->
    gen_server:call(?SERVER, {register_custom_construct, ConstructId, ConstructType, ConstructInfo, OwnerAgent}).

%% Retrieve constructs
get_construct(ConstructType, ConstructId) ->
    gen_server:call(?SERVER, {get_construct, ConstructType, ConstructId}).

%% Query constructs with filters
query_constructs(ConstructType, Filters) ->
    gen_server:call(?SERVER, {query_constructs, ConstructType, Filters}).

%% List constructs by type
list_constructs_by_type(ConstructType) ->
    gen_server:call(?SERVER, {list_constructs_by_type, ConstructType}).

%% List constructs by owner
list_constructs_by_owner(OwnerAgent) ->
    gen_server:call(?SERVER, {list_constructs_by_owner, OwnerAgent}).

%% Discover constructs across the system
discover_constructs(Query, Options) ->
    gen_server:call(?SERVER, {discover_constructs, Query, Options}).

%% Update construct
update_construct(ConstructType, ConstructId, Updates) ->
    gen_server:call(?SERVER, {update_construct, ConstructType, ConstructId, Updates}).

%% Delete construct
delete_construct(ConstructType, ConstructId) ->
    gen_server:call(?SERVER, {delete_construct, ConstructType, ConstructId}).

%% Get registry statistics
get_construct_stats() ->
    gen_server:call(?SERVER, get_construct_stats).

%% gen_server callbacks
init([]) ->
    % Initialize search indexes
    Indexes = #{
        tools => #{}, memories => #{}, files => #{}, 
        conversations => #{}, messages => #{}, custom => #{}
    },
    
    Stats = #{
        total_constructs => 0,
        constructs_by_type => #{},
        constructs_by_owner => #{},
        last_updated => erlang:system_time(millisecond)
    },
    
    {ok, #state{indexes = Indexes, stats = Stats}}.

handle_call({register_tool, ToolId, ToolInfo, OwnerAgent}, _From, State) ->
    ToolConstruct = create_tool_construct(ToolId, ToolInfo, OwnerAgent),
    NewTools = maps:put(ToolId, ToolConstruct, State#state.tools),
    NewState = update_indexes_and_stats(tool, ToolId, ToolConstruct, State#state{tools = NewTools}),
    {reply, ok, NewState};

handle_call({register_memory, MemoryId, MemoryInfo, OwnerAgent}, _From, State) ->
    MemoryConstruct = create_memory_construct(MemoryId, MemoryInfo, OwnerAgent),
    NewMemories = maps:put(MemoryId, MemoryConstruct, State#state.memories),
    NewState = update_indexes_and_stats(memory, MemoryId, MemoryConstruct, State#state{memories = NewMemories}),
    {reply, ok, NewState};

handle_call({register_file, FileId, FileInfo, OwnerAgent}, _From, State) ->
    FileConstruct = create_file_construct(FileId, FileInfo, OwnerAgent),
    NewFiles = maps:put(FileId, FileConstruct, State#state.files),
    NewState = update_indexes_and_stats(file, FileId, FileConstruct, State#state{files = NewFiles}),
    {reply, ok, NewState};

handle_call({register_conversation, ConversationId, ConversationInfo, OwnerAgent}, _From, State) ->
    ConversationConstruct = create_conversation_construct(ConversationId, ConversationInfo, OwnerAgent),
    NewConversations = maps:put(ConversationId, ConversationConstruct, State#state.conversations),
    NewState = update_indexes_and_stats(conversation, ConversationId, ConversationConstruct, State#state{conversations = NewConversations}),
    {reply, ok, NewState};

handle_call({register_message, MessageId, MessageInfo, OwnerAgent}, _From, State) ->
    MessageConstruct = create_message_construct(MessageId, MessageInfo, OwnerAgent),
    NewMessages = maps:put(MessageId, MessageConstruct, State#state.messages),
    NewState = update_indexes_and_stats(message, MessageId, MessageConstruct, State#state{messages = NewMessages}),
    {reply, ok, NewState};

handle_call({register_custom_construct, ConstructId, ConstructType, ConstructInfo, OwnerAgent}, _From, State) ->
    CustomConstruct = create_custom_construct(ConstructId, ConstructType, ConstructInfo, OwnerAgent),
    NewCustom = maps:put(ConstructId, CustomConstruct, State#state.custom_constructs),
    NewState = update_indexes_and_stats(custom, ConstructId, CustomConstruct, State#state{custom_constructs = NewCustom}),
    {reply, ok, NewState};

handle_call({get_construct, ConstructType, ConstructId}, _From, State) ->
    Result = get_construct_internal(ConstructType, ConstructId, State),
    {reply, Result, State};

handle_call({query_constructs, ConstructType, Filters}, _From, State) ->
    Results = query_constructs_internal(ConstructType, Filters, State),
    {reply, Results, State};

handle_call({list_constructs_by_type, ConstructType}, _From, State) ->
    Results = list_constructs_by_type_internal(ConstructType, State),
    {reply, Results, State};

handle_call({list_constructs_by_owner, OwnerAgent}, _From, State) ->
    Results = list_constructs_by_owner_internal(OwnerAgent, State),
    {reply, Results, State};

handle_call({discover_constructs, Query, Options}, _From, State) ->
    Results = discover_constructs_internal(Query, Options, State),
    {reply, Results, State};

handle_call({update_construct, ConstructType, ConstructId, Updates}, _From, State) ->
    {Result, NewState} = update_construct_internal(ConstructType, ConstructId, Updates, State),
    {reply, Result, NewState};

handle_call({delete_construct, ConstructType, ConstructId}, _From, State) ->
    {Result, NewState} = delete_construct_internal(ConstructType, ConstructId, State),
    {reply, Result, NewState};

handle_call(get_construct_stats, _From, State) ->
    {reply, State#state.stats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal Functions

create_tool_construct(ToolId, ToolInfo, OwnerAgent) ->
    Now = erlang:system_time(millisecond),
    #tool_construct{
        id = ToolId,
        name = maps:get(name, ToolInfo, ToolId),
        description = maps:get(description, ToolInfo, <<"">>),
        schema = maps:get(schema, ToolInfo, #{}),
        implementation = maps:get(implementation, ToolInfo, <<"">>),
        owner_agent = OwnerAgent,
        access_level = maps:get(access_level, ToolInfo, public),
        tags = maps:get(tags, ToolInfo, []),
        version = maps:get(version, ToolInfo, <<"1.0.0">>),
        dependencies = maps:get(dependencies, ToolInfo, []),
        performance_metrics = maps:get(performance_metrics, ToolInfo, #{}),
        created_at = Now,
        last_used = Now
    }.

create_memory_construct(MemoryId, MemoryInfo, OwnerAgent) ->
    Now = erlang:system_time(millisecond),
    #memory_construct{
        id = MemoryId,
        type = maps:get(type, MemoryInfo, semantic),
        content = maps:get(content, MemoryInfo, #{}),
        owner_agent = OwnerAgent,
        access_level = maps:get(access_level, MemoryInfo, private),
        retention_policy = maps:get(retention_policy, MemoryInfo, #{}),
        associations = maps:get(associations, MemoryInfo, []),
        confidence = maps:get(confidence, MemoryInfo, 1.0),
        tags = maps:get(tags, MemoryInfo, []),
        created_at = Now,
        last_accessed = Now
    }.

create_file_construct(FileId, FileInfo, OwnerAgent) ->
    Now = erlang:system_time(millisecond),
    #file_construct{
        id = FileId,
        path = maps:get(path, FileInfo, <<"">>),
        type = maps:get(type, FileInfo, <<"application/octet-stream">>),
        size = maps:get(size, FileInfo, 0),
        owner_agent = OwnerAgent,
        access_level = maps:get(access_level, FileInfo, private),
        permissions = maps:get(permissions, FileInfo, #{}),
        content_hash = maps:get(content_hash, FileInfo, <<"">>),
        metadata = maps:get(metadata, FileInfo, #{}),
        tags = maps:get(tags, FileInfo, []),
        created_at = Now,
        last_modified = Now
    }.

create_conversation_construct(ConversationId, ConversationInfo, OwnerAgent) ->
    Now = erlang:system_time(millisecond),
    #conversation_construct{
        id = ConversationId,
        participants = maps:get(participants, ConversationInfo, [OwnerAgent]),
        topic = maps:get(topic, ConversationInfo, <<"">>),
        owner_agent = OwnerAgent,
        access_level = maps:get(access_level, ConversationInfo, shared),
        message_count = maps:get(message_count, ConversationInfo, 0),
        last_activity = Now,
        tags = maps:get(tags, ConversationInfo, []),
        metadata = maps:get(metadata, ConversationInfo, #{}),
        created_at = Now
    }.

create_message_construct(MessageId, MessageInfo, OwnerAgent) ->
    Now = erlang:system_time(millisecond),
    #message_construct{
        id = MessageId,
        conversation_id = maps:get(conversation_id, MessageInfo, <<"">>),
        sender_agent = maps:get(sender_agent, MessageInfo, OwnerAgent),
        recipient_agents = maps:get(recipient_agents, MessageInfo, []),
        content = maps:get(content, MessageInfo, <<"">>),
        message_type = maps:get(message_type, MessageInfo, text),
        metadata = maps:get(metadata, MessageInfo, #{}),
        reply_to = maps:get(reply_to, MessageInfo, undefined),
        timestamp = Now,
        status = maps:get(status, MessageInfo, sent)
    }.

create_custom_construct(ConstructId, ConstructType, ConstructInfo, OwnerAgent) ->
    Now = erlang:system_time(millisecond),
    #custom_construct{
        id = ConstructId,
        type = ConstructType,
        data = maps:get(data, ConstructInfo, #{}),
        owner_agent = OwnerAgent,
        access_level = maps:get(access_level, ConstructInfo, private),
        schema = maps:get(schema, ConstructInfo, #{}),
        metadata = maps:get(metadata, ConstructInfo, #{}),
        tags = maps:get(tags, ConstructInfo, []),
        created_at = Now,
        last_modified = Now
    }.

get_construct_internal(ConstructType, ConstructId, State) ->
    case ConstructType of
        tool -> maps:get(ConstructId, State#state.tools, not_found);
        memory -> maps:get(ConstructId, State#state.memories, not_found);
        file -> maps:get(ConstructId, State#state.files, not_found);
        conversation -> maps:get(ConstructId, State#state.conversations, not_found);
        message -> maps:get(ConstructId, State#state.messages, not_found);
        custom -> maps:get(ConstructId, State#state.custom_constructs, not_found);
        _ -> {error, unknown_construct_type}
    end.

query_constructs_internal(ConstructType, Filters, State) ->
    ConstructMap = get_construct_map(ConstructType, State),
    FilteredResults = maps:fold(fun(ConstructId, Construct, Acc) ->
        case apply_construct_filters(Construct, Filters) of
            true -> [format_construct_result(ConstructType, ConstructId, Construct) | Acc];
            false -> Acc
        end
    end, [], ConstructMap),
    {ok, FilteredResults}.

list_constructs_by_type_internal(ConstructType, State) ->
    ConstructMap = get_construct_map(ConstructType, State),
    Results = maps:fold(fun(ConstructId, Construct, Acc) ->
        [format_construct_result(ConstructType, ConstructId, Construct) | Acc]
    end, [], ConstructMap),
    {ok, Results}.

list_constructs_by_owner_internal(OwnerAgent, State) ->
    % Collect constructs from all types owned by the agent
    AllResults = lists:flatten([
        collect_constructs_by_owner(tool, OwnerAgent, State),
        collect_constructs_by_owner(memory, OwnerAgent, State),
        collect_constructs_by_owner(file, OwnerAgent, State),
        collect_constructs_by_owner(conversation, OwnerAgent, State),
        collect_constructs_by_owner(message, OwnerAgent, State),
        collect_constructs_by_owner(custom, OwnerAgent, State)
    ]),
    {ok, AllResults}.

discover_constructs_internal(Query, Options, State) ->
    % Search across all construct types
    AllResults = lists:flatten([
        search_constructs_by_query(tool, Query, State),
        search_constructs_by_query(memory, Query, State),
        search_constructs_by_query(file, Query, State),
        search_constructs_by_query(conversation, Query, State),
        search_constructs_by_query(message, Query, State),
        search_constructs_by_query(custom, Query, State)
    ]),
    
    % Apply options (sorting, limiting, etc.)
    FinalResults = apply_discovery_options(AllResults, Options),
    {ok, FinalResults}.

update_construct_internal(ConstructType, ConstructId, Updates, State) ->
    case get_construct_internal(ConstructType, ConstructId, State) of
        not_found -> {{error, not_found}, State};
        {error, Reason} -> {{error, Reason}, State};
        Construct ->
            UpdatedConstruct = apply_construct_updates(Construct, Updates),
            NewState = update_construct_in_state(ConstructType, ConstructId, UpdatedConstruct, State),
            {ok, NewState}
    end.

delete_construct_internal(ConstructType, ConstructId, State) ->
    case get_construct_internal(ConstructType, ConstructId, State) of
        not_found -> {{error, not_found}, State};
        {error, Reason} -> {{error, Reason}, State};
        _Construct ->
            NewState = remove_construct_from_state(ConstructType, ConstructId, State),
            {ok, NewState}
    end.

get_construct_map(ConstructType, State) ->
    case ConstructType of
        tool -> State#state.tools;
        memory -> State#state.memories;
        file -> State#state.files;
        conversation -> State#state.conversations;
        message -> State#state.messages;
        custom -> State#state.custom_constructs;
        _ -> #{}
    end.

collect_constructs_by_owner(ConstructType, OwnerAgent, State) ->
    ConstructMap = get_construct_map(ConstructType, State),
    maps:fold(fun(ConstructId, Construct, Acc) ->
        case get_construct_owner(Construct) of
            OwnerAgent -> [format_construct_result(ConstructType, ConstructId, Construct) | Acc];
            _ -> Acc
        end
    end, [], ConstructMap).

get_construct_owner(#tool_construct{owner_agent = Owner}) -> Owner;
get_construct_owner(#memory_construct{owner_agent = Owner}) -> Owner;
get_construct_owner(#file_construct{owner_agent = Owner}) -> Owner;
get_construct_owner(#conversation_construct{owner_agent = Owner}) -> Owner;
get_construct_owner(#message_construct{sender_agent = Owner}) -> Owner;
get_construct_owner(#custom_construct{owner_agent = Owner}) -> Owner.

format_construct_result(ConstructType, ConstructId, Construct) ->
    BaseInfo = #{
        type => ConstructType,
        id => ConstructId,
        owner => get_construct_owner(Construct)
    },
    
    % Add type-specific information
    case ConstructType of
        tool -> BaseInfo#{
            name => Construct#tool_construct.name,
            description => Construct#tool_construct.description,
            tags => Construct#tool_construct.tags,
            version => Construct#tool_construct.version
        };
        memory -> BaseInfo#{
            memory_type => Construct#memory_construct.type,
            confidence => Construct#memory_construct.confidence,
            tags => Construct#memory_construct.tags
        };
        file -> BaseInfo#{
            path => Construct#file_construct.path,
            file_type => Construct#file_construct.type,
            size => Construct#file_construct.size,
            tags => Construct#file_construct.tags
        };
        conversation -> BaseInfo#{
            topic => Construct#conversation_construct.topic,
            participants => Construct#conversation_construct.participants,
            message_count => Construct#conversation_construct.message_count,
            tags => Construct#conversation_construct.tags
        };
        message -> BaseInfo#{
            conversation_id => Construct#message_construct.conversation_id,
            message_type => Construct#message_construct.message_type,
            timestamp => Construct#message_construct.timestamp,
            status => Construct#message_construct.status
        };
        custom -> BaseInfo#{
            custom_type => Construct#custom_construct.type,
            tags => Construct#custom_construct.tags
        }
    end.

apply_construct_filters(_Construct, _Filters) ->
    % Placeholder for filter implementation
    true.

search_constructs_by_query(_ConstructType, _Query, _State) ->
    % Placeholder for query implementation
    [].

apply_discovery_options(Results, Options) ->
    % Apply sorting and limiting
    SortedResults = case maps:get(sort_by, Options, undefined) of
        undefined -> Results;
        created_at -> lists:sort(fun(A, B) -> 
            maps:get(created_at, A, 0) =< maps:get(created_at, B, 0) 
        end, Results);
        _ -> Results
    end,
    
    case maps:get(limit, Options, undefined) of
        undefined -> SortedResults;
        Limit -> lists:sublist(SortedResults, Limit)
    end.

apply_construct_updates(Construct, _Updates) ->
    % Placeholder for update implementation
    Construct.

update_construct_in_state(ConstructType, ConstructId, UpdatedConstruct, State) ->
    case ConstructType of
        tool -> State#state{tools = maps:put(ConstructId, UpdatedConstruct, State#state.tools)};
        memory -> State#state{memories = maps:put(ConstructId, UpdatedConstruct, State#state.memories)};
        file -> State#state{files = maps:put(ConstructId, UpdatedConstruct, State#state.files)};
        conversation -> State#state{conversations = maps:put(ConstructId, UpdatedConstruct, State#state.conversations)};
        message -> State#state{messages = maps:put(ConstructId, UpdatedConstruct, State#state.messages)};
        custom -> State#state{custom_constructs = maps:put(ConstructId, UpdatedConstruct, State#state.custom_constructs)}
    end.

remove_construct_from_state(ConstructType, ConstructId, State) ->
    case ConstructType of
        tool -> State#state{tools = maps:remove(ConstructId, State#state.tools)};
        memory -> State#state{memories = maps:remove(ConstructId, State#state.memories)};
        file -> State#state{files = maps:remove(ConstructId, State#state.files)};
        conversation -> State#state{conversations = maps:remove(ConstructId, State#state.conversations)};
        message -> State#state{messages = maps:remove(ConstructId, State#state.messages)};
        custom -> State#state{custom_constructs = maps:remove(ConstructId, State#state.custom_constructs)}
    end.

update_indexes_and_stats(ConstructType, ConstructId, Construct, State) ->
    % Update indexes
    OwnerAgent = get_construct_owner(Construct),
    TypeIndexes = maps:get(ConstructType, State#state.indexes, #{}),
    OwnerConstructs = maps:get(OwnerAgent, TypeIndexes, []),
    NewOwnerConstructs = [ConstructId | lists:delete(ConstructId, OwnerConstructs)],
    NewTypeIndexes = maps:put(OwnerAgent, NewOwnerConstructs, TypeIndexes),
    NewIndexes = maps:put(ConstructType, NewTypeIndexes, State#state.indexes),
    
    % Update stats
    Stats = State#state.stats,
    NewStats = Stats#{
        total_constructs => maps:get(total_constructs, Stats, 0) + 1,
        last_updated => erlang:system_time(millisecond)
    },
    
    State#state{indexes = NewIndexes, stats = NewStats}.