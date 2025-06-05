%% universal_tool_registry.erl
%% Universal Tool Registry - Central registry for all tools with dynamic registration
-module(universal_tool_registry).
-behaviour(gen_server).

-export([
    start_link/0,
    register_tool/2,
    register_tool_with_code/3,
    register_tool_with_url/3,
    register_tool_with_ast/3,
    unregister_tool/1,
    get_tool/1,
    list_tools/0,
    list_tools_by_source/1,
    search_tools/1,
    get_tool_metadata/1,
    update_tool_code/2,
    get_tools_for_agent/1,
    register_agent_tool/3,
    sync_from_sources/0
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

-record(tool_entry, {
    name :: binary(),
    description :: binary(),
    schema :: map(),
    source :: atom(), % local | mcp | agent | external
    code :: binary() | undefined,
    ast :: term() | undefined,
    url :: binary() | undefined,
    metadata :: map(),
    created_at :: integer(),
    updated_at :: integer(),
    version :: binary(),
    tags :: [binary()],
    dependencies :: [binary()],
    executor :: function() | undefined
}).

-record(state, {
    tools = #{} :: #{binary() => #tool_entry{}},
    source_configs = #{} :: map(),
    agent_tools = #{} :: #{binary() => [binary()]}, % agent_id -> [tool_names]
    tool_index = #{} :: #{binary() => [binary()]}, % tag/keyword -> [tool_names]
    sync_interval = 60000 :: integer() % 1 minute default
}).

%% API Functions
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Register a tool with basic schema
register_tool(Name, Schema) when is_binary(Name), is_map(Schema) ->
    gen_server:call(?SERVER, {register_tool, Name, Schema, local, #{}}).

%% Register a tool with executable code
register_tool_with_code(Name, Schema, Code) when is_binary(Name), is_map(Schema), is_binary(Code) ->
    Metadata = #{code_type => erlang, execution_model => local},
    gen_server:call(?SERVER, {register_tool_with_code, Name, Schema, Code, Metadata}).

%% Register a tool with URL endpoint
register_tool_with_url(Name, Schema, Url) when is_binary(Name), is_map(Schema), is_binary(Url) ->
    Metadata = #{endpoint_type => http, url => Url, execution_model => remote},
    gen_server:call(?SERVER, {register_tool_with_url, Name, Schema, Url, Metadata}).

%% Register a tool with AST representation
register_tool_with_ast(Name, Schema, AST) when is_binary(Name), is_map(Schema) ->
    Metadata = #{ast_type => erlang, execution_model => compiled},
    gen_server:call(?SERVER, {register_tool_with_ast, Name, Schema, AST, Metadata}).

%% Unregister a tool
unregister_tool(Name) when is_binary(Name) ->
    gen_server:call(?SERVER, {unregister_tool, Name}).

%% Get a specific tool
get_tool(Name) when is_binary(Name) ->
    gen_server:call(?SERVER, {get_tool, Name}).

%% List all tools
list_tools() ->
    gen_server:call(?SERVER, list_tools).

%% List tools by source
list_tools_by_source(Source) when is_atom(Source) ->
    gen_server:call(?SERVER, {list_tools_by_source, Source}).

%% Search tools by keywords/tags
search_tools(Query) when is_binary(Query) ->
    gen_server:call(?SERVER, {search_tools, Query}).

%% Get tool metadata
get_tool_metadata(Name) when is_binary(Name) ->
    gen_server:call(?SERVER, {get_tool_metadata, Name}).

%% Update tool code
update_tool_code(Name, NewCode) when is_binary(Name), is_binary(NewCode) ->
    gen_server:call(?SERVER, {update_tool_code, Name, NewCode}).

%% Get tools for a specific agent
get_tools_for_agent(AgentId) when is_binary(AgentId) ->
    gen_server:call(?SERVER, {get_tools_for_agent, AgentId}).

%% Register a tool for an agent
register_agent_tool(AgentId, ToolName, Schema) when is_binary(AgentId), is_binary(ToolName), is_map(Schema) ->
    gen_server:call(?SERVER, {register_agent_tool, AgentId, ToolName, Schema}).

%% Sync tools from all configured sources
sync_from_sources() ->
    gen_server:call(?SERVER, sync_from_sources, 30000).

%% gen_server callbacks
init([]) ->
    % Schedule periodic sync
    timer:send_interval(60000, sync_sources),
    
    % Initialize with default tools from existing agent_tools
    InitialState = #state{},
    {ok, sync_builtin_tools(InitialState)}.

handle_call({register_tool, Name, Schema, Source, Metadata}, _From, State) ->
    ToolEntry = create_tool_entry(Name, Schema, Source, Metadata),
    NewTools = maps:put(Name, ToolEntry, State#state.tools),
    NewIndex = update_tool_index(Name, ToolEntry, State#state.tool_index),
    {reply, ok, State#state{tools = NewTools, tool_index = NewIndex}};

handle_call({register_tool_with_code, Name, Schema, Code, Metadata}, _From, State) ->
    ToolEntry = create_tool_entry(Name, Schema, local, Metadata#{ code => Code }),
    UpdatedEntry = ToolEntry#tool_entry{code = Code},
    NewTools = maps:put(Name, UpdatedEntry, State#state.tools),
    NewIndex = update_tool_index(Name, UpdatedEntry, State#state.tool_index),
    {reply, ok, State#state{tools = NewTools, tool_index = NewIndex}};

handle_call({register_tool_with_url, Name, Schema, Url, Metadata}, _From, State) ->
    ToolEntry = create_tool_entry(Name, Schema, external, Metadata#{ url => Url }),
    UpdatedEntry = ToolEntry#tool_entry{url = Url},
    NewTools = maps:put(Name, UpdatedEntry, State#state.tools),
    NewIndex = update_tool_index(Name, UpdatedEntry, State#state.tool_index),
    {reply, ok, State#state{tools = NewTools, tool_index = NewIndex}};

handle_call({register_tool_with_ast, Name, Schema, AST, Metadata}, _From, State) ->
    ToolEntry = create_tool_entry(Name, Schema, local, Metadata#{ ast => AST }),
    UpdatedEntry = ToolEntry#tool_entry{ast = AST},
    NewTools = maps:put(Name, UpdatedEntry, State#state.tools),
    NewIndex = update_tool_index(Name, UpdatedEntry, State#state.tool_index),
    {reply, ok, State#state{tools = NewTools, tool_index = NewIndex}};

handle_call({unregister_tool, Name}, _From, State) ->
    NewTools = maps:remove(Name, State#state.tools),
    NewIndex = remove_from_tool_index(Name, State#state.tool_index),
    {reply, ok, State#state{tools = NewTools, tool_index = NewIndex}};

handle_call({get_tool, Name}, _From, State) ->
    Result = maps:get(Name, State#state.tools, not_found),
    {reply, Result, State};

handle_call(list_tools, _From, State) ->
    ToolList = maps:keys(State#state.tools),
    {reply, ToolList, State};

handle_call({list_tools_by_source, Source}, _From, State) ->
    FilteredTools = maps:filter(
        fun(_, #tool_entry{source = ToolSource}) -> ToolSource =:= Source end,
        State#state.tools
    ),
    ToolList = maps:keys(FilteredTools),
    {reply, ToolList, State};

handle_call({search_tools, Query}, _From, State) ->
    Results = search_tools_internal(Query, State#state.tools, State#state.tool_index),
    {reply, Results, State};

handle_call({get_tool_metadata, Name}, _From, State) ->
    case maps:get(Name, State#state.tools, not_found) of
        not_found -> {reply, not_found, State};
        #tool_entry{} = Entry -> {reply, tool_entry_to_metadata(Entry), State}
    end;

handle_call({update_tool_code, Name, NewCode}, _From, State) ->
    case maps:get(Name, State#state.tools, not_found) of
        not_found -> 
            {reply, {error, tool_not_found}, State};
        #tool_entry{} = Entry ->
            UpdatedEntry = Entry#tool_entry{
                code = NewCode,
                updated_at = erlang:system_time(millisecond),
                version = generate_version()
            },
            NewTools = maps:put(Name, UpdatedEntry, State#state.tools),
            {reply, ok, State#state{tools = NewTools}}
    end;

handle_call({get_tools_for_agent, AgentId}, _From, State) ->
    AgentToolNames = maps:get(AgentId, State#state.agent_tools, []),
    AgentTools = maps:with(AgentToolNames, State#state.tools),
    {reply, AgentTools, State};

handle_call({register_agent_tool, AgentId, ToolName, Schema}, _From, State) ->
    % Register the tool in the universal registry
    ToolEntry = create_tool_entry(ToolName, Schema, agent, #{agent_id => AgentId}),
    NewTools = maps:put(ToolName, ToolEntry, State#state.tools),
    
    % Associate tool with agent
    AgentToolList = maps:get(AgentId, State#state.agent_tools, []),
    UpdatedAgentToolList = [ToolName | lists:delete(ToolName, AgentToolList)],
    NewAgentTools = maps:put(AgentId, UpdatedAgentToolList, State#state.agent_tools),
    
    % Update index
    NewIndex = update_tool_index(ToolName, ToolEntry, State#state.tool_index),
    
    {reply, ok, State#state{
        tools = NewTools,
        agent_tools = NewAgentTools,
        tool_index = NewIndex
    }};

handle_call(sync_from_sources, _From, State) ->
    NewState = sync_all_sources(State),
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(sync_sources, State) ->
    NewState = sync_all_sources(State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal Functions
create_tool_entry(Name, Schema, Source, Metadata) ->
    Now = erlang:system_time(millisecond),
    #tool_entry{
        name = Name,
        description = maps:get(<<"description">>, Schema, <<"">>),
        schema = Schema,
        source = Source,
        metadata = Metadata,
        created_at = Now,
        updated_at = Now,
        version = generate_version(),
        tags = extract_tags(Schema, Metadata),
        dependencies = extract_dependencies(Schema, Metadata)
    }.

generate_version() ->
    <<X:32, Y:32, Z:32>> = crypto:strong_rand_bytes(12),
    iolist_to_binary(io_lib:format("~8.16.0b-~8.16.0b-~8.16.0b", [X, Y, Z])).

extract_tags(Schema, Metadata) ->
    % Extract tags from schema and metadata
    SchemaName = maps:get(<<"name">>, Schema, <<"">>),
    MetadataTags = maps:get(tags, Metadata, []),
    
    % Auto-generate tags based on tool name and description
    AutoTags = case SchemaName of
        <<"jina_", _/binary>> -> [<<"search">>, <<"ai">>, <<"jina">>];
        <<"shell">> -> [<<"system">>, <<"command">>];
        <<"file_", _/binary>> -> [<<"filesystem">>, <<"io">>];
        <<"http_", _/binary>> -> [<<"network">>, <<"web">>];
        _ -> [<<"general">>]
    end,
    
    lists:usort(AutoTags ++ MetadataTags).

extract_dependencies(Schema, Metadata) ->
    % Extract dependencies from schema and metadata
    maps:get(dependencies, Metadata, []).

update_tool_index(ToolName, #tool_entry{tags = Tags}, Index) ->
    % Add tool to index for each tag
    lists:foldl(fun(Tag, AccIndex) ->
        ExistingTools = maps:get(Tag, AccIndex, []),
        UpdatedTools = [ToolName | lists:delete(ToolName, ExistingTools)],
        maps:put(Tag, UpdatedTools, AccIndex)
    end, Index, Tags).

remove_from_tool_index(ToolName, Index) ->
    % Remove tool from all index entries
    maps:map(fun(_, ToolList) ->
        lists:delete(ToolName, ToolList)
    end, Index).

search_tools_internal(Query, Tools, Index) ->
    QueryLower = string:lowercase(binary_to_list(Query)),
    
    % Search by tags first
    TagMatches = maps:fold(fun(Tag, ToolNames, Acc) ->
        TagStr = binary_to_list(Tag),
        case string:str(string:lowercase(TagStr), QueryLower) > 0 of
            true -> ToolNames ++ Acc;
            false -> Acc
        end
    end, [], Index),
    
    % Search by tool name and description
    NameDescMatches = maps:fold(fun(ToolName, #tool_entry{description = Desc}, Acc) ->
        NameStr = string:lowercase(binary_to_list(ToolName)),
        DescStr = string:lowercase(binary_to_list(Desc)),
        case (string:str(NameStr, QueryLower) > 0) orelse (string:str(DescStr, QueryLower) > 0) of
            true -> [ToolName | Acc];
            false -> Acc
        end
    end, [], Tools),
    
    % Combine and deduplicate results
    AllMatches = lists:usort(TagMatches ++ NameDescMatches),
    
    % Return tool entries for matches
    [{ToolName, maps:get(ToolName, Tools)} || ToolName <- AllMatches, maps:is_key(ToolName, Tools)].

tool_entry_to_metadata(#tool_entry{} = Entry) ->
    #{
        name => Entry#tool_entry.name,
        description => Entry#tool_entry.description,
        source => Entry#tool_entry.source,
        version => Entry#tool_entry.version,
        tags => Entry#tool_entry.tags,
        dependencies => Entry#tool_entry.dependencies,
        created_at => Entry#tool_entry.created_at,
        updated_at => Entry#tool_entry.updated_at,
        metadata => Entry#tool_entry.metadata,
        has_code => Entry#tool_entry.code =/= undefined,
        has_ast => Entry#tool_entry.ast =/= undefined,
        has_url => Entry#tool_entry.url =/= undefined
    }.

sync_builtin_tools(State) ->
    % Sync from existing agent_tools module
    try
        AllTools = agent_tools:list_tools(),
        NewState = lists:foldl(fun(ToolName, AccState) ->
            % Convert atom to binary
            ToolNameBin = atom_to_binary(ToolName, utf8),
            
            % Get tool schema from agent_tools
            case agent_tools:get_tools([ToolName]) of
                [] -> AccState;
                [ToolSchema] ->
                    % Create tool entry
                    ToolEntry = create_tool_entry(ToolNameBin, ToolSchema, local, #{source_module => agent_tools}),
                    NewTools = maps:put(ToolNameBin, ToolEntry, AccState#state.tools),
                    NewIndex = update_tool_index(ToolNameBin, ToolEntry, AccState#state.tool_index),
                    AccState#state{tools = NewTools, tool_index = NewIndex}
            end
        end, State, AllTools),
        NewState
    catch
        _:_ -> State % If agent_tools not available, continue with empty state
    end.

sync_all_sources(State) ->
    % Sync from multiple sources
    State1 = sync_builtin_tools(State),
    State2 = sync_mcp_tools(State1),
    State3 = sync_data_model_tools(State2),
    State3.

sync_mcp_tools(State) ->
    % Sync tools from MCP servers
    try
        case mcp_manager:get_all_tools() of
            {ok, McpTools} ->
                lists:foldl(fun({ToolName, ToolSchema}, AccState) ->
                    ToolNameBin = ensure_binary(ToolName),
                    ToolEntry = create_tool_entry(ToolNameBin, ToolSchema, mcp, #{source => mcp}),
                    NewTools = maps:put(ToolNameBin, ToolEntry, AccState#state.tools),
                    NewIndex = update_tool_index(ToolNameBin, ToolEntry, AccState#state.tool_index),
                    AccState#state{tools = NewTools, tool_index = NewIndex}
                end, State, McpTools);
            _ -> State
        end
    catch
        _:_ -> State
    end.

sync_data_model_tools(State) ->
    % Sync tools from data models (placeholder for future implementation)
    % This could integrate with knowledge bases, databases, etc.
    State.

ensure_binary(Value) when is_binary(Value) -> Value;
ensure_binary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
ensure_binary(Value) when is_list(Value) -> list_to_binary(Value);
ensure_binary(Value) -> iolist_to_binary(io_lib:format("~p", [Value])).