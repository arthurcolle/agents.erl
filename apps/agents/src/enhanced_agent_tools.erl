%% enhanced_agent_tools.erl
%% Enhanced agent tools integration with universal tool registry
-module(enhanced_agent_tools).
-behaviour(gen_server).

-export([
    start_link/1,
    get_tools_for_agent/1,
    execute_tool/3,
    register_agent_tool/3,
    search_available_tools/2,
    get_tool_sources/0,
    sync_agent_tools/1,
    get_tool_metadata/1,
    list_agent_tools/1
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

% Tool entry record definition (from universal_tool_registry)
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
    agent_tool_cache = #{} :: map(), % agent_id -> {tools, last_sync}
    execution_stats = #{} :: map(),  % tool_name -> execution_stats
    cache_ttl = 300000 :: integer()  % 5 minutes cache TTL
}).

%% API Functions
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%% Get all tools available to an agent from multiple sources
get_tools_for_agent(AgentId) when is_binary(AgentId) ->
    gen_server:call(?SERVER, {get_tools_for_agent, AgentId}).

%% Execute a tool with enhanced routing and error handling
execute_tool(AgentId, ToolName, Arguments) when is_binary(AgentId), is_binary(ToolName) ->
    gen_server:call(?SERVER, {execute_tool, AgentId, ToolName, Arguments}, infinity).

%% Register a new tool for an agent
register_agent_tool(AgentId, ToolName, ToolDefinition) ->
    gen_server:call(?SERVER, {register_agent_tool, AgentId, ToolName, ToolDefinition}).

%% Search for available tools
search_available_tools(AgentId, Query) ->
    gen_server:call(?SERVER, {search_available_tools, AgentId, Query}).

%% Get all tool sources
get_tool_sources() ->
    gen_server:call(?SERVER, get_tool_sources).

%% Sync tools for an agent from all sources
sync_agent_tools(AgentId) ->
    gen_server:call(?SERVER, {sync_agent_tools, AgentId}).

%% Get tool metadata
get_tool_metadata(ToolName) ->
    gen_server:call(?SERVER, {get_tool_metadata, ToolName}).

%% List all tools for an agent
list_agent_tools(AgentId) ->
    gen_server:call(?SERVER, {list_agent_tools, AgentId}).

%% gen_server callbacks
init(Options) ->
    % Start the universal tool registry if not already started
    case whereis(universal_tool_registry) of
        undefined -> 
            {ok, _} = universal_tool_registry:start_link();
        _ -> ok
    end,
    
    % Schedule cache cleanup
    timer:send_interval(60000, cleanup_cache),
    
    {ok, #state{}}.

handle_call({get_tools_for_agent, AgentId}, _From, State) ->
    case get_cached_tools(AgentId, State) of
        {ok, Tools} -> 
            {reply, {ok, Tools}, State};
        cache_miss ->
            {Tools, NewState} = fetch_and_cache_tools(AgentId, State),
            {reply, {ok, Tools}, NewState}
    end;

handle_call({execute_tool, AgentId, ToolName, Arguments}, _From, State) ->
    Result = execute_tool_internal(AgentId, ToolName, Arguments),
    NewState = update_execution_stats(ToolName, Result, State),
    {reply, Result, NewState};

handle_call({register_agent_tool, AgentId, ToolName, ToolDefinition}, _From, State) ->
    % Register in universal registry
    case universal_tool_registry:register_agent_tool(AgentId, ToolName, ToolDefinition) of
        ok ->
            % Invalidate cache for this agent
            NewState = invalidate_agent_cache(AgentId, State),
            {reply, ok, NewState};
        Error ->
            {reply, Error, State}
    end;

handle_call({search_available_tools, AgentId, Query}, _From, State) ->
    % Search across all sources
    Results = search_tools_for_agent(AgentId, Query),
    {reply, Results, State};

handle_call(get_tool_sources, _From, State) ->
    Sources = [
        #{name => <<"universal_registry">>, type => <<"local">>, status => <<"active">>},
        #{name => <<"agent_tools">>, type => <<"builtin">>, status => <<"active">>},
        #{name => <<"mcp_servers">>, type => <<"external">>, status => get_mcp_status()},
        #{name => <<"data_models">>, type => <<"database">>, status => <<"planned">>}
    ],
    {reply, Sources, State};

handle_call({sync_agent_tools, AgentId}, _From, State) ->
    % Force refresh of tools for this agent
    NewState = invalidate_agent_cache(AgentId, State),
    {Tools, FinalState} = fetch_and_cache_tools(AgentId, NewState),
    {reply, {ok, length(Tools)}, FinalState};

handle_call({get_tool_metadata, ToolName}, _From, State) ->
    Result = universal_tool_registry:get_tool_metadata(ToolName),
    {reply, Result, State};

handle_call({list_agent_tools, AgentId}, _From, State) ->
    case get_cached_tools(AgentId, State) of
        {ok, Tools} -> 
            ToolNames = [maps:get(<<"name">>, Tool, <<"unknown">>) || Tool <- Tools],
            {reply, {ok, ToolNames}, State};
        cache_miss ->
            {Tools, NewState} = fetch_and_cache_tools(AgentId, State),
            ToolNames = [maps:get(<<"name">>, Tool, <<"unknown">>) || Tool <- Tools],
            {reply, {ok, ToolNames}, NewState}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup_cache, State) ->
    NewState = cleanup_expired_cache(State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal Functions
get_cached_tools(AgentId, #state{agent_tool_cache = Cache, cache_ttl = TTL}) ->
    case maps:get(AgentId, Cache, undefined) of
        undefined -> cache_miss;
        {Tools, Timestamp} ->
            case erlang:system_time(millisecond) - Timestamp < TTL of
                true -> {ok, Tools};
                false -> cache_miss
            end
    end.

fetch_and_cache_tools(AgentId, State) ->
    % Fetch tools from all sources
    AllTools = fetch_tools_from_all_sources(AgentId),
    
    % Cache the results
    Timestamp = erlang:system_time(millisecond),
    NewCache = maps:put(AgentId, {AllTools, Timestamp}, State#state.agent_tool_cache),
    
    {AllTools, State#state{agent_tool_cache = NewCache}}.

fetch_tools_from_all_sources(AgentId) ->
    Sources = [
        fun() -> fetch_from_universal_registry(AgentId) end,
        fun() -> fetch_from_builtin_tools() end,
        fun() -> fetch_from_mcp_sources() end,
        fun() -> fetch_from_data_models(AgentId) end
    ],
    
    % Fetch from all sources in parallel
    Results = parallel_fetch(Sources),
    
    % Merge and deduplicate tools
    AllTools = lists:flatten(Results),
    deduplicate_tools(AllTools).

fetch_from_universal_registry(AgentId) ->
    try
        case universal_tool_registry:get_tools_for_agent(AgentId) of
            Tools when is_map(Tools) ->
                maps:values(maps:map(fun(_, ToolEntry) ->
                    tool_entry_to_schema(ToolEntry)
                end, Tools));
            _ -> []
        end
    catch
        _:_ -> []
    end.

fetch_from_builtin_tools() ->
    try
        case agent_tools:list_tools() of
            Tools when is_list(Tools) ->
                lists:filtermap(fun(ToolName) ->
                    case agent_tools:get_tools([ToolName]) of
                        [ToolSchema] -> {true, ToolSchema};
                        _ -> false
                    end
                end, Tools);
            _ -> []
        end
    catch
        _:_ -> []
    end.

fetch_from_mcp_sources() ->
    try
        case mcp_manager:get_all_tools() of
            {ok, McpTools} ->
                [ToolSchema || {_Name, ToolSchema} <- McpTools];
            _ -> []
        end
    catch
        _:_ -> []
    end.

fetch_from_data_models(_AgentId) ->
    % Placeholder for data model integration
    % Could fetch tools from knowledge bases, databases, etc.
    [].

parallel_fetch(Sources) ->
    Parent = self(),
    Refs = lists:map(fun(SourceFun) ->
        Ref = make_ref(),
        spawn_link(fun() ->
            Result = try SourceFun() catch _:_ -> [] end,
            Parent ! {result, Ref, Result}
        end),
        Ref
    end, Sources),
    
    collect_parallel_results(Refs, []).

collect_parallel_results([], Results) ->
    Results;
collect_parallel_results(Refs, Results) ->
    receive
        {result, Ref, Result} ->
            NewRefs = lists:delete(Ref, Refs),
            collect_parallel_results(NewRefs, [Result | Results])
    after 5000 ->
        % Timeout after 5 seconds
        Results
    end.

deduplicate_tools(Tools) ->
    % Deduplicate based on tool name
    ToolMap = lists:foldl(fun(Tool, Acc) ->
        Name = maps:get(<<"name">>, Tool, <<"unknown">>),
        case maps:is_key(Name, Acc) of
            true -> Acc; % Keep first occurrence
            false -> maps:put(Name, Tool, Acc)
        end
    end, #{}, Tools),
    maps:values(ToolMap).

tool_entry_to_schema(ToolEntry) ->
    % Convert tool entry record to OpenAI schema format
    #{
        <<"type">> => <<"function">>,
        <<"function">> => #{
            <<"name">> => ToolEntry#tool_entry.name,
            <<"description">> => ToolEntry#tool_entry.description,
            <<"parameters">> => maps:get(<<"parameters">>, ToolEntry#tool_entry.schema, #{})
        }
    }.

execute_tool_internal(AgentId, ToolName, Arguments) ->
    % Get tool metadata to determine execution method
    case universal_tool_registry:get_tool(ToolName) of
        not_found ->
            % Fallback to legacy agent_tools
            execute_legacy_tool(ToolName, Arguments);
        ToolEntry ->
            execute_by_source(ToolEntry, AgentId, Arguments)
    end.

execute_by_source(ToolEntry, AgentId, Arguments) ->
    case ToolEntry#tool_entry.source of
        local when ToolEntry#tool_entry.code =/= undefined ->
            execute_code_tool(ToolEntry, Arguments);
        local when ToolEntry#tool_entry.ast =/= undefined ->
            execute_ast_tool(ToolEntry, Arguments);
        external when ToolEntry#tool_entry.url =/= undefined ->
            execute_url_tool(ToolEntry, Arguments);
        mcp ->
            execute_mcp_tool(ToolEntry, Arguments);
        agent ->
            execute_agent_tool(ToolEntry, AgentId, Arguments);
        _ ->
            execute_legacy_tool(ToolEntry#tool_entry.name, Arguments)
    end.

execute_code_tool(ToolEntry, Arguments) ->
    % Execute Erlang code directly
    try
        % This is a simplified example - in production you'd want proper sandboxing
        Code = ToolEntry#tool_entry.code,
        {ok, Tokens, _} = erl_scan:string(binary_to_list(Code)),
        {ok, Exprs} = erl_parse:parse_exprs(Tokens),
        {value, Result, _} = erl_eval:exprs(Exprs, [{'Arguments', Arguments}]),
        Result
    catch
        E:R -> {error, {code_execution_failed, E, R}}
    end.

execute_ast_tool(ToolEntry, Arguments) ->
    % Execute pre-compiled AST
    try
        AST = ToolEntry#tool_entry.ast,
        {value, Result, _} = erl_eval:exprs(AST, [{'Arguments', Arguments}]),
        Result
    catch
        E:R -> {error, {ast_execution_failed, E, R}}
    end.

execute_url_tool(ToolEntry, Arguments) ->
    % Execute via HTTP request
    try
        Url = ToolEntry#tool_entry.url,
        Body = jsx:encode(Arguments),
        case httpc:request(post, {binary_to_list(Url), [], "application/json", Body}, [], []) of
            {ok, {{_, 200, _}, _, ResponseBody}} ->
                jsx:decode(list_to_binary(ResponseBody));
            {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
                {error, {http_error, StatusCode, ErrorBody}};
            {error, Reason} ->
                {error, {http_request_failed, Reason}}
        end
    catch
        E:R -> {error, {url_execution_failed, E, R}}
    end.

execute_mcp_tool(ToolEntry, Arguments) ->
    % Execute via MCP
    try
        ToolName = ToolEntry#tool_entry.name,
        mcp_manager:execute_tool(ToolName, Arguments)
    catch
        E:R -> {error, {mcp_execution_failed, E, R}}
    end.

execute_agent_tool(ToolEntry, AgentId, Arguments) ->
    % Execute tool registered by another agent
    try
        SourceAgentId = maps:get(agent_id, ToolEntry#tool_entry.metadata, AgentId),
        % Could implement inter-agent communication here
        {error, {agent_tool_execution_not_implemented, SourceAgentId}}
    catch
        E:R -> {error, {agent_tool_execution_failed, E, R}}
    end.

execute_legacy_tool(ToolName, Arguments) ->
    % Fallback to existing agent_tools
    try
        ToolNameAtom = binary_to_atom(ToolName, utf8),
        agent_tools:execute_tool(ToolNameAtom, Arguments)
    catch
        E:R -> {error, {legacy_execution_failed, E, R}}
    end.

search_tools_for_agent(AgentId, Query) ->
    % Search tools available to this agent
    AllMatches = universal_tool_registry:search_tools(Query),
    
    % Filter by agent access permissions (placeholder for future ACL implementation)
    FilteredMatches = lists:filter(fun({_ToolName, _ToolEntry}) ->
        % For now, all tools are accessible to all agents
        true
    end, AllMatches),
    
    % Format results
    lists:map(fun({ToolName, ToolEntry}) ->
        #{
            name => ToolName,
            description => ToolEntry#tool_entry.description,
            source => ToolEntry#tool_entry.source,
            tags => ToolEntry#tool_entry.tags
        }
    end, FilteredMatches).

invalidate_agent_cache(AgentId, State) ->
    NewCache = maps:remove(AgentId, State#state.agent_tool_cache),
    State#state{agent_tool_cache = NewCache}.

cleanup_expired_cache(#state{agent_tool_cache = Cache, cache_ttl = TTL} = State) ->
    Now = erlang:system_time(millisecond),
    NewCache = maps:filter(fun(_, {_Tools, Timestamp}) ->
        Now - Timestamp < TTL
    end, Cache),
    State#state{agent_tool_cache = NewCache}.

update_execution_stats(ToolName, Result, #state{execution_stats = Stats} = State) ->
    ExistingStats = maps:get(ToolName, Stats, #{executions => 0, successes => 0, errors => 0}),
    
    NewStats = case Result of
        {error, _} -> 
            ExistingStats#{
                executions => maps:get(executions, ExistingStats) + 1,
                errors => maps:get(errors, ExistingStats) + 1
            };
        _ -> 
            ExistingStats#{
                executions => maps:get(executions, ExistingStats) + 1,
                successes => maps:get(successes, ExistingStats) + 1
            }
    end,
    
    UpdatedStatsMap = maps:put(ToolName, NewStats, Stats),
    State#state{execution_stats = UpdatedStatsMap}.

get_mcp_status() ->
    try
        case mcp_manager:get_status() of
            {ok, _} -> <<"active">>;
            _ -> <<"inactive">>
        end
    catch
        _:_ -> <<"inactive">>
    end.