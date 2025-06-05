-module(agent_tool_registry).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    register_tool/3,
    unregister_tool/2,
    share_tool/3,
    borrow_tool/3,
    return_tool/2,
    list_agent_tools/1,
    list_available_tools/0,
    search_tools/1,
    get_tool_info/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    tools = #{} :: #{tool_id() => tool_info()},
    agent_tools = #{} :: #{agent_id() => [tool_id()]},
    shared_tools = #{} :: #{tool_id() => share_info()},
    borrowed_tools = #{} :: #{agent_id() => [{tool_id(), owner_id()}]}
}).

-record(tool_info, {
    id :: tool_id(),
    name :: binary(),
    description :: binary(),
    owner :: agent_id(),
    schema :: map(),
    capabilities :: [binary()],
    shared = false :: boolean(),
    created_at :: integer()
}).

-record(share_info, {
    max_borrows = unlimited :: unlimited | integer(),
    current_borrows = [] :: [agent_id()],
    permissions = all :: all | [agent_id()],
    expires :: undefined | integer()
}).

-type tool_id() :: binary().
-type agent_id() :: binary().
-type owner_id() :: agent_id().
-type tool_info() :: #tool_info{}.
-type share_info() :: #share_info{}.

%% API Functions
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Register a new tool for an agent
register_tool(AgentId, ToolName, ToolSchema) ->
    gen_server:call(?MODULE, {register_tool, AgentId, ToolName, ToolSchema}).

%% Unregister a tool
unregister_tool(AgentId, ToolId) ->
    gen_server:call(?MODULE, {unregister_tool, AgentId, ToolId}).

%% Share a tool with other agents
share_tool(AgentId, ToolId, ShareOptions) ->
    gen_server:call(?MODULE, {share_tool, AgentId, ToolId, ShareOptions}).

%% Borrow a shared tool
borrow_tool(BorrowerAgentId, OwnerAgentId, ToolId) ->
    gen_server:call(?MODULE, {borrow_tool, BorrowerAgentId, OwnerAgentId, ToolId}).

%% Return a borrowed tool
return_tool(BorrowerAgentId, ToolId) ->
    gen_server:call(?MODULE, {return_tool, BorrowerAgentId, ToolId}).

%% List all tools owned by an agent
list_agent_tools(AgentId) ->
    gen_server:call(?MODULE, {list_agent_tools, AgentId}).

%% List all available tools (owned + shared)
list_available_tools() ->
    gen_server:call(?MODULE, list_available_tools).

%% Search tools by capability or name
search_tools(Query) ->
    gen_server:call(?MODULE, {search_tools, Query}).

%% Get detailed tool information
get_tool_info(AgentId, ToolId) ->
    gen_server:call(?MODULE, {get_tool_info, AgentId, ToolId}).

%% gen_server callbacks
init([]) ->
    {ok, #state{}}.

handle_call({register_tool, AgentId, ToolName, ToolSchema}, _From, State) ->
    ToolId = generate_tool_id(AgentId, ToolName),
    
    ToolInfo = #tool_info{
        id = ToolId,
        name = ToolName,
        description = maps:get(<<"description">>, ToolSchema, <<>>),
        owner = AgentId,
        schema = ToolSchema,
        capabilities = maps:get(<<"capabilities">>, ToolSchema, []),
        created_at = erlang:system_time(millisecond)
    },
    
    NewTools = maps:put(ToolId, ToolInfo, State#state.tools),
    
    AgentTools = maps:get(AgentId, State#state.agent_tools, []),
    NewAgentTools = maps:put(AgentId, [ToolId | AgentTools], State#state.agent_tools),
    
    NewState = State#state{
        tools = NewTools,
        agent_tools = NewAgentTools
    },
    
    notify_tool_registered(AgentId, ToolId, ToolInfo),
    
    {reply, {ok, ToolId}, NewState};

handle_call({unregister_tool, AgentId, ToolId}, _From, State) ->
    case maps:get(ToolId, State#state.tools, undefined) of
        undefined ->
            {reply, {error, tool_not_found}, State};
        ToolInfo when ToolInfo#tool_info.owner =/= AgentId ->
            {reply, {error, not_owner}, State};
        _ ->
            NewTools = maps:remove(ToolId, State#state.tools),
            
            AgentTools = maps:get(AgentId, State#state.agent_tools, []),
            NewAgentTools = maps:put(AgentId, lists:delete(ToolId, AgentTools), State#state.agent_tools),
            
            % Remove from shared tools if shared
            NewSharedTools = maps:remove(ToolId, State#state.shared_tools),
            
            % Return borrowed instances
            NewBorrowedTools = return_all_borrows(ToolId, State#state.borrowed_tools),
            
            NewState = State#state{
                tools = NewTools,
                agent_tools = NewAgentTools,
                shared_tools = NewSharedTools,
                borrowed_tools = NewBorrowedTools
            },
            
            notify_tool_unregistered(AgentId, ToolId),
            
            {reply, ok, NewState}
    end;

handle_call({share_tool, AgentId, ToolId, ShareOptions}, _From, State) ->
    case maps:get(ToolId, State#state.tools, undefined) of
        undefined ->
            {reply, {error, tool_not_found}, State};
        ToolInfo when ToolInfo#tool_info.owner =/= AgentId ->
            {reply, {error, not_owner}, State};
        ToolInfo ->
            ShareInfo = #share_info{
                max_borrows = maps:get(<<"max_borrows">>, ShareOptions, unlimited),
                permissions = maps:get(<<"permissions">>, ShareOptions, all),
                expires = maps:get(<<"expires">>, ShareOptions, undefined)
            },
            
            UpdatedToolInfo = ToolInfo#tool_info{shared = true},
            NewTools = maps:put(ToolId, UpdatedToolInfo, State#state.tools),
            NewSharedTools = maps:put(ToolId, ShareInfo, State#state.shared_tools),
            
            NewState = State#state{
                tools = NewTools,
                shared_tools = NewSharedTools
            },
            
            notify_tool_shared(AgentId, ToolId, ShareInfo),
            
            {reply, ok, NewState}
    end;

handle_call({borrow_tool, BorrowerAgentId, OwnerAgentId, ToolId}, _From, State) ->
    case can_borrow_tool(BorrowerAgentId, ToolId, State) of
        {ok, ToolInfo} ->
            % Update share info
            ShareInfo = maps:get(ToolId, State#state.shared_tools),
            UpdatedShareInfo = ShareInfo#share_info{
                current_borrows = [BorrowerAgentId | ShareInfo#share_info.current_borrows]
            },
            NewSharedTools = maps:put(ToolId, UpdatedShareInfo, State#state.shared_tools),
            
            % Track borrowed tool
            BorrowerTools = maps:get(BorrowerAgentId, State#state.borrowed_tools, []),
            NewBorrowerTools = [{ToolId, OwnerAgentId} | BorrowerTools],
            NewBorrowedTools = maps:put(BorrowerAgentId, NewBorrowerTools, State#state.borrowed_tools),
            
            NewState = State#state{
                shared_tools = NewSharedTools,
                borrowed_tools = NewBorrowedTools
            },
            
            notify_tool_borrowed(BorrowerAgentId, OwnerAgentId, ToolId),
            
            {reply, {ok, tool_to_map(ToolInfo)}, NewState};
        Error ->
            {reply, Error, State}
    end;

handle_call({return_tool, BorrowerAgentId, ToolId}, _From, State) ->
    BorrowerTools = maps:get(BorrowerAgentId, State#state.borrowed_tools, []),
    case lists:keyfind(ToolId, 1, BorrowerTools) of
        false ->
            {reply, {error, tool_not_borrowed}, State};
        {ToolId, _OwnerAgentId} ->
            % Update borrowed tools
            NewBorrowerTools = lists:keydelete(ToolId, 1, BorrowerTools),
            NewBorrowedTools = maps:put(BorrowerAgentId, NewBorrowerTools, State#state.borrowed_tools),
            
            % Update share info
            ShareInfo = maps:get(ToolId, State#state.shared_tools),
            UpdatedShareInfo = ShareInfo#share_info{
                current_borrows = lists:delete(BorrowerAgentId, ShareInfo#share_info.current_borrows)
            },
            NewSharedTools = maps:put(ToolId, UpdatedShareInfo, State#state.shared_tools),
            
            NewState = State#state{
                shared_tools = NewSharedTools,
                borrowed_tools = NewBorrowedTools
            },
            
            notify_tool_returned(BorrowerAgentId, ToolId),
            
            {reply, ok, NewState}
    end;

handle_call({list_agent_tools, AgentId}, _From, State) ->
    OwnedTools = maps:get(AgentId, State#state.agent_tools, []),
    BorrowedTools = maps:get(AgentId, State#state.borrowed_tools, []),
    
    OwnedToolInfos = [tool_to_map(maps:get(ToolId, State#state.tools)) || 
                      ToolId <- OwnedTools,
                      maps:is_key(ToolId, State#state.tools)],
    
    BorrowedToolInfos = [{tool_to_map(maps:get(ToolId, State#state.tools)), Owner} ||
                         {ToolId, Owner} <- BorrowedTools,
                         maps:is_key(ToolId, State#state.tools)],
    
    {reply, {ok, #{owned => OwnedToolInfos, borrowed => BorrowedToolInfos}}, State};

handle_call(list_available_tools, _From, State) ->
    SharedTools = [tool_to_map(ToolInfo) || 
                   {_ToolId, ToolInfo} <- maps:to_list(State#state.tools),
                   ToolInfo#tool_info.shared],
    {reply, {ok, SharedTools}, State};

handle_call({search_tools, Query}, _From, State) ->
    QueryLower = string:lowercase(binary_to_list(Query)),
    
    MatchingTools = maps:filter(fun(_ToolId, ToolInfo) ->
        NameMatch = string:find(string:lowercase(binary_to_list(ToolInfo#tool_info.name)), QueryLower) =/= nomatch,
        DescMatch = string:find(string:lowercase(binary_to_list(ToolInfo#tool_info.description)), QueryLower) =/= nomatch,
        CapMatch = lists:any(fun(Cap) ->
            string:find(string:lowercase(binary_to_list(Cap)), QueryLower) =/= nomatch
        end, ToolInfo#tool_info.capabilities),
        
        NameMatch orelse DescMatch orelse CapMatch
    end, State#state.tools),
    
    Results = [tool_to_map(ToolInfo) || {_ToolId, ToolInfo} <- maps:to_list(MatchingTools)],
    
    {reply, {ok, Results}, State};

handle_call({get_tool_info, _AgentId, ToolId}, _From, State) ->
    case maps:get(ToolId, State#state.tools, undefined) of
        undefined ->
            {reply, {error, tool_not_found}, State};
        ToolInfo ->
            ShareInfo = maps:get(ToolId, State#state.shared_tools, undefined),
            FullInfo = tool_to_map(ToolInfo),
            FullInfoWithShare = case ShareInfo of
                undefined -> FullInfo;
                _ -> FullInfo#{share_info => share_info_to_map(ShareInfo)}
            end,
            {reply, {ok, FullInfoWithShare}, State}
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

%% Internal functions
generate_tool_id(AgentId, ToolName) ->
    Timestamp = erlang:system_time(millisecond),
    Random = rand:uniform(10000),
    list_to_binary(io_lib:format("~s_~s_~p_~p", [AgentId, ToolName, Timestamp, Random])).

can_borrow_tool(BorrowerAgentId, ToolId, State) ->
    case maps:get(ToolId, State#state.tools, undefined) of
        undefined ->
            {error, tool_not_found};
        ToolInfo when not ToolInfo#tool_info.shared ->
            {error, tool_not_shared};
        ToolInfo ->
            case maps:get(ToolId, State#state.shared_tools, undefined) of
                undefined ->
                    {error, share_info_not_found};
                ShareInfo ->
                    case check_borrow_permissions(BorrowerAgentId, ShareInfo) of
                        ok ->
                            case check_borrow_limit(ShareInfo) of
                                ok -> {ok, ToolInfo};
                                Error -> Error
                            end;
                        Error ->
                            Error
                    end
            end
    end.

check_borrow_permissions(BorrowerAgentId, ShareInfo) ->
    case ShareInfo#share_info.permissions of
        all -> ok;
        AllowedAgents when is_list(AllowedAgents) ->
            case lists:member(BorrowerAgentId, AllowedAgents) of
                true -> ok;
                false -> {error, permission_denied}
            end
    end.

check_borrow_limit(ShareInfo) ->
    case ShareInfo#share_info.max_borrows of
        unlimited -> ok;
        MaxBorrows when is_integer(MaxBorrows) ->
            CurrentBorrows = length(ShareInfo#share_info.current_borrows),
            if
                CurrentBorrows < MaxBorrows -> ok;
                true -> {error, borrow_limit_reached}
            end
    end.

return_all_borrows(ToolId, BorrowedTools) ->
    maps:map(fun(_AgentId, Tools) ->
        lists:filter(fun({TId, _}) -> TId =/= ToolId end, Tools)
    end, BorrowedTools).

tool_to_map(#tool_info{} = ToolInfo) ->
    #{
        id => ToolInfo#tool_info.id,
        name => ToolInfo#tool_info.name,
        description => ToolInfo#tool_info.description,
        owner => ToolInfo#tool_info.owner,
        schema => ToolInfo#tool_info.schema,
        capabilities => ToolInfo#tool_info.capabilities,
        shared => ToolInfo#tool_info.shared,
        created_at => ToolInfo#tool_info.created_at
    }.

share_info_to_map(#share_info{} = ShareInfo) ->
    #{
        max_borrows => ShareInfo#share_info.max_borrows,
        current_borrows => ShareInfo#share_info.current_borrows,
        permissions => ShareInfo#share_info.permissions,
        expires => ShareInfo#share_info.expires
    }.

notify_tool_registered(AgentId, ToolId, ToolInfo) ->
    broadcast_event(#{
        type => tool_registered,
        agent_id => AgentId,
        tool_id => ToolId,
        tool_name => ToolInfo#tool_info.name,
        timestamp => erlang:system_time(millisecond)
    }).

notify_tool_unregistered(AgentId, ToolId) ->
    broadcast_event(#{
        type => tool_unregistered,
        agent_id => AgentId,
        tool_id => ToolId,
        timestamp => erlang:system_time(millisecond)
    }).

notify_tool_shared(AgentId, ToolId, _ShareInfo) ->
    broadcast_event(#{
        type => tool_shared,
        agent_id => AgentId,
        tool_id => ToolId,
        timestamp => erlang:system_time(millisecond)
    }).

notify_tool_borrowed(BorrowerAgentId, OwnerAgentId, ToolId) ->
    broadcast_event(#{
        type => tool_borrowed,
        borrower_id => BorrowerAgentId,
        owner_id => OwnerAgentId,
        tool_id => ToolId,
        timestamp => erlang:system_time(millisecond)
    }).

notify_tool_returned(BorrowerAgentId, ToolId) ->
    broadcast_event(#{
        type => tool_returned,
        borrower_id => BorrowerAgentId,
        tool_id => ToolId,
        timestamp => erlang:system_time(millisecond)
    }).

broadcast_event(Event) ->
    case catch gproc:send({p, l, websocket}, {broadcast, jsx:encode(Event)}) of
        {'EXIT', _} -> ok;
        _ -> ok
    end.