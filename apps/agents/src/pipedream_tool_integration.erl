-module(pipedream_tool_integration).
-behaviour(gen_server).

-export([
    start_link/0,
    register_user_tools/1,
    get_user_tools/1,
    execute_pipedream_tool/3,
    format_tools_for_agents/1,
    sync_user_tools/1,
    get_integration_stats/0
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    user_tools = #{}, % external_user_id => [formatted_tools]
    tool_cache = #{}, % tool_name => {app_slug, original_name, schema}
    sync_interval = 300000, % 5 minutes
    stats = #{
        tools_registered => 0,
        tools_executed => 0,
        execution_errors => 0,
        last_sync => 0
    }
}).

-define(SERVER, ?MODULE).

%% Logging macros (consistent with agent_tools.erl)
-define(LOG_INFO(Msg), colored_logger:data(processed, Msg)).
-define(LOG_INFO(Msg, Args), colored_logger:data(processed, io_lib:format(Msg, Args))).
-define(LOG_ERROR(Msg), colored_logger:fire(inferno, Msg)).
-define(LOG_ERROR(Msg, Args), colored_logger:fire(inferno, io_lib:format(Msg, Args))).
-define(LOG_SUCCESS(Msg), colored_logger:complete(success, Msg)).
-define(LOG_SUCCESS(Msg, Args), colored_logger:complete(success, io_lib:format(Msg, Args))).

%% API Functions

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

register_user_tools(ExternalUserId) ->
    gen_server:call(?SERVER, {register_user_tools, ExternalUserId}).

get_user_tools(ExternalUserId) ->
    gen_server:call(?SERVER, {get_user_tools, ExternalUserId}).

execute_pipedream_tool(ExternalUserId, ToolName, Arguments) ->
    gen_server:call(?SERVER, {execute_pipedream_tool, ExternalUserId, ToolName, Arguments}, 30000).

format_tools_for_agents(ExternalUserId) ->
    gen_server:call(?SERVER, {format_tools_for_agents, ExternalUserId}).

sync_user_tools(ExternalUserId) ->
    gen_server:cast(?SERVER, {sync_user_tools, ExternalUserId}).

get_integration_stats() ->
    gen_server:call(?SERVER, get_integration_stats).

%% Gen Server Callbacks

init([]) ->
    % Schedule periodic sync
    schedule_sync(),
    
    {ok, #state{}}.

handle_call({register_user_tools, ExternalUserId}, _From, State) ->
    case fetch_and_register_user_tools(ExternalUserId, State) of
        {ok, NewState} ->
            ?LOG_SUCCESS("[PIPEDREAM] Registered tools for user ~s", [ExternalUserId]),
            {reply, ok, NewState};
        {error, Reason} ->
            ?LOG_ERROR("[PIPEDREAM] Failed to register tools for user ~s: ~p", [ExternalUserId, Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call({get_user_tools, ExternalUserId}, _From, State) ->
    UserTools = maps:get(ExternalUserId, State#state.user_tools, []),
    {reply, {ok, UserTools}, State};

handle_call({execute_pipedream_tool, ExternalUserId, ToolName, Arguments}, _From, State) ->
    StartTime = erlang:monotonic_time(millisecond),
    ?LOG_INFO("[PIPEDREAM] Executing tool ~s for user ~s", [ToolName, ExternalUserId]),
    
    Result = case maps:get(ToolName, State#state.tool_cache, undefined) of
        undefined ->
            {error, tool_not_found};
        {AppSlug, OriginalName, _Schema} ->
            execute_tool_via_mcp(ExternalUserId, AppSlug, OriginalName, Arguments)
    end,
    
    Duration = erlang:monotonic_time(millisecond) - StartTime,
    NewStats = case Result of
        {ok, _} ->
            ?LOG_SUCCESS("[PIPEDREAM] Tool ~s executed successfully in ~pms", [ToolName, Duration]),
            maps:put(tools_executed, 
                    maps:get(tools_executed, State#state.stats, 0) + 1,
                    State#state.stats);
        {error, Reason} ->
            ?LOG_ERROR("[PIPEDREAM] Tool ~s failed after ~pms: ~p", [ToolName, Duration, Reason]),
            ErrorCount = maps:get(execution_errors, State#state.stats, 0) + 1,
            maps:put(execution_errors, ErrorCount, State#state.stats)
    end,
    
    {reply, Result, State#state{stats = NewStats}};

handle_call({format_tools_for_agents, ExternalUserId}, _From, State) ->
    UserTools = maps:get(ExternalUserId, State#state.user_tools, []),
    FormattedTools = format_tools_for_openai(UserTools),
    {reply, {ok, FormattedTools}, State};

handle_call(get_integration_stats, _From, #state{stats = Stats} = State) ->
    EnhancedStats = maps:merge(Stats, #{
        total_users => maps:size(State#state.user_tools),
        cached_tools => maps:size(State#state.tool_cache),
        timestamp => erlang:system_time(second)
    }),
    {reply, {ok, EnhancedStats}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({sync_user_tools, ExternalUserId}, State) ->
    case fetch_and_register_user_tools(ExternalUserId, State) of
        {ok, NewState} ->
            ?LOG_INFO("[PIPEDREAM] Synced tools for user ~s", [ExternalUserId]),
            {noreply, NewState};
        {error, Reason} ->
            ?LOG_ERROR("[PIPEDREAM] Failed to sync tools for user ~s: ~p", [ExternalUserId, Reason]),
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(sync_timer, State) ->
    % Sync tools for all registered users
    NewState = sync_all_users(State),
    schedule_sync(),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal Functions

schedule_sync() ->
    erlang:send_after(300000, self(), sync_timer). % 5 minutes

fetch_and_register_user_tools(ExternalUserId, State) ->
    try
        case pipedream_autodiscovery:get_user_tools(ExternalUserId) of
            {ok, PipedreamTools} ->
                % Format tools for agent system
                FormattedTools = format_pipedream_tools(PipedreamTools),
                
                % Register each tool with the agent tools system
                RegistrationResults = lists:map(fun(Tool) ->
                    ToolName = binary_to_list(maps:get(<<"name">>, Tool)),
                    
                    % Cache tool metadata
                    ToolCache = State#state.tool_cache,
                    AppSlug = binary_to_list(maps:get(<<"app_slug">>, Tool)),
                    OriginalName = binary_to_list(maps:get(<<"original_name">>, Tool)),
                    NewToolCache = maps:put(ToolName, {AppSlug, OriginalName, Tool}, ToolCache),
                    
                    % Register with agent_tools
                    case agent_tools:register_tool(ToolName, Tool) of
                        ok ->
                            % Register executor function
                            ExecutorFn = fun(Name, Args) -> 
                                execute_pipedream_tool(ExternalUserId, Name, Args)
                            end,
                            agent_tools:register_executor(ToolName, ExecutorFn, #{source => pipedream}),
                            {ok, ToolName, NewToolCache};
                        Error ->
                            {error, ToolName, Error}
                    end
                end, FormattedTools),
                
                % Process results
                {SuccessTools, ToolCache} = lists:foldl(fun
                    ({ok, ToolName, Cache}, {Acc, CacheAcc}) ->
                        {[ToolName | Acc], maps:merge(CacheAcc, Cache)};
                    ({error, ToolName, Reason}, {Acc, CacheAcc}) ->
                        ?LOG_ERROR("[PIPEDREAM] Failed to register tool ~s: ~p", [ToolName, Reason]),
                        {Acc, CacheAcc}
                end, {[], State#state.tool_cache}, RegistrationResults),
                
                % Update user tools
                NewUserTools = maps:put(ExternalUserId, FormattedTools, State#state.user_tools),
                
                % Update stats
                NewStats = maps:merge(State#state.stats, #{
                    tools_registered => maps:get(tools_registered, State#state.stats, 0) + length(SuccessTools),
                    last_sync => erlang:system_time(second)
                }),
                
                NewState = State#state{
                    user_tools = NewUserTools,
                    tool_cache = ToolCache,
                    stats = NewStats
                },
                
                {ok, NewState};
                
            {error, Reason} ->
                {error, Reason}
        end
    catch
        ErrorType:ErrorReason ->
            {error, {ErrorType, ErrorReason}}
    end.

format_pipedream_tools(PipedreamTools) ->
    lists:map(fun(Tool) ->
        % Convert Pipedream tool format to agent_tools format
        #{
            <<"type">> => <<"function">>,
            <<"name">> => maps:get(<<"name">>, Tool),
            <<"description">> => maps:get(<<"description">>, Tool, <<"Pipedream tool">>),
            <<"parameters">> => format_parameters(maps:get(<<"parameters">>, Tool, #{})),
            <<"source">> => <<"pipedream">>,
            <<"app_slug">> => maps:get(<<"app_slug">>, Tool),
            <<"original_name">> => maps:get(<<"original_name">>, Tool),
            <<"strict">> => true
        }
    end, PipedreamTools).

format_parameters(Parameters) when is_map(Parameters) ->
    % Ensure parameters follow OpenAI function calling format
    Parameters#{
        <<"additionalProperties">> => false
    };
format_parameters(_) ->
    #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{},
        <<"additionalProperties">> => false
    }.

execute_tool_via_mcp(ExternalUserId, AppSlug, ToolName, Arguments) ->
    try
        case pipedream_mcp_client:call_tool(ExternalUserId, AppSlug, ToolName, Arguments) of
            {ok, Result} ->
                {ok, format_tool_result(Result)};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        ErrorType:ErrorReason ->
            {error, {ErrorType, ErrorReason}}
    end.

format_tool_result(Result) when is_map(Result) ->
    % Format result for agent consumption
    case maps:get(<<"content">>, Result, undefined) of
        undefined ->
            Result;
        Content when is_list(Content) ->
            % Handle multiple content blocks
            FormattedContent = lists:map(fun(Block) ->
                case Block of
                    #{<<"type">> := <<"text">>, <<"text">> := Text} ->
                        Text;
                    _ ->
                        jsx:encode(Block)
                end
            end, Content),
            string:join(FormattedContent, "\n");
        Content ->
            Content
    end;
format_tool_result(Result) ->
    Result.

format_tools_for_openai(Tools) ->
    % Convert tools to OpenAI function calling format
    lists:map(fun(Tool) ->
        #{
            type => function,
            function => #{
                name => binary_to_list(maps:get(<<"name">>, Tool)),
                description => binary_to_list(maps:get(<<"description">>, Tool, <<"Pipedream tool">>)),
                parameters => maps:get(<<"parameters">>, Tool, #{})
            }
        }
    end, Tools).

sync_all_users(State) ->
    Users = maps:keys(State#state.user_tools),
    lists:foldl(fun(UserId, AccState) ->
        case fetch_and_register_user_tools(UserId, AccState) of
            {ok, NewState} ->
                NewState;
            {error, _Reason} ->
                AccState
        end
    end, State, Users).

%% Public utility functions for integration

get_pipedream_tools_for_user(ExternalUserId) ->
    case gen_server:call(?SERVER, {get_user_tools, ExternalUserId}) of
        {ok, Tools} ->
            format_tools_for_openai(Tools);
        {error, _} ->
            []
    end.

is_pipedream_tool(ToolName) ->
    case gen_server:call(?SERVER, {is_pipedream_tool, ToolName}) of
        {ok, true} -> true;
        _ -> false
    end.

refresh_user_tools(ExternalUserId) ->
    gen_server:cast(?SERVER, {sync_user_tools, ExternalUserId}).

%% Integration with existing agent_tools module
%% This can be called from agent_tools:get_enhanced_tools/1

get_enhanced_tools_for_user(ExternalUserId, RequestedTools) ->
    UserTools = get_pipedream_tools_for_user(ExternalUserId),
    
    % Filter to only requested tools if specified
    case RequestedTools of
        all ->
            UserTools;
        ToolNames when is_list(ToolNames) ->
            lists:filter(fun(#{function := #{name := Name}}) ->
                lists:member(Name, ToolNames)
            end, UserTools);
        _ ->
            []
    end.