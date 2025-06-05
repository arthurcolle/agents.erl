-module(pipedream_autodiscovery).
-behaviour(gen_server).

-export([
    start_link/0,
    get_discovered_apps/0,
    get_app_tools/1,
    refresh_discovery/0,
    get_user_tools/1,
    register_user_apps/2,
    get_discovery_stats/0
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    discovered_apps = #{},
    app_tools = #{},
    user_apps = #{},
    last_discovery = 0,
    discovery_interval = 3600000, % 1 hour
    stats = #{
        total_apps => 0,
        total_tools => 0,
        last_update => 0,
        discovery_errors => 0
    }
}).

-define(SERVER, ?MODULE).
-define(ETS_APPS, pipedream_apps).
-define(ETS_TOOLS, pipedream_tools).
-define(ETS_USER_APPS, pipedream_user_apps).

%% API Functions

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_discovered_apps() ->
    gen_server:call(?SERVER, get_discovered_apps).

get_app_tools(AppSlug) ->
    gen_server:call(?SERVER, {get_app_tools, AppSlug}).

refresh_discovery() ->
    gen_server:cast(?SERVER, refresh_discovery).

get_user_tools(ExternalUserId) ->
    gen_server:call(?SERVER, {get_user_tools, ExternalUserId}).

register_user_apps(ExternalUserId, AppSlugs) ->
    gen_server:call(?SERVER, {register_user_apps, ExternalUserId, AppSlugs}).

get_discovery_stats() ->
    gen_server:call(?SERVER, get_discovery_stats).

%% Gen Server Callbacks

init([]) ->
    ets:new(?ETS_APPS, [named_table, public, set, {read_concurrency, true}]),
    ets:new(?ETS_TOOLS, [named_table, public, set, {read_concurrency, true}]),
    ets:new(?ETS_USER_APPS, [named_table, public, bag, {read_concurrency, true}]),
    
    State = #state{},
    
    % Start initial discovery
    self() ! initial_discovery,
    
    % Schedule periodic discovery
    schedule_discovery(State#state.discovery_interval),
    
    {ok, State}.

handle_call(get_discovered_apps, _From, State) ->
    Apps = ets:tab2list(?ETS_APPS),
    {reply, {ok, Apps}, State};

handle_call({get_app_tools, AppSlug}, _From, State) ->
    case ets:lookup(?ETS_TOOLS, AppSlug) of
        [{AppSlug, Tools}] ->
            {reply, {ok, Tools}, State};
        [] ->
            {reply, {error, app_not_found}, State}
    end;

handle_call({get_user_tools, ExternalUserId}, _From, State) ->
    UserApps = ets:lookup(?ETS_USER_APPS, ExternalUserId),
    AllTools = lists:foldl(fun({_, AppSlug}, Acc) ->
        case ets:lookup(?ETS_TOOLS, AppSlug) of
            [{AppSlug, Tools}] ->
                Acc ++ Tools;
            [] ->
                Acc
        end
    end, [], UserApps),
    {reply, {ok, AllTools}, State};

handle_call({register_user_apps, ExternalUserId, AppSlugs}, _From, State) ->
    % Clear existing user apps
    ets:match_delete(?ETS_USER_APPS, {ExternalUserId, '_'}),
    
    % Add new user apps
    lists:foreach(fun(AppSlug) ->
        ets:insert(?ETS_USER_APPS, {ExternalUserId, AppSlug})
    end, AppSlugs),
    
    {reply, ok, State};

handle_call(get_discovery_stats, _From, #state{stats = Stats} = State) ->
    {reply, {ok, Stats}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(refresh_discovery, State) ->
    self() ! discovery_refresh,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(initial_discovery, State) ->
    NewState = perform_discovery(State),
    {noreply, NewState};

handle_info(discovery_refresh, State) ->
    NewState = perform_discovery(State),
    schedule_discovery(State#state.discovery_interval),
    {noreply, NewState};

handle_info(discovery_timer, State) ->
    NewState = perform_discovery(State),
    schedule_discovery(State#state.discovery_interval),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ets:delete(?ETS_APPS),
    ets:delete(?ETS_TOOLS),
    ets:delete(?ETS_USER_APPS),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal Functions

schedule_discovery(Interval) ->
    erlang:send_after(Interval, self(), discovery_timer).

perform_discovery(State) ->
    logger:info("Starting Pipedream app discovery"),
    
    StartTime = erlang:system_time(millisecond),
    
    case pipedream_mcp_client:get_available_apps() of
        {ok, Apps} ->
            logger:info("Discovered ~p Pipedream apps", [length(Apps)]),
            
            % Store apps in ETS
            ets:delete_all_objects(?ETS_APPS),
            lists:foreach(fun(App) ->
                AppSlug = maps:get(<<"name_slug">>, App),
                ets:insert(?ETS_APPS, {AppSlug, App})
            end, Apps),
            
            % Discover tools for popular apps (limit to prevent rate limiting)
            PopularApps = get_popular_apps(Apps),
            {TotalTools, Errors} = discover_tools_for_apps(PopularApps),
            
            NewStats = #{
                total_apps => length(Apps),
                total_tools => TotalTools,
                last_update => StartTime,
                discovery_errors => Errors,
                discovery_duration => erlang:system_time(millisecond) - StartTime
            },
            
            logger:info("Discovery completed: ~p apps, ~p tools, ~p errors", 
                       [length(Apps), TotalTools, Errors]),
            
            State#state{
                last_discovery = StartTime,
                stats = NewStats
            };
            
        {error, Reason} ->
            logger:error("Failed to discover Pipedream apps: ~p", [Reason]),
            
            NewStats = maps:put(discovery_errors, 
                               maps:get(discovery_errors, State#state.stats, 0) + 1,
                               State#state.stats),
            
            State#state{stats = NewStats}
    end.

get_popular_apps(Apps) ->
    % Filter to most commonly used apps to avoid overwhelming the system
    PopularSlugs = [
        <<"gmail">>, <<"slack">>, <<"notion">>, <<"google_sheets">>, <<"github">>,
        <<"discord">>, <<"trello">>, <<"airtable">>, <<"linear">>, <<"asana">>,
        <<"calendly">>, <<"dropbox">>, <<"hubspot">>, <<"salesforce">>, <<"stripe">>,
        <<"mailchimp">>, <<"shopify">>, <<"zoom">>, <<"microsoft_teams">>, <<"jira">>
    ],
    
    lists:filter(fun(App) ->
        AppSlug = maps:get(<<"name_slug">>, App),
        lists:member(AppSlug, PopularSlugs)
    end, Apps).

discover_tools_for_apps(Apps) ->
    discover_tools_for_apps(Apps, 0, 0).

discover_tools_for_apps([], TotalTools, Errors) ->
    {TotalTools, Errors};

discover_tools_for_apps([App | Rest], TotalTools, Errors) ->
    AppSlug = binary_to_list(maps:get(<<"name_slug">>, App)),
    
    % Use a dummy user ID for tool discovery
    case pipedream_mcp_client:discover_tools("autodiscovery_user", AppSlug) of
        {ok, Tools} ->
            ets:insert(?ETS_TOOLS, {maps:get(<<"name_slug">>, App), Tools}),
            discover_tools_for_apps(Rest, TotalTools + length(Tools), Errors);
            
        {error, Reason} ->
            logger:warning("Failed to discover tools for ~s: ~p", [AppSlug, Reason]),
            discover_tools_for_apps(Rest, TotalTools, Errors + 1)
    end.

%% Public utility functions for integration

get_app_by_slug(AppSlug) ->
    case ets:lookup(?ETS_APPS, AppSlug) of
        [{AppSlug, App}] ->
            {ok, App};
        [] ->
            {error, not_found}
    end.

search_apps(Query) ->
    Apps = ets:tab2list(?ETS_APPS),
    QueryLower = string:lowercase(Query),
    
    MatchingApps = lists:filter(fun({_Slug, App}) ->
        Name = string:lowercase(binary_to_list(maps:get(<<"name">>, App, <<"">>))),
        Description = string:lowercase(binary_to_list(maps:get(<<"description">>, App, <<"">>))),
        
        string:str(Name, QueryLower) > 0 orelse 
        string:str(Description, QueryLower) > 0
    end, Apps),
    
    lists:map(fun({_Slug, App}) -> App end, MatchingApps).

get_tools_by_category(Category) ->
    Tools = ets:tab2list(?ETS_TOOLS),
    lists:foldl(fun({_AppSlug, AppTools}, Acc) ->
        CategoryTools = lists:filter(fun(Tool) ->
            case maps:get(<<"category">>, Tool, undefined) of
                undefined -> false;
                ToolCategory -> 
                    string:lowercase(binary_to_list(ToolCategory)) =:= 
                    string:lowercase(Category)
            end
        end, AppTools),
        Acc ++ CategoryTools
    end, [], Tools).