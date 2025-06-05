-module(pipedream_auth_handler).
-behaviour(gen_server).

-export([
    start_link/0,
    generate_connection_url/2,
    verify_connection/2,
    get_user_connections/1,
    disconnect_user_app/2,
    refresh_user_connections/1,
    handle_oauth_callback/3,
    get_connection_stats/0
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    connections = #{}, % external_user_id => #{app_slug => connection_data}
    pending_auth = #{}, % auth_token => {external_user_id, app_slug, timestamp}
    stats = #{
        total_connections => 0,
        active_users => 0,
        auth_attempts => 0,
        successful_auths => 0
    }
}).

-define(SERVER, ?MODULE).
-define(ETS_CONNECTIONS, pipedream_connections).
-define(AUTH_TOKEN_EXPIRY, 600000). % 10 minutes

%% API Functions

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

generate_connection_url(ExternalUserId, AppSlug) ->
    gen_server:call(?SERVER, {generate_connection_url, ExternalUserId, AppSlug}).

verify_connection(ExternalUserId, AppSlug) ->
    gen_server:call(?SERVER, {verify_connection, ExternalUserId, AppSlug}).

get_user_connections(ExternalUserId) ->
    gen_server:call(?SERVER, {get_user_connections, ExternalUserId}).

disconnect_user_app(ExternalUserId, AppSlug) ->
    gen_server:call(?SERVER, {disconnect_user_app, ExternalUserId, AppSlug}).

refresh_user_connections(ExternalUserId) ->
    gen_server:call(?SERVER, {refresh_user_connections, ExternalUserId}).

handle_oauth_callback(AuthToken, Code, State) ->
    gen_server:call(?SERVER, {oauth_callback, AuthToken, Code, State}).

get_connection_stats() ->
    gen_server:call(?SERVER, get_connection_stats).

%% Gen Server Callbacks

init([]) ->
    ets:new(?ETS_CONNECTIONS, [named_table, public, set, {read_concurrency, true}]),
    
    % Schedule cleanup of expired auth tokens
    schedule_cleanup(),
    
    {ok, #state{}}.

handle_call({generate_connection_url, ExternalUserId, AppSlug}, _From, State) ->
    case pipedream_mcp_client:connect_user_account(ExternalUserId, AppSlug) of
        {ok, URL} ->
            AuthToken = generate_auth_token(),
            PendingAuth = maps:put(AuthToken, 
                                  {ExternalUserId, AppSlug, erlang:system_time(millisecond)},
                                  State#state.pending_auth),
            
            % Add auth token to URL
            EnhancedURL = URL ++ "&auth_token=" ++ AuthToken,
            
            NewStats = maps:put(auth_attempts, 
                               maps:get(auth_attempts, State#state.stats, 0) + 1,
                               State#state.stats),
            
            {reply, {ok, EnhancedURL}, State#state{
                pending_auth = PendingAuth,
                stats = NewStats
            }};
        Error ->
            {reply, Error, State}
    end;

handle_call({verify_connection, ExternalUserId, AppSlug}, _From, State) ->
    case pipedream_mcp_client:get_user_connection_status(ExternalUserId, AppSlug) of
        {ok, connected} ->
            % Store connection in ETS
            ConnectionKey = {ExternalUserId, AppSlug},
            ConnectionData = #{
                app_slug => AppSlug,
                connected_at => erlang:system_time(second),
                status => connected,
                last_verified => erlang:system_time(second)
            },
            ets:insert(?ETS_CONNECTIONS, {ConnectionKey, ConnectionData}),
            
            % Update state
            UserConnections = maps:get(ExternalUserId, State#state.connections, #{}),
            NewUserConnections = maps:put(AppSlug, ConnectionData, UserConnections),
            NewConnections = maps:put(ExternalUserId, NewUserConnections, State#state.connections),
            
            % Update stats
            NewStats = update_connection_stats(State#state.stats, connected),
            
            {reply, {ok, connected}, State#state{
                connections = NewConnections,
                stats = NewStats
            }};
        {ok, not_connected} ->
            {reply, {ok, not_connected}, State};
        Error ->
            {reply, Error, State}
    end;

handle_call({get_user_connections, ExternalUserId}, _From, State) ->
    Connections = case ets:match(?ETS_CONNECTIONS, {{ExternalUserId, '$1'}, '$2'}) of
        [] ->
            #{};
        Matches ->
            maps:from_list([{AppSlug, Data} || [AppSlug, Data] <- Matches])
    end,
    {reply, {ok, Connections}, State};

handle_call({disconnect_user_app, ExternalUserId, AppSlug}, _From, State) ->
    ConnectionKey = {ExternalUserId, AppSlug},
    ets:delete(?ETS_CONNECTIONS, ConnectionKey),
    
    % Update state
    UserConnections = maps:get(ExternalUserId, State#state.connections, #{}),
    NewUserConnections = maps:remove(AppSlug, UserConnections),
    NewConnections = case maps:size(NewUserConnections) of
        0 -> maps:remove(ExternalUserId, State#state.connections);
        _ -> maps:put(ExternalUserId, NewUserConnections, State#state.connections)
    end,
    
    % Update stats
    NewStats = update_connection_stats(State#state.stats, disconnected),
    
    {reply, ok, State#state{
        connections = NewConnections,
        stats = NewStats
    }};

handle_call({refresh_user_connections, ExternalUserId}, _From, State) ->
    % Get all apps the user has registered for
    case pipedream_autodiscovery:get_user_tools(ExternalUserId) of
        {ok, Tools} ->
            AppSlugs = lists:usort([maps:get(<<"app_slug">>, Tool) || Tool <- Tools]),
            RefreshResults = lists:map(fun(AppSlug) ->
                AppSlugStr = binary_to_list(AppSlug),
                case pipedream_mcp_client:get_user_connection_status(ExternalUserId, AppSlugStr) of
                    {ok, Status} ->
                        {AppSlugStr, Status};
                    {error, Reason} ->
                        {AppSlugStr, {error, Reason}}
                end
            end, AppSlugs),
            
            {reply, {ok, RefreshResults}, State};
        Error ->
            {reply, Error, State}
    end;

handle_call({oauth_callback, AuthToken, Code, OAuthState}, _From, State) ->
    case maps:get(AuthToken, State#state.pending_auth, undefined) of
        undefined ->
            {reply, {error, invalid_auth_token}, State};
        {ExternalUserId, AppSlug, Timestamp} ->
            Now = erlang:system_time(millisecond),
            if
                Now - Timestamp > ?AUTH_TOKEN_EXPIRY ->
                    NewPendingAuth = maps:remove(AuthToken, State#state.pending_auth),
                    {reply, {error, auth_token_expired}, State#state{pending_auth = NewPendingAuth}};
                true ->
                    % Process OAuth callback
                    case process_oauth_callback(ExternalUserId, AppSlug, Code, OAuthState) of
                        {ok, ConnectionData} ->
                            % Store successful connection
                            ConnectionKey = {ExternalUserId, AppSlug},
                            ets:insert(?ETS_CONNECTIONS, {ConnectionKey, ConnectionData}),
                            
                            % Update state
                            UserConnections = maps:get(ExternalUserId, State#state.connections, #{}),
                            NewUserConnections = maps:put(AppSlug, ConnectionData, UserConnections),
                            NewConnections = maps:put(ExternalUserId, NewUserConnections, State#state.connections),
                            NewPendingAuth = maps:remove(AuthToken, State#state.pending_auth),
                            
                            % Update stats
                            NewStats = maps:put(successful_auths,
                                              maps:get(successful_auths, State#state.stats, 0) + 1,
                                              update_connection_stats(State#state.stats, connected)),
                            
                            {reply, {ok, connected}, State#state{
                                connections = NewConnections,
                                pending_auth = NewPendingAuth,
                                stats = NewStats
                            }};
                        Error ->
                            NewPendingAuth = maps:remove(AuthToken, State#state.pending_auth),
                            {reply, Error, State#state{pending_auth = NewPendingAuth}}
                    end
            end
    end;

handle_call(get_connection_stats, _From, #state{stats = Stats} = State) ->
    % Calculate real-time stats
    TotalConnections = ets:info(?ETS_CONNECTIONS, size),
    ActiveUsers = length(lists:usort([UserId || {{UserId, _}, _} <- ets:tab2list(?ETS_CONNECTIONS)])),
    
    EnhancedStats = maps:merge(Stats, #{
        total_connections => TotalConnections,
        active_users => ActiveUsers,
        timestamp => erlang:system_time(second)
    }),
    
    {reply, {ok, EnhancedStats}, State#state{stats = EnhancedStats}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup_expired_tokens, State) ->
    Now = erlang:system_time(millisecond),
    NewPendingAuth = maps:filter(fun(_, {_, _, Timestamp}) ->
        Now - Timestamp < ?AUTH_TOKEN_EXPIRY
    end, State#state.pending_auth),
    
    schedule_cleanup(),
    {noreply, State#state{pending_auth = NewPendingAuth}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ets:delete(?ETS_CONNECTIONS),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal Functions

generate_auth_token() ->
    base64:encode(crypto:strong_rand_bytes(32)).

schedule_cleanup() ->
    erlang:send_after(300000, self(), cleanup_expired_tokens). % 5 minutes

process_oauth_callback(ExternalUserId, AppSlug, Code, OAuthState) ->
    % This would typically involve exchanging the code for an access token
    % with Pipedream's OAuth endpoint, but for now we'll simulate success
    ConnectionData = #{
        app_slug => AppSlug,
        connected_at => erlang:system_time(second),
        status => connected,
        oauth_code => Code,
        oauth_state => OAuthState,
        last_verified => erlang:system_time(second)
    },
    {ok, ConnectionData}.

update_connection_stats(Stats, connected) ->
    maps:put(total_connections, maps:get(total_connections, Stats, 0) + 1, Stats);
update_connection_stats(Stats, disconnected) ->
    Current = maps:get(total_connections, Stats, 0),
    maps:put(total_connections, max(0, Current - 1), Stats);
update_connection_stats(Stats, _) ->
    Stats.

%% Public utility functions

get_connection_status(ExternalUserId, AppSlug) ->
    ConnectionKey = {ExternalUserId, AppSlug},
    case ets:lookup(?ETS_CONNECTIONS, ConnectionKey) of
        [{ConnectionKey, #{status := Status}}] ->
            {ok, Status};
        [] ->
            {ok, not_connected}
    end.

list_connected_apps(ExternalUserId) ->
    Connections = ets:match(?ETS_CONNECTIONS, {{ExternalUserId, '$1'}, '$2'}),
    ConnectedApps = [{AppSlug, Data} || [AppSlug, Data] <- Connections],
    {ok, ConnectedApps}.

bulk_verify_connections(ExternalUserId, AppSlugs) ->
    Results = lists:map(fun(AppSlug) ->
        case pipedream_mcp_client:get_user_connection_status(ExternalUserId, AppSlug) of
            {ok, Status} ->
                {AppSlug, Status};
            {error, Reason} ->
                {AppSlug, {error, Reason}}
        end
    end, AppSlugs),
    {ok, Results}.