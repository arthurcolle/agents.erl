-module(mcp_github_registry_updater).
-behaviour(gen_server).

%% API
-export([start_link/0, force_update/0, get_last_update/0, get_registry_stats/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Enhanced colorful logging
-define(LOG(Level, Format, Args), 
    colored_logger:log(Level, "MCP_GITHUB", "[~s:~p] " ++ Format, [?MODULE, ?LINE | Args])).

-define(LOG_INFO(Format, Args), ?LOG(info, Format, Args)).
-define(LOG_WARN(Format, Args), ?LOG(warning, Format, Args)).
-define(LOG_ERROR(Format, Args), ?LOG(error, Format, Args)).
-define(LOG_DEBUG(Format, Args), ?LOG(debug, Format, Args)).
-define(LOG_SUCCESS(Format, Args), ?LOG(success, Format, Args)).
-define(LOG_NETWORK(Format, Args), colored_logger:network(connected, io_lib:format("[~s:~p] " ++ Format, [?MODULE, ?LINE | Args]))).
-define(LOG_COSMIC(Format, Args), colored_logger:cosmic(star, io_lib:format("[~s:~p] " ++ Format, [?MODULE, ?LINE | Args]))).

-record(state, {
    timer_ref,
    last_update,
    registry_data = #{},
    update_interval = 3600000  % 1 hour in milliseconds
}).

-define(GITHUB_API_URL, "https://api.github.com/repos/modelcontextprotocol/servers").
-define(GITHUB_README_URL, "https://raw.githubusercontent.com/modelcontextprotocol/servers/main/README.md").

%%====================================================================
%% API
%%====================================================================

start_link() ->
    colored_logger:startup("Starting MCP GitHub registry updater", []),
    case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
        {ok, Pid} ->
            ?LOG_SUCCESS("GitHub registry updater started with PID ~p", [Pid]),
            {ok, Pid};
        {error, Reason} ->
            ?LOG_ERROR("Failed to start GitHub registry updater: ~p", [Reason]),
            {error, Reason}
    end.

force_update() ->
    gen_server:call(?MODULE, force_update).

get_last_update() ->
    gen_server:call(?MODULE, get_last_update).

get_registry_stats() ->
    gen_server:call(?MODULE, get_registry_stats).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    ?LOG_INFO("Initializing GitHub registry updater", []),
    
    % Schedule immediate first update
    self() ! update_registry,
    
    % Schedule periodic updates
    TimerRef = erlang:send_after(3600000, self(), update_registry),
    
    {ok, #state{timer_ref = TimerRef}}.

handle_call(force_update, _From, State) ->
    ?LOG_INFO("Force updating MCP registry from GitHub", []),
    self() ! update_registry,
    {reply, ok, State};

handle_call(get_last_update, _From, State) ->
    {reply, State#state.last_update, State};

handle_call(get_registry_stats, _From, State) ->
    Stats = #{
        last_update => State#state.last_update,
        total_servers => maps:size(State#state.registry_data),
        update_interval => State#state.update_interval,
        registry_data => State#state.registry_data
    },
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, unknown_request, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(update_registry, State) ->
    ?LOG_COSMIC("🌟 Starting scheduled MCP registry update from GitHub", []),
    
    NewState = case update_mcp_registry() of
        {ok, RegistryData} ->
            ?LOG_SUCCESS("Successfully updated MCP registry with ~p servers", [maps:size(RegistryData)]),
            
            % Broadcast update to websocket clients
            try
                agent_ws_handler:broadcast(#{
                    type => <<"mcp_registry_updated">>,
                    timestamp => erlang:system_time(second),
                    server_count => maps:size(RegistryData)
                }),
                ?LOG_NETWORK("📡 Broadcast registry update to websocket clients", [])
            catch
                Class:Error ->
                    ?LOG_WARN("Failed to broadcast registry update: ~p:~p", [Class, Error])
            end,
            
            State#state{
                registry_data = RegistryData,
                last_update = erlang:system_time(second)
            };
        {error, Reason} ->
            ?LOG_ERROR("Failed to update MCP registry: ~p", [Reason]),
            State
    end,
    
    % Schedule next update
    NewTimerRef = erlang:send_after(State#state.update_interval, self(), update_registry),
    FinalState = NewState#state{timer_ref = NewTimerRef},
    
    {noreply, FinalState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    case State#state.timer_ref of
        undefined -> ok;
        TimerRef -> erlang:cancel_timer(TimerRef)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

update_mcp_registry() ->
    ?LOG_INFO("Fetching MCP servers from GitHub repository", []),
    
    case fetch_github_readme() of
        {ok, ReadmeContent} ->
            ?LOG_DEBUG("Successfully fetched README content (~p bytes)", [byte_size(ReadmeContent)]),
            parse_mcp_servers(ReadmeContent);
        {error, Reason} ->
            ?LOG_ERROR("Failed to fetch GitHub README: ~p", [Reason]),
            {error, {fetch_failed, Reason}}
    end.

fetch_github_readme() ->
    ?LOG_NETWORK("🌐 Fetching README from GitHub: ~s", [?GITHUB_README_URL]),
    
    case httpc:request(get, {?GITHUB_README_URL, []}, 
                      [{timeout, 30000}, {connect_timeout, 10000}], 
                      [{body_format, binary}]) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
            ?LOG_SUCCESS("Successfully fetched README (~p bytes)", [byte_size(Body)]),
            {ok, Body};
        {ok, {{_Version, StatusCode, ReasonPhrase}, _Headers, _Body}} ->
            ?LOG_ERROR("GitHub API returned error: ~p ~s", [StatusCode, ReasonPhrase]),
            {error, {http_error, StatusCode, ReasonPhrase}};
        {error, Reason} ->
            ?LOG_ERROR("HTTP request failed: ~p", [Reason]),
            {error, {http_request_failed, Reason}}
    end.

parse_mcp_servers(ReadmeContent) ->
    ?LOG_INFO("Parsing MCP servers from README content", []),
    
    try
        % Convert binary to string for regex processing
        Content = binary_to_list(ReadmeContent),
        
        % Parse different sections
        ReferenceServers = parse_reference_servers(Content),
        OfficialIntegrations = parse_official_integrations(Content),
        CommunityServers = parse_community_servers(Content),
        
        % Combine all servers
        AllServers = maps:merge(maps:merge(ReferenceServers, OfficialIntegrations), CommunityServers),
        
        ?LOG_SUCCESS("Parsed ~p total MCP servers from GitHub", [maps:size(AllServers)]),
        ?LOG_DEBUG("Reference: ~p, Official: ~p, Community: ~p", 
                  [maps:size(ReferenceServers), maps:size(OfficialIntegrations), maps:size(CommunityServers)]),
        
        {ok, AllServers}
    catch
        Class:Error:Stack ->
            ?LOG_ERROR("Failed to parse README content: ~p:~p~nStack: ~p", [Class, Error, Stack]),
            {error, {parse_failed, Error}}
    end.

parse_reference_servers(Content) ->
    ?LOG_DEBUG("Parsing reference servers section", []),
    Section = extract_section(Content, "🌟 Reference Servers", "Archived"),
    parse_server_list(Section, reference).

parse_official_integrations(Content) ->
    ?LOG_DEBUG("Parsing official integrations section", []),
    Section = extract_section(Content, "🎖️ Official Integrations", "🌎 Community Servers"),
    parse_server_list(Section, official).

parse_community_servers(Content) ->
    ?LOG_DEBUG("Parsing community servers section", []),
    Section = extract_section(Content, "🌎 Community Servers", "📚 Frameworks"),
    parse_server_list(Section, community).

extract_section(Content, StartMarker, EndMarker) ->
    case {string:str(Content, StartMarker), string:str(Content, EndMarker)} of
        {0, _} -> "";
        {Start, 0} -> string:substr(Content, Start);
        {Start, End} when End > Start -> string:substr(Content, Start, End - Start);
        _ -> ""
    end.

parse_server_list(Section, Category) ->
    ?LOG_DEBUG("Parsing server list for category: ~p", [Category]),
    
    % Regex to match server entries with various patterns
    Patterns = [
        % Pattern: [Name](url) - Description
        "\\[([^\\]]+)\\]\\(([^\\)]+)\\)\\s*-\\s*([^\n]+)",
        % Pattern: **Name** - Description with possible link
        "\\*\\*([^\\*]+)\\*\\*\\s*-\\s*([^\n]+)",
        % Pattern: Name Logo Name - Description
        "([A-Za-z0-9\\s\\.]+)\\sLogo\\s+([^\\s]+)\\s*-\\s*([^\n]+)"
    ],
    
    lists:foldl(fun(Pattern, Acc) ->
        case re:run(Section, Pattern, [global, {capture, all, list}]) of
            {match, Matches} ->
                ?LOG_DEBUG("Found ~p matches with pattern: ~s", [length(Matches), Pattern]),
                lists:foldl(fun(Match, AccInner) ->
                    case extract_server_info(Match, Category) of
                        {ok, ServerInfo} ->
                            ServerId = generate_server_id(ServerInfo),
                            maps:put(ServerId, ServerInfo, AccInner);
                        {error, Reason} ->
                            ?LOG_WARN("Failed to extract server info: ~p", [Reason]),
                            AccInner
                    end
                end, Acc, Matches);
            nomatch ->
                Acc
        end
    end, #{}, Patterns).

extract_server_info([_Full, Name, Url, Description], Category) ->
    {ok, #{
        name => string:strip(Name),
        url => string:strip(Url),
        description => string:strip(Description),
        category => Category,
        parsed_at => erlang:system_time(second)
    }};
extract_server_info([_Full, Name, Description], Category) ->
    {ok, #{
        name => string:strip(Name),
        url => "",
        description => string:strip(Description),
        category => Category,
        parsed_at => erlang:system_time(second)
    }};
extract_server_info(Match, _Category) ->
    ?LOG_WARN("Unexpected match format: ~p", [Match]),
    {error, unexpected_format}.

generate_server_id(#{name := Name, category := Category}) ->
    SafeName = re:replace(string:to_lower(Name), "[^a-z0-9]", "_", [global, {return, list}]),
    lists:flatten(io_lib:format("~s_~s", [atom_to_list(Category), SafeName])).