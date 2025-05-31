-module(mcp_manager).
-behaviour(gen_server).

%% Comprehensive MCP manager handling both local servers and remote clients
%% This is the central coordinator for all MCP operations

-export([start_link/0, 
         % Server management
         start_local_server/1, stop_local_server/1, list_local_servers/0,
         register_tool/3, register_resource/3, register_prompt/3,
         % Client management  
         connect_remote_server/2, disconnect_remote_server/1, list_remote_servers/0,
         % Discovery
         discover_servers/0, auto_connect/1,
         % Operations
         execute_tool/3, read_resource/2, get_prompt/2,
         % Status
         get_status/0, get_metrics/0,
         % Discovery management
         add_discovery_filter/1, remove_discovery_filter/1, clear_discovery_cache/0,
         get_discovery_cache/0, set_discovery_options/1,
         % Circuit breaker
         get_circuit_breaker_status/1, reset_circuit_breaker/1,
         % Inspector compatibility
         export_server_config/1, export_servers_file/0, set_inspector_config/1,
         get_inspector_config/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    local_servers = #{}, % ServerId -> {Pid, Config}
    remote_clients = #{}, % ServerId -> {Pid, Config}
    discovery_timer,
    auto_connect_enabled = true,
    discovery_cache = #{}, % URL -> {ServerInfo, LastChecked, Status}
    discovery_filters = [], % List of filter functions
    failed_servers = #{}, % URL -> {FailCount, LastFailed, BackoffUntil}
    circuit_breakers = #{}, % ServerId -> {State, FailureCount, LastFailure}
    metrics = #{
        discovery_attempts => 0,
        successful_connections => 0,
        failed_connections => 0,
        filtered_servers => 0
    },
    inspector_config = #{
        mcp_server_request_timeout => 10000,
        mcp_request_timeout_reset_on_progress => true,
        mcp_request_max_total_timeout => 60000,
        mcp_proxy_full_address => <<>>,
        mcp_auto_open_enabled => true
    }
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    io:format("[MCP_MGR] Starting MCP manager~n"),
    case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
        {ok, Pid} ->
            io:format("[MCP_MGR] Manager started successfully with PID ~p~n", [Pid]),
            {ok, Pid};
        {error, Reason} ->
            io:format("[ERROR] Failed to start MCP manager: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Local server management
start_local_server(Config) ->
    gen_server:call(?MODULE, {start_local_server, Config}).

stop_local_server(ServerId) ->
    gen_server:call(?MODULE, {stop_local_server, ServerId}).

list_local_servers() ->
    gen_server:call(?MODULE, list_local_servers).

register_tool(ServerId, ToolName, ToolDef) ->
    gen_server:call(?MODULE, {register_tool, ServerId, ToolName, ToolDef}).

register_resource(ServerId, URI, ResourceDef) ->
    gen_server:call(?MODULE, {register_resource, ServerId, URI, ResourceDef}).

register_prompt(ServerId, PromptName, PromptDef) ->
    gen_server:call(?MODULE, {register_prompt, ServerId, PromptName, PromptDef}).

%% Remote client management
connect_remote_server(ServerId, Config) ->
    gen_server:call(?MODULE, {connect_remote_server, ServerId, Config}).

disconnect_remote_server(ServerId) ->
    gen_server:call(?MODULE, {disconnect_remote_server, ServerId}).

list_remote_servers() ->
    gen_server:call(?MODULE, list_remote_servers).

%% Discovery
discover_servers() ->
    gen_server:call(?MODULE, discover_servers).

auto_connect(Enabled) ->
    gen_server:call(?MODULE, {auto_connect, Enabled}).

%% Operations
execute_tool(ServerId, ToolName, Arguments) ->
    gen_server:call(?MODULE, {execute_tool, ServerId, ToolName, Arguments}, 30000).

read_resource(ServerId, URI) ->
    gen_server:call(?MODULE, {read_resource, ServerId, URI}, 30000).

get_prompt(ServerId, PromptName) ->
    gen_server:call(?MODULE, {get_prompt, ServerId, PromptName}, 30000).

get_status() ->
    gen_server:call(?MODULE, get_status).

%% Inspector compatibility functions
export_server_config(ServerId) ->
    gen_server:call(?MODULE, {export_server_config, ServerId}).

export_servers_file() ->
    gen_server:call(?MODULE, export_servers_file).

set_inspector_config(Config) ->
    gen_server:call(?MODULE, {set_inspector_config, Config}).

get_inspector_config() ->
    gen_server:call(?MODULE, get_inspector_config).

get_metrics() ->
    gen_server:call(?MODULE, get_metrics).

add_discovery_filter(FilterFun) ->
    gen_server:call(?MODULE, {add_discovery_filter, FilterFun}).

remove_discovery_filter(FilterFun) ->
    gen_server:call(?MODULE, {remove_discovery_filter, FilterFun}).

clear_discovery_cache() ->
    gen_server:call(?MODULE, clear_discovery_cache).

get_discovery_cache() ->
    gen_server:call(?MODULE, get_discovery_cache).

set_discovery_options(Options) ->
    gen_server:call(?MODULE, {set_discovery_options, Options}).

get_circuit_breaker_status(ServerId) ->
    gen_server:call(?MODULE, {get_circuit_breaker_status, ServerId}).

reset_circuit_breaker(ServerId) ->
    gen_server:call(?MODULE, {reset_circuit_breaker, ServerId}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    process_flag(trap_exit, true),
    
    % Start discovery timer - much longer interval to reduce spam
    Timer = erlang:send_after(60000, self(), discovery_tick), % Start after 1 minute
    
    % Start advanced configuration manager
    case mcp_advanced_config:start_link() of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    
    % Start OAuth manager
    case oauth_manager:start_link() of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    
    % Get dynamic port allocation
    DefaultConfig = #{
        server_id => <<"agents_main">>,
        transport => websocket
    },
    
    % Use advanced config to handle port allocation
    {ok, ConfigWithPort} = mcp_advanced_config:configure_server(DefaultConfig),
    
    % Start default local server with allocated port
    LocalPid = case mcp_server:start_link(ConfigWithPort) of
        {ok, Pid} ->
            register(agents_main_mcp_server, Pid),
            io:format("[MCP_MGR] Started default server on port ~p~n", 
                     [maps:get(port, ConfigWithPort)]),
            Pid;
        {error, StartError} ->
            io:format("[MCP_MGR] Failed to start default server: ~p~n", [StartError]),
            throw({error, {failed_to_start_default_server, StartError}})
    end,
    
    % Initialize Graphlit MCP server
    GraphlitConfig = #{
        server_id => <<"graphlit">>,
        command => <<"npx">>,
        args => [<<"-y">>, <<"graphlit-mcp-server">>],
        env => #{
            <<"GRAPHLIT_ORGANIZATION_ID">> => <<"1b591b0d-eb12-4f6d-be5a-ceb95b40e716">>,
            <<"GRAPHLIT_ENVIRONMENT_ID">> => <<"42d0e0ef-8eeb-463d-921c-ef5119541eb9">>,
            <<"GRAPHLIT_JWT_SECRET">> => <<"+Lhv6z0u6qjp/mEHVN/vH2iCkKAJ5Z2TOraf4Zit8K0=">>
        },
        transport => stdio,
        auto_start => true
    },
    
    % Initialize default discovery filters
    DefaultFilters = [
        fun(#{url := Url}) ->
            % Filter out same-host connections to avoid loops
            % Also filter out the current server ports 8080 and 8767
            case binary:match(Url, [<<"localhost:8767">>, <<"127.0.0.1:8767">>, 
                                   <<"localhost:8080">>, <<"127.0.0.1:8080">>]) of
                nomatch -> true;
                _ -> false
            end
        end,
        fun(#{type := Type}) -> Type =:= network end % Only network discovery
    ],
    
    % Try to auto-connect Graphlit server
    GraphlitServers = case auto_connect_graphlit(GraphlitConfig) of
        {ok, GraphlitPid} ->
            #{<<"graphlit">> => {GraphlitPid, GraphlitConfig}};
        {error, ConnectError} ->
            io:format("[WARNING] Failed to auto-connect Graphlit server: ~p~n", [ConnectError]),
            #{}
    end,
    
    State = #state{
        local_servers = maps:merge(#{<<"agents_main">> => {LocalPid, DefaultConfig}}, GraphlitServers),
        discovery_timer = Timer,
        discovery_filters = DefaultFilters
    },
    
    % Auto-connect all configured MCP servers after a short delay
    erlang:send_after(5000, self(), auto_connect_configured_servers),
    
    {ok, State}.

handle_call({start_local_server, Config}, _From, State) ->
    ServerId = maps:get(server_id, Config, generate_server_id()),
    ConfigWithId = Config#{server_id => ServerId},
    
    case maps:find(ServerId, State#state.local_servers) of
        {ok, _} ->
            {reply, {error, already_exists}, State};
        error ->
            case mcp_server:start_link(ConfigWithId) of
                {ok, Pid} ->
                    NewServers = maps:put(ServerId, {Pid, ConfigWithId}, State#state.local_servers),
                    {reply, {ok, ServerId}, State#state{local_servers = NewServers}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({stop_local_server, ServerId}, _From, State) ->
    case maps:find(ServerId, State#state.local_servers) of
        {ok, {Pid, _Config}} ->
            mcp_server:stop(Pid),
            NewServers = maps:remove(ServerId, State#state.local_servers),
            {reply, ok, State#state{local_servers = NewServers}};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(list_local_servers, _From, State) ->
    Servers = maps:fold(fun(ServerId, {Pid, Config}, Acc) ->
        Status = case is_process_alive(Pid) of
            true -> running;
            false -> stopped
        end,
        [#{
            id => ServerId,
            config => Config,
            status => Status,
            pid => Pid
        } | Acc]
    end, [], State#state.local_servers),
    {reply, Servers, State};

handle_call({register_tool, ServerId, ToolName, ToolDef}, _From, State) ->
    case find_local_server(ServerId, State) of
        {ok, Pid} ->
            Result = mcp_server:register_tool(Pid, ToolName, ToolDef),
            {reply, Result, State};
        error ->
            {reply, {error, server_not_found}, State}
    end;

handle_call({register_resource, ServerId, URI, ResourceDef}, _From, State) ->
    case find_local_server(ServerId, State) of
        {ok, Pid} ->
            Result = mcp_server:register_resource(Pid, URI, ResourceDef),
            {reply, Result, State};
        error ->
            {reply, {error, server_not_found}, State}
    end;

handle_call({register_prompt, ServerId, PromptName, PromptDef}, _From, State) ->
    case find_local_server(ServerId, State) of
        {ok, Pid} ->
            Result = mcp_server:register_prompt(Pid, PromptName, PromptDef),
            {reply, Result, State};
        error ->
            {reply, {error, server_not_found}, State}
    end;

handle_call({connect_remote_server, ServerId, Config}, _From, State) ->
    case maps:find(ServerId, State#state.remote_clients) of
        {ok, _} ->
            {reply, {error, already_connected}, State};
        error ->
            case mcp_connection_manager:connect_server(ServerId) of
                {ok, Pid} ->
                    NewClients = maps:put(ServerId, {Pid, Config}, State#state.remote_clients),
                    {reply, {ok, ServerId}, State#state{remote_clients = NewClients}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({disconnect_remote_server, ServerId}, _From, State) ->
    case maps:find(ServerId, State#state.remote_clients) of
        {ok, {_Pid, _Config}} ->
            mcp_connection_manager:disconnect_server(ServerId),
            NewClients = maps:remove(ServerId, State#state.remote_clients),
            {reply, ok, State#state{remote_clients = NewClients}};
        error ->
            {reply, {error, not_connected}, State}
    end;

handle_call(list_remote_servers, _From, State) ->
    RemoteServers = mcp_registry:list_servers(),
    {reply, RemoteServers, State};

handle_call(discover_servers, _From, State) ->
    DiscoveredServers = perform_discovery(),
    {reply, DiscoveredServers, State};

handle_call({auto_connect, Enabled}, _From, State) ->
    {reply, ok, State#state{auto_connect_enabled = Enabled}};

handle_call({execute_tool, ServerId, ToolName, Arguments}, _From, State) ->
    Result = case find_local_server(ServerId, State) of
        {ok, _Pid} ->
            % Local server tool execution would need to be implemented
            {error, local_tool_execution_not_implemented};
        error ->
            % Try remote server
            mcp_client:call_tool(ServerId, ToolName, Arguments)
    end,
    {reply, Result, State};

handle_call({read_resource, ServerId, URI}, _From, State) ->
    Result = case find_local_server(ServerId, State) of
        {ok, _Pid} ->
            % Local server resource reading would need to be implemented
            {error, local_resource_reading_not_implemented};
        error ->
            % Try remote server
            mcp_client:read_resource(ServerId, URI)
    end,
    {reply, Result, State};

handle_call({get_prompt, ServerId, PromptName}, _From, State) ->
    Result = case find_local_server(ServerId, State) of
        {ok, _Pid} ->
            % Local server prompt getting would need to be implemented
            {error, local_prompt_getting_not_implemented};
        error ->
            % Try remote server
            mcp_client:get_prompt(ServerId, PromptName)
    end,
    {reply, Result, State};

handle_call(get_status, _From, State) ->
    LocalCount = maps:size(State#state.local_servers),
    RemoteCount = maps:size(State#state.remote_clients),
    
    Status = #{
        local_servers => LocalCount,
        remote_clients => RemoteCount,
        auto_connect_enabled => State#state.auto_connect_enabled,
        discovery_active => State#state.discovery_timer =/= undefined
    },
    
    {reply, Status, State};

handle_call({export_server_config, ServerId}, _From, State) ->
    Result = case maps:find(ServerId, State#state.local_servers) of
        {ok, {_Pid, Config}} ->
            export_server_entry(Config);
        error ->
            case maps:find(ServerId, State#state.remote_clients) of
                {ok, {_Pid, Config}} ->
                    export_server_entry(Config);
                error ->
                    {error, server_not_found}
            end
    end,
    {reply, Result, State};

handle_call(export_servers_file, _From, State) ->
    AllServers = maps:merge(State#state.local_servers, State#state.remote_clients),
    ServersConfig = maps:fold(fun(ServerId, {_Pid, Config}, Acc) ->
        ServerEntry = export_server_entry(Config),
        Acc#{ServerId => ServerEntry}
    end, #{}, AllServers),
    
    FullConfig = #{
        <<"mcpServers">> => ServersConfig
    },
    
    {reply, {ok, jsx:encode(FullConfig)}, State};

handle_call({set_inspector_config, Config}, _From, State) ->
    NewInspectorConfig = maps:merge(State#state.inspector_config, Config),
    {reply, ok, State#state{inspector_config = NewInspectorConfig}};

handle_call(get_inspector_config, _From, State) ->
    {reply, State#state.inspector_config, State};

handle_call(get_metrics, _From, State) ->
    {reply, State#state.metrics, State};

handle_call({add_discovery_filter, FilterFun}, _From, State) ->
    NewFilters = [FilterFun | State#state.discovery_filters],
    {reply, ok, State#state{discovery_filters = NewFilters}};

handle_call({remove_discovery_filter, FilterFun}, _From, State) ->
    NewFilters = lists:delete(FilterFun, State#state.discovery_filters),
    {reply, ok, State#state{discovery_filters = NewFilters}};

handle_call(clear_discovery_cache, _From, State) ->
    {reply, ok, State#state{discovery_cache = #{}}};

handle_call(get_discovery_cache, _From, State) ->
    {reply, State#state.discovery_cache, State};

handle_call({set_discovery_options, Options}, _From, State) ->
    % Update auto_connect_enabled and other discovery options
    NewAutoConnect = maps:get(auto_connect_enabled, Options, State#state.auto_connect_enabled),
    {reply, ok, State#state{auto_connect_enabled = NewAutoConnect}};

handle_call({get_circuit_breaker_status, ServerId}, _From, State) ->
    Status = maps:get(ServerId, State#state.circuit_breakers, #{state => closed, failure_count => 0}),
    {reply, Status, State};

handle_call({reset_circuit_breaker, ServerId}, _From, State) ->
    NewCircuitBreakers = maps:remove(ServerId, State#state.circuit_breakers),
    {reply, ok, State#state{circuit_breakers = NewCircuitBreakers}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(auto_connect_configured_servers, State) ->
    % Connect to all configured MCP servers from mcp_server_config
    io:format("[MCP_MGR] Auto-connecting to configured MCP servers~n"),
    connect_all_configured_servers(),
    {noreply, State};

handle_info(discovery_tick, State) ->
    % Perform periodic discovery with caching and filtering
    if State#state.auto_connect_enabled ->
        {NewState, _DiscoveredServers} = perform_filtered_discovery_and_connect(State);
    true -> 
        NewState = State
    end,
    
    % Schedule next discovery - much longer interval to reduce spam
    Timer = erlang:send_after(300000, self(), discovery_tick), % Every 5 minutes
    {noreply, NewState#state{discovery_timer = Timer}};

handle_info({'EXIT', Pid, Reason}, State) ->
    % Handle process deaths
    case find_server_by_pid(Pid, State) of
        {local, ServerId} ->
            error_logger:warning_msg("Local MCP server ~s died: ~p~n", [ServerId, Reason]),
            NewServers = maps:remove(ServerId, State#state.local_servers),
            {noreply, State#state{local_servers = NewServers}};
        {remote, ServerId} ->
            error_logger:warning_msg("Remote MCP client ~s died: ~p~n", [ServerId, Reason]),
            NewClients = maps:remove(ServerId, State#state.remote_clients),
            {noreply, State#state{remote_clients = NewClients}};
        not_found ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    % Clean up discovery timer
    if State#state.discovery_timer =/= undefined ->
        erlang:cancel_timer(State#state.discovery_timer);
    true -> ok
    end,
    
    % Stop all local servers
    maps:fold(fun(_ServerId, {Pid, _Config}, _) ->
        catch mcp_server:stop(Pid)
    end, ok, State#state.local_servers),
    
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

generate_server_id() ->
    iolist_to_binary(io_lib:format("server_~p", [erlang:system_time(microsecond)])).

connect_all_configured_servers() ->
    % Get all servers from mcp_server_config
    try
        case mcp_server_config:get_all_servers() of
            {ok, ConfiguredServers} ->
                io:format("[MCP_MGR] Found ~p configured servers to connect~n", [length(ConfiguredServers)]),
                lists:foreach(fun(ServerConfig) ->
                    connect_configured_server(ServerConfig)
                end, ConfiguredServers);
            {error, Reason} ->
                io:format("[MCP_MGR] Failed to get configured servers: ~p~n", [Reason])
        end
    catch
        Class:Error:Stack ->
            io:format("[MCP_MGR] Error connecting configured servers: ~p:~p~n~p~n", [Class, Error, Stack])
    end.

connect_configured_server(ServerRecord) ->
    % Extract data from the #mcp_server{} record
    % Based on the record definition in mcp_server_config.erl:
    % -record(mcp_server, {id, name, category, url, auth_type, maintainer, description, capabilities, status, last_checked, metadata}).
    
    try
        ServerId = element(2, ServerRecord),    % id field
        ServerName = element(3, ServerRecord),  % name field  
        ServerUrl = element(5, ServerRecord),   % url field
        
        if ServerId =:= undefined orelse ServerUrl =:= undefined ->
            io:format("[MCP_MGR] Skipping server with missing id or url: ~p~n", [ServerRecord]);
        true ->
            io:format("[MCP_MGR] Connecting to configured server: ~s (~s)~n", [ServerName, ServerId]),
            
            % Convert record to map for registry config
            ServerConfig = #{
                id => ServerId,
                name => ServerName,
                url => ServerUrl,
                auth_type => element(6, ServerRecord),    % auth_type field
                maintainer => element(7, ServerRecord),   % maintainer field
                description => element(8, ServerRecord),  % description field
                capabilities => element(9, ServerRecord), % capabilities field
                status => element(10, ServerRecord),      % status field
                metadata => element(12, ServerRecord)     % metadata field
            },
            
            % Register server in registry first
            case mcp_registry:register_server(ServerName, ServerUrl, ServerConfig) of
                {ok, RegistryId} ->
                    % Now try to connect using the registry ID with extended timeout
                    case catch gen_server:call(mcp_connection_manager, {connect_server, RegistryId}, 15000) of
                        {ok, _Pid} ->
                            io:format("[MCP_MGR] Successfully connected to server: ~s~n", [ServerName]);
                        {error, Reason} ->
                            io:format("[MCP_MGR] Failed to connect to server ~s: ~p~n", [ServerName, Reason]);
                        {'EXIT', {timeout, _}} ->
                            io:format("[MCP_MGR] Connection to server ~s timed out, will retry in background~n", [ServerName]);
                        Other ->
                            io:format("[MCP_MGR] Unexpected response connecting to server ~s: ~p~n", [ServerName, Other])
                    end;
                {error, Reason} ->
                    io:format("[MCP_MGR] Failed to register server ~s: ~p~n", [ServerName, Reason])
            end
        end
    catch
        Class:Error:Stack ->
            io:format("[MCP_MGR] Error processing server record ~p: ~p:~p~n~p~n", [ServerRecord, Class, Error, Stack])
    end.

find_local_server(ServerId, State) ->
    case maps:find(ServerId, State#state.local_servers) of
        {ok, {Pid, _Config}} -> {ok, Pid};
        error -> error
    end.

find_server_by_pid(Pid, State) ->
    % Check local servers
    case maps:fold(fun(ServerId, {ServerPid, _}, Acc) ->
        case ServerPid of
            Pid -> {found, {local, ServerId}};
            _ -> Acc
        end
    end, not_found, State#state.local_servers) of
        {found, Result} -> Result;
        not_found ->
            % Check remote clients
            maps:fold(fun(ServerId, {ClientPid, _}, Acc) ->
                case ClientPid of
                    Pid -> {found, {remote, ServerId}};
                    _ -> Acc
                end
            end, not_found, State#state.remote_clients)
    end.

perform_discovery() ->
    % Discovery methods:
    % 1. Network scanning for WebSocket MCP servers
    % 2. Process scanning for stdio MCP servers
    % 3. Configuration file reading
    % 4. mDNS/Bonjour discovery
    
    Discovered = [],
    
    % Network discovery (placeholder)
    NetworkServers = discover_network_servers(),
    
    % Process discovery (placeholder)
    ProcessServers = discover_process_servers(),
    
    % Configuration discovery
    ConfigServers = discover_config_servers(),
    
    Discovered ++ NetworkServers ++ ProcessServers ++ ConfigServers.

discover_network_servers() ->
    % Scan common ports for MCP WebSocket servers, but only a few well-known ones
    % Reduce spam by limiting to the most common MCP ports
    CommonPorts = [8767, 3000],  % Reduced from [8767, 8768, 8769, 8080, 3000]
    Hosts = [<<"localhost">>],   % Only check localhost to avoid network scanning
    
    lists:foldl(fun(Host, Acc) ->
        lists:foldl(fun(Port, InnerAcc) ->
            Url = iolist_to_binary(io_lib:format("ws://~s:~p/mcp", [Host, Port])),
            case test_mcp_server(Url) of
                {ok, ServerInfo} ->
                    [#{
                        type => network,
                        url => Url,
                        server_info => ServerInfo
                    } | InnerAcc];
                {error, _} ->
                    InnerAcc
            end
        end, Acc, CommonPorts)
    end, [], Hosts).

discover_process_servers() ->
    % Look for running processes that might be MCP servers
    % This is a placeholder - would need more sophisticated detection
    [].

discover_config_servers() ->
    % Read from configuration files
    % This would read from ~/.mcp/servers.json or similar
    ConfigFile = filename:join([os:getenv("HOME", "/tmp"), ".mcp", "servers.json"]),
    case file:read_file(ConfigFile) of
        {ok, Data} ->
            try
                Config = jsx:decode(Data, [return_maps]),
                maps:get(<<"servers">>, Config, [])
            catch
                _:_ -> []
            end;
        {error, _} ->
            []
    end.

test_mcp_server(Url) ->
    % Quick test to see if a server responds to MCP protocol
    try
        % Extract host and port from URL
        case parse_websocket_url(Url) of
            {ok, Host, Port} ->
                % Try to connect to the port to see if anything is listening
                case gen_tcp:connect(Host, Port, [binary, {active, false}], 5000) of
                    {ok, Socket} ->
                        gen_tcp:close(Socket),
                        {ok, #{server_info => #{name => <<"Detected Server">>}}};
                    {error, _Reason} ->
                        {error, not_reachable}
                end;
            {error, invalid_url} ->
                {error, invalid_url}
        end
    catch
        _:_ ->
            {error, not_reachable}
    end.

perform_auto_discovery_and_connect() ->
    % Legacy function - redirect to new implementation
    perform_filtered_discovery_and_connect(#state{}).

perform_filtered_discovery_and_connect(State) ->
    StartTime = erlang:system_time(millisecond),
    
    % Update metrics
    UpdatedMetrics = maps:update_with(discovery_attempts, fun(X) -> X + 1 end, 1, State#state.metrics),
    StateWithMetrics = State#state{metrics = UpdatedMetrics},
    
    % Log discovery start
    catch mcp_advanced_logger:log_discovery_event(discovery_started, #{
        auto_connect_enabled => StateWithMetrics#state.auto_connect_enabled
    }),
    
    % Perform discovery with caching
    {NewCache, DiscoveredServers} = perform_cached_discovery(StateWithMetrics#state.discovery_cache),
    
    % Apply filters
    {FilteredServers, FilteredCount} = apply_discovery_filters(DiscoveredServers, StateWithMetrics#state.discovery_filters),
    
    % Log discovery results
    catch mcp_advanced_logger:log_discovery_event(discovery_completed, #{
        total_discovered => length(DiscoveredServers),
        filtered_count => FilteredCount,
        remaining_count => length(FilteredServers),
        discovery_time => erlang:system_time(millisecond) - StartTime
    }),
    
    % Update filtered servers metric
    NewMetrics = maps:update_with(filtered_servers, fun(X) -> X + FilteredCount end, FilteredCount, UpdatedMetrics),
    
    % Attempt connections with circuit breaker logic
    {FinalState, ConnectionResults} = attempt_filtered_connections(FilteredServers, 
        StateWithMetrics#state{discovery_cache = NewCache, metrics = NewMetrics}),
    
    % Log performance metric
    TotalTime = erlang:system_time(millisecond) - StartTime,
    catch mcp_advanced_logger:log_performance_metric(discovery_cycle, TotalTime, #{
        discovered_count => length(DiscoveredServers),
        filtered_count => FilteredCount,
        connection_attempts => length(FilteredServers)
    }),
    
    {FinalState, ConnectionResults}.

perform_cached_discovery(Cache) ->
    Now = erlang:system_time(second),
    CacheTimeout = 300, % 5 minutes
    
    % Check cache first
    case should_use_cache(Cache, Now, CacheTimeout) of
        {true, CachedServers} ->
            {Cache, CachedServers};
        false ->
            % Perform fresh discovery
            Discovered = perform_discovery(),
            % Update cache
            NewCache = maps:fold(fun(Url, ServerInfo, Acc) ->
                CacheEntry = #{
                    server_info => ServerInfo,
                    last_checked => Now,
                    status => available
                },
                maps:put(Url, CacheEntry, Acc)
            end, #{}, 
            maps:from_list([{maps:get(url, S, undefined), S} || S <- Discovered])),
            {NewCache, Discovered}
    end.

should_use_cache(Cache, Now, Timeout) ->
    case maps:size(Cache) of
        0 -> false;
        _ ->
            % Check if any cache entries are still valid
            ValidEntries = maps:filter(fun(_Url, #{last_checked := LastChecked}) ->
                Now - LastChecked < Timeout
            end, Cache),
            case maps:size(ValidEntries) > 0 of
                true ->
                    CachedServers = [maps:get(server_info, Entry) || Entry <- maps:values(ValidEntries)],
                    {true, CachedServers};
                false ->
                    false
            end
    end.

apply_discovery_filters(Servers, Filters) ->
    {FilteredServers, FilteredCount} = lists:foldl(fun(Server, {AccServers, AccFiltered}) ->
        case apply_filters_to_server(Server, Filters) of
            true -> {[Server | AccServers], AccFiltered};
            false -> {AccServers, AccFiltered + 1}
        end
    end, {[], 0}, Servers),
    {lists:reverse(FilteredServers), FilteredCount}.

apply_filters_to_server(Server, Filters) ->
    lists:all(fun(Filter) ->
        try
            Filter(Server)
        catch
            _:_ -> false % Filter function failed, exclude server
        end
    end, Filters).

attempt_filtered_connections(FilteredServers, State) ->
    lists:foldl(fun(ServerInfo, {AccState, AccResults}) ->
        Url = maps:get(url, ServerInfo, undefined),
        case should_attempt_connection_with_circuit_breaker(Url, AccState) of
            true ->
                case attempt_auto_connect_with_circuit_breaker(ServerInfo, AccState) of
                    {ok, NewState} ->
                        NewMetrics = maps:update_with(successful_connections, fun(X) -> X + 1 end, 1, NewState#state.metrics),
                        {NewState#state{metrics = NewMetrics}, [{Url, success} | AccResults]};
                    {error, Reason, NewState} ->
                        NewMetrics = maps:update_with(failed_connections, fun(X) -> X + 1 end, 1, NewState#state.metrics),
                        {NewState#state{metrics = NewMetrics}, [{Url, {error, Reason}} | AccResults]}
                end;
            false ->
                {AccState, [{Url, circuit_breaker_open} | AccResults]}
        end
    end, {State, []}, FilteredServers).

should_attempt_connection_with_circuit_breaker(Url, State) ->
    case maps:find(Url, State#state.failed_servers) of
        {ok, #{backoff_until := BackoffUntil}} ->
            erlang:system_time(second) > BackoffUntil;
        {ok, #{fail_count := FailCount}} when FailCount > 5 ->
            false; % Too many failures
        _ ->
            true
    end.

attempt_auto_connect_with_circuit_breaker(ServerInfo, State) ->
    Url = maps:get(url, ServerInfo, undefined),
    case attempt_auto_connect(ServerInfo) of
        ok ->
            % Success - clear failed server entry
            NewFailedServers = maps:remove(Url, State#state.failed_servers),
            % Log successful auto-connect
            catch mcp_advanced_logger:log_discovery_event(auto_connect_success, #{
                url => Url,
                server_info => ServerInfo
            }),
            {ok, State#state{failed_servers = NewFailedServers}};
        {error, Reason} ->
            % Failure - update failed server tracking
            FailedEntry = update_failed_server_entry(Url, State#state.failed_servers),
            NewFailedServers = maps:put(Url, FailedEntry, State#state.failed_servers),
            % Log failed auto-connect
            catch mcp_advanced_logger:log_discovery_event(auto_connect_failed, #{
                url => Url,
                reason => Reason,
                server_info => ServerInfo
            }),
            {error, Reason, State#state{failed_servers = NewFailedServers}}
    end.

update_failed_server_entry(Url, FailedServers) ->
    Now = erlang:system_time(second),
    case maps:find(Url, FailedServers) of
        {ok, Entry} ->
            FailCount = maps:get(fail_count, Entry, 0) + 1,
            BackoffTime = min(300, FailCount * 60), % Max 5 minutes backoff
            Entry#{
                fail_count => FailCount,
                last_failed => Now,
                backoff_until => Now + BackoffTime
            };
        error ->
            #{
                fail_count => 1,
                last_failed => Now,
                backoff_until => Now + 60 % 1 minute initial backoff
            }
    end.

should_auto_connect(#{type := network, url := _Url}) ->
    % Auto-connect to local network servers
    true;
should_auto_connect(_) ->
    false.

attempt_auto_connect(#{url := Url} = ServerInfo) ->
    ServerId = generate_server_id(),
    Config = #{
        url => Url,
        auto_discovered => true,
        server_info => ServerInfo
    },
    
    % Register in registry first
    case mcp_registry:register_server(ServerId, Url, Config) of
        {ok, _} ->
            % Try to connect
            case mcp_connection_manager:connect_server(ServerId) of
                {ok, _} ->
                    error_logger:info_msg("Auto-connected to MCP server: ~s~n", [Url]),
                    ok;
                {error, Reason} ->
                    error_logger:warning_msg("Failed to auto-connect to ~s: ~p~n", [Url, Reason]),
                    mcp_registry:unregister_server(ServerId),
                    {error, Reason}
            end;
        {error, Reason} ->
            error_logger:warning_msg("Failed to register discovered server ~s: ~p~n", [Url, Reason]),
            {error, Reason}
    end.

parse_websocket_url(Url) ->
    % Parse WebSocket URL like "ws://localhost:8765/mcp" or "ws://127.0.0.1:3000/mcp"
    try
        % Remove ws:// prefix
        case binary:match(Url, <<"ws://">>) of
            {0, 5} ->
                Rest = binary:part(Url, 5, byte_size(Url) - 5),
                % Find the first slash to separate host:port from path
                case binary:split(Rest, <<"/">>) of
                    [HostPort | _] ->
                        % Split host and port
                        case binary:split(HostPort, <<":">>) of
                            [Host, PortBin] ->
                                try
                                    Port = binary_to_integer(PortBin),
                                    HostStr = binary_to_list(Host),
                                    {ok, HostStr, Port}
                                catch
                                    _:_ -> {error, invalid_url}
                                end;
                            [Host] ->
                                % Default port for ws:// is 80
                                HostStr = binary_to_list(Host),
                                {ok, HostStr, 80}
                        end;
                    _ ->
                        {error, invalid_url}
                end;
            _ ->
                {error, invalid_url}
        end
    catch
        _:_ ->
            {error, invalid_url}
    end.

%% Inspector compatibility helpers
export_server_entry(Config) ->
    case maps:get(transport, Config, websocket) of
        websocket ->
            case maps:get(url, Config) of
                undefined ->
                    Port = maps:get(port, Config, 8767),
                    #{
                        <<"type">> => <<"sse">>,
                        <<"url">> => iolist_to_binary(io_lib:format("http://localhost:~p/mcp", [Port])),
                        <<"note">> => <<"Erlang MCP Server via SSE">>
                    };
                Url ->
                    #{
                        <<"type">> => <<"sse">>,
                        <<"url">> => Url,
                        <<"note">> => <<"Remote MCP Server via SSE">>
                    }
            end;
        stdio ->
            ServerId = maps:get(server_id, Config, <<"default-server">>),
            #{
                <<"command">> => <<"./start_mcp">>,
                <<"args">> => [<<"server">>, ServerId],
                <<"env">> => #{
                    <<"MCP_SERVER_ID">> => ServerId
                }
            };
        http_sse ->
            Url = maps:get(url, Config, <<"http://localhost:8767/mcp">>),
            #{
                <<"type">> => <<"sse">>,
                <<"url">> => Url,
                <<"note">> => <<"HTTP SSE transport">>
            }
    end.

%% Helper function to auto-connect Graphlit MCP server
auto_connect_graphlit(Config) ->
    % Register Graphlit server in the server configuration
    case mcp_server_config:add_server(#{
        id => <<"graphlit">>,
        name => <<"Graphlit">>,
        category => <<"Knowledge Management">>,
        url => <<"npx -y graphlit-mcp-server">>,
        auth_type => api_key,
        maintainer => <<"Graphlit">>,
        description => <<"Ingest anything from Slack, Discord, websites, Google Drive, email, Jira, Linear or GitHub into a searchable, RAG-ready knowledge base">>,
        status => active,
        capabilities => [
            <<"query_contents">>, <<"query_collections">>, <<"query_feeds">>,
            <<"retrieve_relevant_sources">>, <<"extract_structured_json">>,
            <<"ingest_files">>, <<"ingest_web_pages">>, <<"web_crawling">>,
            <<"web_search">>, <<"slack_integration">>, <<"discord_integration">>,
            <<"github_integration">>, <<"notion_integration">>, <<"linear_integration">>
        ],
        metadata => Config
    }) of
        {ok, _} ->
            % Try to connect via MCP client
            case mcp_connection_manager:connect_server(<<"graphlit">>) of
                {ok, Pid} ->
                    io:format("[MCP_MGR] Successfully connected to Graphlit MCP server~n"),
                    {ok, Pid};
                {error, Reason} ->
                    io:format("[WARNING] Failed to connect to Graphlit MCP server: ~p~n", [Reason]),
                    {error, Reason}
            end;
        {error, {aborted, {already_exists, _}}} ->
            % Server already exists, try to connect
            case mcp_connection_manager:connect_server(<<"graphlit">>) of
                {ok, Pid} ->
                    io:format("[MCP_MGR] Connected to existing Graphlit MCP server~n"),
                    {ok, Pid};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            io:format("[ERROR] Failed to register Graphlit server: ~p~n", [Reason]),
            {error, Reason}
    end.