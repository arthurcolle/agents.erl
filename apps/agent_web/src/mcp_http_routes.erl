%% @doc MCP HTTP Routes Integration
%%
%% Integrates MCP Streamable HTTP transport routes into the existing
%% Cowboy web server configuration.
-module(mcp_http_routes).

-include_lib("kernel/include/logger.hrl").

%% API exports
-export([
    get_mcp_routes/0,
    add_mcp_routes/1,
    start_mcp_endpoint/1,
    stop_mcp_endpoint/0
]).

%% Configuration exports
-export([
    get_default_config/0,
    validate_config/1
]).

%% Types
-type route_spec() :: {binary(), module(), map()}.
-type routes() :: [route_spec()].
-type config() :: map().

%%% ============================================================================
%%% API Functions
%%% ============================================================================

%% @doc Get MCP route specifications
-spec get_mcp_routes() -> routes().
get_mcp_routes() ->
    [
        % Main MCP endpoint - supports GET, POST, DELETE
        {"/mcp", mcp_streamable_http_handler, #{}},
        
        % Alternative endpoint paths for compatibility
        {"/api/mcp", mcp_streamable_http_handler, #{}},
        {"/mcp/v1", mcp_streamable_http_handler, #{}},
        
        % Health check endpoint for MCP transport
        {"/mcp/health", mcp_health_handler, #{}},
        
        % MCP transport status and diagnostics
        {"/mcp/status", mcp_status_handler, #{}}
    ].

%% @doc Add MCP routes to existing Cowboy dispatch
-spec add_mcp_routes([term()]) -> [term()].
add_mcp_routes(ExistingRoutes) ->
    McpRoutes = get_mcp_routes(),
    
    % Add MCP routes to the default host
    case ExistingRoutes of
        [] ->
            % No existing routes, create new dispatch
            [
                {'_', McpRoutes}
            ];
        _ ->
            % Merge with existing routes
            lists:map(fun
                ({'_', HostRoutes}) ->
                    {'_', HostRoutes ++ McpRoutes};
                ({Host, HostRoutes}) ->
                    {Host, HostRoutes ++ McpRoutes};
                (Route) ->
                    Route
            end, ExistingRoutes)
    end.

%% @doc Start MCP endpoint with configuration
-spec start_mcp_endpoint(config()) -> {ok, pid()} | {error, term()}.
start_mcp_endpoint(Config) ->
    ValidatedConfig = validate_and_merge_config(Config),
    
    case mcp_streamable_http_sup:start_link(ValidatedConfig) of
        {ok, Pid} ->
            ?LOG_INFO("Started MCP Streamable HTTP transport on endpoint ~s", 
                     [maps:get(endpoint_path, ValidatedConfig, <<"/mcp">>)]),
            {ok, Pid};
        {error, Reason} ->
            ?LOG_ERROR("Failed to start MCP transport: ~p", [Reason]),
            {error, Reason}
    end.

%% @doc Stop MCP endpoint
-spec stop_mcp_endpoint() -> ok.
stop_mcp_endpoint() ->
    case whereis(mcp_streamable_http_sup) of
        undefined ->
            ok;
        Pid ->
            supervisor:terminate_child(Pid, mcp_streamable_http_sup),
            ?LOG_INFO("Stopped MCP Streamable HTTP transport")
    end.

%%% ============================================================================
%%% Configuration Functions
%%% ============================================================================

%% @doc Get default MCP transport configuration
-spec get_default_config() -> config().
get_default_config() ->
    #{
        % Transport settings
        endpoint_path => <<"/mcp">>,
        protocol_version => <<"2025-03-26">>,
        
        % Security settings
        allowed_origins => [
            <<"http://localhost">>,
            <<"https://localhost">>,
            <<"http://127.0.0.1">>,
            <<"https://127.0.0.1">>
        ],
        validate_origin => true,
        require_session_for_streams => false,
        
        % Session management
        session_timeout => 3600000,    % 1 hour
        stream_timeout => 300000,      % 5 minutes
        max_sessions => 1000,
        session_storage => ets,
        
        % Message buffering
        max_buffer_size => 10000,
        max_message_age => 3600000,    % 1 hour
        enable_message_replay => true,
        buffer_cleanup_interval => 60000, % 1 minute
        
        % SSE settings
        keepalive_interval => 30000,   % 30 seconds
        max_event_id => 999999999,
        enable_compression => false,
        
        % Performance settings
        max_request_size => 1048576,   % 1MB
        max_concurrent_streams => 100,
        connection_timeout => 30000,   % 30 seconds
        
        % Logging and monitoring
        log_level => info,
        enable_metrics => true,
        metrics_interval => 60000,     % 1 minute
        
        % Feature flags
        enable_backwards_compatibility => true,
        enable_extensions => false,
        strict_json_rpc => true
    }.

%% @doc Validate configuration
-spec validate_config(config()) -> {ok, config()} | {error, term()}.
validate_config(Config) ->
    try
        ValidatedConfig = validate_config_fields(Config),
        {ok, ValidatedConfig}
    catch
        error:{validation_error, Field, Reason} ->
            {error, {invalid_config, Field, Reason}};
        error:Reason ->
            {error, {validation_failed, Reason}}
    end.

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

%% @doc Validate and merge with default configuration
-spec validate_and_merge_config(config()) -> config().
validate_and_merge_config(UserConfig) ->
    DefaultConfig = get_default_config(),
    MergedConfig = maps:merge(DefaultConfig, UserConfig),
    
    case validate_config(MergedConfig) of
        {ok, ValidatedConfig} ->
            ValidatedConfig;
        {error, Reason} ->
            ?LOG_WARNING("Configuration validation failed: ~p, using defaults", [Reason]),
            DefaultConfig
    end.

%% @doc Validate individual configuration fields
-spec validate_config_fields(config()) -> config().
validate_config_fields(Config) ->
    ValidatedConfig = maps:fold(fun validate_config_field/3, #{}, Config),
    
    % Additional cross-field validations
    validate_cross_field_constraints(ValidatedConfig),
    
    ValidatedConfig.

%% @doc Validate individual configuration field
-spec validate_config_field(atom(), term(), config()) -> config().
validate_config_field(endpoint_path, Path, Acc) when is_binary(Path) ->
    case binary:match(Path, <<"/">>) of
        {0, 1} -> Acc#{endpoint_path => Path};
        _ -> error({validation_error, endpoint_path, must_start_with_slash})
    end;

validate_config_field(protocol_version, Version, Acc) when is_binary(Version) ->
    ValidVersions = [<<"2025-03-26">>, <<"2024-11-05">>],
    case lists:member(Version, ValidVersions) of
        true -> Acc#{protocol_version => Version};
        false -> error({validation_error, protocol_version, unsupported_version})
    end;

validate_config_field(allowed_origins, Origins, Acc) when is_list(Origins) ->
    ValidatedOrigins = lists:map(fun(Origin) ->
        case is_binary(Origin) of
            true -> Origin;
            false -> error({validation_error, allowed_origins, must_be_binary_list})
        end
    end, Origins),
    Acc#{allowed_origins => ValidatedOrigins};

validate_config_field(session_timeout, Timeout, Acc) when is_integer(Timeout), Timeout > 0 ->
    Acc#{session_timeout => Timeout};

validate_config_field(stream_timeout, Timeout, Acc) when is_integer(Timeout), Timeout > 0 ->
    Acc#{stream_timeout => Timeout};

validate_config_field(max_sessions, Max, Acc) when is_integer(Max), Max > 0 ->
    Acc#{max_sessions => Max};

validate_config_field(session_storage, Storage, Acc) when Storage =:= ets; Storage =:= persistent ->
    Acc#{session_storage => Storage};

validate_config_field(max_buffer_size, Size, Acc) when is_integer(Size), Size > 0 ->
    Acc#{max_buffer_size => Size};

validate_config_field(max_message_age, Age, Acc) when is_integer(Age), Age > 0 ->
    Acc#{max_message_age => Age};

validate_config_field(keepalive_interval, Interval, Acc) when is_integer(Interval), Interval > 0 ->
    Acc#{keepalive_interval => Interval};

validate_config_field(max_request_size, Size, Acc) when is_integer(Size), Size > 0 ->
    Acc#{max_request_size => Size};

validate_config_field(max_concurrent_streams, Max, Acc) when is_integer(Max), Max > 0 ->
    Acc#{max_concurrent_streams => Max};

validate_config_field(connection_timeout, Timeout, Acc) when is_integer(Timeout), Timeout > 0 ->
    Acc#{connection_timeout => Timeout};

validate_config_field(log_level, Level, Acc) when Level =:= debug; Level =:= info; 
                                                   Level =:= warning; Level =:= error ->
    Acc#{log_level => Level};

validate_config_field(Field, Value, Acc) when is_boolean(Value) ->
    % Accept any boolean field
    Acc#{Field => Value};

validate_config_field(Field, Value, Acc) ->
    % Unknown field - log warning but don't fail
    ?LOG_WARNING("Unknown configuration field: ~p with value ~p", [Field, Value]),
    Acc#{Field => Value}.

%% @doc Validate cross-field constraints
-spec validate_cross_field_constraints(config()) -> ok.
validate_cross_field_constraints(Config) ->
    SessionTimeout = maps:get(session_timeout, Config),
    StreamTimeout = maps:get(stream_timeout, Config),
    
    % Stream timeout should be less than session timeout
    case StreamTimeout < SessionTimeout of
        true -> ok;
        false -> 
            ?LOG_WARNING("Stream timeout (~p) should be less than session timeout (~p)", 
                        [StreamTimeout, SessionTimeout])
    end,
    
    MaxBufferSize = maps:get(max_buffer_size, Config),
    MaxSessions = maps:get(max_sessions, Config),
    
    % Reasonable buffer size relative to max sessions
    ReasonableBufferSize = MaxSessions * 100,
    case MaxBufferSize > ReasonableBufferSize of
        true ->
            ?LOG_WARNING("Buffer size (~p) may be too large for max sessions (~p)", 
                        [MaxBufferSize, MaxSessions]);
        false -> ok
    end,
    
    ok.