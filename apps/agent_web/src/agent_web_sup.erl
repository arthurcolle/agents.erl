-module(agent_web_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    io:format("[SUP] Starting agent_web supervisor~n"),
    case supervisor:start_link({local, ?SERVER}, ?MODULE, []) of
        {ok, Pid} ->
            io:format("[SUP] Supervisor started successfully with PID ~p~n", [Pid]),
            {ok, Pid};
        {error, Reason} ->
            io:format("[ERROR] Failed to start supervisor: ~p~n", [Reason]),
            {error, Reason}
    end.

init([]) ->
    io:format("[SUP] Initializing supervisor with children~n"),
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    
    Port = application:get_env(agent_web, port, 8080),
    HttpsPort = application:get_env(agent_web, https_port, 8443),
    SslEnabled = application:get_env(agent_web, ssl_enabled, false),
    io:format("[SUP] Using port ~p for HTTP web server~n", [Port]),
    case SslEnabled of
        true -> io:format("[SUP] Using port ~p for HTTPS web server~n", [HttpsPort]);
        false -> io:format("[SUP] HTTPS disabled~n")
    end,
    
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", agent_web_handler, []},
            {"/api/agents", agent_api_handler, []},
            {"/api/agents/:id", agent_api_handler, []},
            {"/api/agents/:id/execute", agent_execute_handler, []},
            {"/api/conversations", conversation_handler, []},
            {"/api/conversations/:conversation_id", conversation_handler, []},
            {"/api/timeline/events", timeline_handler, []},
            {"/api/upload/image", image_upload_handler, []},
            {"/api/templates", agent_templates_handler, []},
            {"/api/examples/:type", examples_handler, []},
            {"/api/monitoring/[...]", agent_monitoring_handler, []},
            {"/api/knowledge/[...]", knowledge_base_handler, []},
            {"/api/mcp-servers", mcp_server_handler, []},
            {"/api/mcp-servers/:id", mcp_server_handler, []},
            {"/api/mcp-servers/:id/discover", mcp_server_handler, []},
            {"/api/mcp/servers", mcp_api_handler, []},
            {"/api/mcp/servers/:id", mcp_api_handler, []},
            {"/api/mcp/servers/:id/test", mcp_api_handler, []},
            {"/api/mcp/servers/:id/connect", mcp_api_handler, []},
            {"/api/mcp/servers/:id/disconnect", mcp_api_handler, []},
            {"/api/mcp/servers/:id/tools", mcp_api_handler, []},
            {"/api/mcp/servers/:id/resources", mcp_api_handler, []},
            {"/api/mcp/servers/:id/prompts", mcp_api_handler, []},
            {"/api/mcp/orchestration/[...]", mcp_orchestration_handler, []},
            {"/api/mcp/ai/[...]", mcp_orchestration_handler, []},
            {"/api/mcp/local/[...]", mcp_management_handler, []},
            {"/api/mcp/remote/[...]", mcp_management_handler, []},
            {"/api/mcp/discover", mcp_management_handler, []},
            {"/api/mcp/status", mcp_management_handler, []},
            {"/api/mcp/auto_connect", mcp_management_handler, []},
            {"/api/mcp/execute_tool", mcp_management_handler, []},
            {"/api/mcp/read_resource", mcp_management_handler, []},
            {"/api/mcp/get_prompt", mcp_management_handler, []},
            {"/api/mcp/export/server/:id", mcp_api_handler, []},
            {"/api/system/health", system_health_handler, []},
            {"/api/ssl/[...]", ssl_cert_handler, []},
            {"/api/mcp/export/servers", mcp_api_handler, []},
            {"/api/oauth/authorize/:provider", oauth_handler, []},
            {"/api/oauth/callback/:provider", oauth_handler, []},
            {"/api/oauth/revoke/:provider", oauth_handler, []},
            {"/api/oauth/status/:provider", oauth_handler, []},
            {"/api/claude/config", claude_config_handler, []},
            {"/api/claude/export", claude_config_handler, []},
            {"/api/claude/oauth-status", claude_config_handler, []},
            {"/mcp", mcp_sse_handler, #{server_id => <<"agents_main">>}},
            {"/mcp/:server_id", mcp_sse_handler, []},
            {"/ws", agent_ws_handler, []},
            {"/vite.svg", cowboy_static, {priv_file, agent_web, "static/dist/vite.svg"}},
            {"/assets/[...]", cowboy_static, {priv_dir, agent_web, "static/dist/assets"}},
            {"/uploads/[...]", cowboy_static, {priv_dir, agent_web, "static/uploads"}},
            {"/static/[...]", cowboy_static, {priv_dir, agent_web, "static"}}
        ]}
    ]),
    io:format("[SUP] Compiled Cowboy routes~n"),
    
    % HTTP configuration
    HttpTransportOpts = [{port, Port}],
    ProtocolOpts = #{
        env => #{dispatch => Dispatch},
        middlewares => [cowboy_router, cowboy_handler],
        request_timeout => 60000,
        idle_timeout => infinity
    },
    io:format("[SUP] Configured Cowboy HTTP with transport opts: ~p~n", [HttpTransportOpts]),
    
    HttpCowboySpec = ranch:child_spec(
        agent_web_http_listener,
        ranch_tcp,
        HttpTransportOpts,
        cowboy_clear,
        ProtocolOpts
    ),
    io:format("[SUP] Created HTTP Cowboy child spec~n"),
    
    % HTTPS configuration (optional)
    HttpsSpecs = case SslEnabled of
        true ->
            CertFile = application:get_env(agent_web, cert_file, "./certs/localhost.crt"),
            KeyFile = application:get_env(agent_web, key_file, "./certs/localhost.key"),
            case {filelib:is_regular(CertFile), filelib:is_regular(KeyFile)} of
                {true, true} ->
                    io:format("[SUP] SSL certificates found: ~s, ~s~n", [CertFile, KeyFile]),
                    HttpsTransportOpts = [
                        {port, HttpsPort},
                        {certfile, CertFile},
                        {keyfile, KeyFile},
                        {versions, ['tlsv1.2', 'tlsv1.3']},
                        {ciphers, [
                            "ECDHE-RSA-AES256-GCM-SHA384",
                            "ECDHE-RSA-AES128-GCM-SHA256",
                            "ECDHE-RSA-AES256-SHA384",
                            "ECDHE-RSA-AES128-SHA256"
                        ]}
                    ],
                    HttpsCowboySpec = ranch:child_spec(
                        agent_web_https_listener,
                        ranch_ssl,
                        HttpsTransportOpts,
                        cowboy_tls,
                        ProtocolOpts
                    ),
                    io:format("[SUP] Created HTTPS Cowboy child spec~n"),
                    [HttpsCowboySpec];
                _ ->
                    io:format("[SUP] SSL certificates not found: ~s or ~s, HTTPS disabled~n", [CertFile, KeyFile]),
                    []
            end;
        false ->
            []
    end,
    
    McpAdvancedLoggerSpec = #{
        id => mcp_advanced_logger,
        start => {mcp_advanced_logger, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [mcp_advanced_logger]
    },
    
    McpRegistrySpec = #{
        id => mcp_registry,
        start => {mcp_registry, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [mcp_registry]
    },
    
    McpAdvancedConfigSpec = #{
        id => mcp_advanced_config,
        start => {mcp_advanced_config, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [mcp_advanced_config]
    },
    
    OAuthManagerSpec = #{
        id => oauth_manager,
        start => {oauth_manager, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [oauth_manager]
    },
    
    McpMonitorSpec = #{
        id => mcp_monitor,
        start => {mcp_monitor, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [mcp_monitor]
    },
    
    McpConnectionManagerSpec = #{
        id => mcp_connection_manager,
        start => {mcp_connection_manager, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [mcp_connection_manager]
    },
    
    McpManagerSpec = #{
        id => mcp_manager,
        start => {mcp_manager, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [mcp_manager]
    },
    
    McpOrchestrationEngineSpec = #{
        id => mcp_orchestration_engine,
        start => {mcp_orchestration_engine, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [mcp_orchestration_engine]
    },
    
    % Advanced AI Systems
    AiOrchestrationEngineSpec = #{
        id => ai_orchestration_engine,
        start => {ai_orchestration_engine, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [ai_orchestration_engine]
    },
    
    QuantumConsensusEngineSpec = #{
        id => quantum_consensus_engine,
        start => {quantum_consensus_engine, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [quantum_consensus_engine]
    },
    
    SelfHealingSystemSpec = #{
        id => self_healing_system,
        start => {self_healing_system, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [self_healing_system]
    },
    
    NeuralLoadBalancerSpec = #{
        id => neural_load_balancer,
        start => {neural_load_balancer, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [neural_load_balancer]
    },
    
    BlockchainAuthSystemSpec = #{
        id => blockchain_auth_system,
        start => {blockchain_auth_system, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [blockchain_auth_system]
    },
    
    TemporalDebuggerSpec = #{
        id => temporal_debugger,
        start => {temporal_debugger, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [temporal_debugger]
    },
    
    ChildSpecs = [
        McpAdvancedLoggerSpec, 
        McpRegistrySpec, 
        McpAdvancedConfigSpec,
        OAuthManagerSpec,
        McpMonitorSpec,
        McpConnectionManagerSpec, 
        McpManagerSpec, 
        McpOrchestrationEngineSpec,
        % Advanced AI Systems
        AiOrchestrationEngineSpec,
        QuantumConsensusEngineSpec,
        SelfHealingSystemSpec,
        NeuralLoadBalancerSpec,
        BlockchainAuthSystemSpec,
        TemporalDebuggerSpec,
        HttpCowboySpec
    ] ++ HttpsSpecs,
    io:format("[SUP] Starting ~p child processes~n", [length(ChildSpecs)]),
    lists:foreach(fun(ChildSpec) ->
        case ChildSpec of
            #{id := Id} ->
                io:format("[SUP] Will start child: ~p~n", [Id]);
            {Id, _, _, _, _, _} ->
                io:format("[SUP] Will start child: ~p (ranch format)~n", [Id]);
            _ ->
                io:format("[SUP] Will start child: ~p~n", [element(1, ChildSpec)])
        end
    end, ChildSpecs),
    io:format("[SUP] Supervisor initialization complete~n"),
    {ok, {SupFlags, ChildSpecs}}.