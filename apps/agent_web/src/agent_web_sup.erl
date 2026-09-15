-module(agent_web_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    %% Services start before the listener, and bootstrap starts only
    %% after both are available.
    SupFlags = #{strategy => rest_for_one,
                 intensity => 5,
                 period => 30},
    
    Port = application:get_env(agent_web, port, 8080),
    
    %% Comprehensive routes with all handlers
    Dispatch = cowboy_router:compile([
        {'_', [
            %% Static Assets
            {"/", agent_web_handler, []},
            {"/dashboard", agent_web_handler, []},
            {"/assets/[...]", cowboy_static, {priv_dir, agent_web, "static/dist/assets"}},
            {"/vite.svg", cowboy_static, {priv_file, agent_web, "static/dist/vite.svg"}},
            {"/static/[...]", cowboy_static, {priv_dir, agent_web, "static"}},
            {"/uploads/[...]", cowboy_static, {priv_dir, agent_web, "uploads"}},
            
            %% Documentation
            {"/docs", swagger_docs_handler, []},
            {"/docs/swagger.json", swagger_docs_handler, []},
            
            %% Core Agent Management
            {"/api/agents", agent_api_handler, []},
            {"/api/agents/health", agent_api_handler, []},
            {"/api/agents/communicate", agent_communication_handler, []},
            {"/api/agents/broadcast", agent_communication_handler, []},
            {"/api/agents/collaborations", agent_communication_handler, []},
            {"/api/agents/messages/:id", agent_communication_handler, []},
            {"/api/agents/quorum/[...]", agent_quorum_handler, []},
            {"/api/agents/:id/chat", agent_chat_handler, []},
            {"/api/agents/:id/execute", agent_execute_handler, []},
            {"/api/agents/:id", agent_api_handler, []},
            
            %% Fleet Management
            {"/api/fleet/agents", fleet_management_handler, []},
            {"/api/fleet/status", fleet_management_handler, []},
            {"/api/fleet/broadcast", fleet_management_handler, []},
            {"/api/fleet/autonomous/enable", fleet_management_handler, []},
            {"/api/fleet/autonomous/execute", fleet_management_handler, []},
            
            %% Super Agent
            {"/api/super-agent/capabilities", super_agent_handler, []},
            {"/api/super-agent/chat", super_agent_handler, []},
            {"/api/super-agent/system-command", super_agent_handler, []},
            {"/api/super-agent/system-status", super_agent_handler, []},
            
            %% MCP (Model Context Protocol)
            {"/api/mcp", mcp_api_handler, []},
            {"/api/mcp/servers", mcp_api_handler, []},
            {"/api/mcp/servers/:id", mcp_api_handler, []},
            {"/api/mcp/servers/:id/test", mcp_api_handler, []},
            {"/api/mcp/servers/:id/connect", mcp_api_handler, []},
            {"/api/mcp/servers/:id/disconnect", mcp_api_handler, []},
            {"/api/mcp/servers/:id/tools", mcp_api_handler, []},
            {"/api/mcp/servers/:id/resources", mcp_api_handler, []},
            {"/api/mcp/servers/:id/prompts", mcp_api_handler, []},
            {"/api/mcp/export/server/:id", mcp_api_handler, []},
            {"/api/mcp/export/servers", mcp_api_handler, []},
            {"/api/mcp/local/[...]", mcp_management_handler, []},
            {"/api/mcp/remote/[...]", mcp_management_handler, []},
            {"/api/mcp/orchestration/[...]", mcp_orchestration_engine, []},
            {"/api/mcp/discover", mcp_management_handler, []},
            {"/api/mcp/status", mcp_management_handler, []},
            {"/api/mcp/auto_connect", mcp_management_handler, []},
            {"/api/mcp/execute_tool", mcp_management_handler, []},
            {"/api/mcp/read_resource", mcp_management_handler, []},
            {"/api/mcp/get_prompt", mcp_management_handler, []},
            {"/api/mcp-servers", mcp_sse_handler, []},
            {"/api/mcp-servers/:id", mcp_sse_handler, []},
            {"/api/mcp-servers/:id/discover", mcp_sse_handler, []},
            {"/mcp", mcp_sse_handler, []},
            {"/mcp/:server_id", mcp_sse_handler, []},
            
            %% System Health & Monitoring
            {"/api/system/health", system_health_handler, []},
            {"/api/system/session", agent_web_session_handler, []},
            {"/api/system/metrics", system_health_handler, []},
            {"/api/system/supervision-tree", supervision_tree_handler, []},
            {"/api/monitoring/[...]", agent_monitoring_handler, []},
            
            %% Error Handling & Crash Reports
            {"/api/crashes", crash_report_api_handler, []},
            {"/api/crashes/stats", crash_report_api_handler, []},
            {"/api/crashes/:id", crash_report_api_handler, []},
            {"/api/crashes/:id/analysis", crash_report_api_handler, []},
            {"/api/crashes/:id/fixes", crash_report_api_handler, []},
            {"/api/crashes/:id/analyze", crash_report_api_handler, []},
            {"/api/crashes/:id/fix", crash_report_api_handler, []},
            {"/api/errors", error_api_handler, []},
            {"/api/errors/summary", error_api_handler, []},
            {"/api/errors/trends", error_api_handler, []},
            {"/api/errors/clear", error_api_handler, []},
            {"/api/interpretations", error_interpretation_api_handler, []},
            {"/api/interpretations/interpret", error_interpretation_api_handler, []},
            {"/api/interpretations/subscribe", error_interpretation_api_handler, []},
            {"/api/interpretations/:id", error_interpretation_api_handler, []},
            
            %% Authentication & API Keys
            {"/api/keys", api_keys_handler, []},
            {"/api/keys/requirements", api_keys_handler, []},
            {"/api/keys/check", api_keys_handler, []},
            {"/api/keys/load-env", api_keys_handler, []},
            {"/api/keys/:service", api_keys_handler, []},
            {"/api/oauth/authorize/:provider", oauth_handler, []},
            {"/api/oauth/callback/:provider", oauth_handler, []},
            {"/api/oauth/revoke/:provider", oauth_handler, []},
            {"/api/oauth/status/:provider", oauth_handler, []},
            {"/api/mcp/oauth/start/:server_id", mcp_oauth_handler, []},
            {"/api/mcp/oauth/callback/:server_id", mcp_oauth_handler, []},
            {"/api/mcp/oauth/status/:server_id", mcp_oauth_handler, []},
            {"/api/mcp/oauth/connect/:server_id", mcp_oauth_handler, []},
            {"/api/mcp/oauth/refresh/:server_id", mcp_oauth_handler, []},
            {"/api/mcp/oauth/revoke/:server_id", mcp_oauth_handler, []},
            
            %% Workflow Orchestration
            {"/api/workflow/create", workflow_api_handler, []},
            {"/api/workflow/scatter", workflow_api_handler, []},
            {"/api/workflow/gather", workflow_api_handler, []},
            {"/api/workflow/execute-script", workflow_api_handler, []},
            {"/api/workflow/compose", workflow_api_handler, []},
            {"/api/workflow/status", workflow_api_handler, []},
            
            %% Bulk Operations
            {"/api/bulk/broadcast", bulk_operations_handler, []},
            {"/api/bulk/system-prompt", bulk_operations_handler, []},
            {"/api/bulk/callback", bulk_operations_handler, []},
            {"/api/bulk/agent-management", bulk_operations_handler, []},
            {"/api/bulk/query", bulk_operations_handler, []},
            {"/api/bulk/transform", bulk_operations_handler, []},
            
            %% Cost Tracking
            {"/api/costs", cost_tracking_handler, []},
            {"/api/costs/summary", cost_tracking_handler, []},
            {"/api/costs/agent/:agent_id", cost_tracking_handler, []},
            {"/api/costs/model/:model", cost_tracking_handler, []},
            {"/api/costs/timerange", cost_tracking_handler, []},
            {"/api/costs/report", cost_tracking_handler, []},
            {"/api/costs/reset", cost_tracking_handler, []},
            
            %% Conversations & Timeline
            {"/api/conversations", conversation_handler, []},
            {"/api/conversations/:conversation_id", conversation_handler, []},
            {"/api/timeline/events", timeline_handler, []},
            
            %% Models & Definitions
            {"/api/models/definitions", model_api_handler, []},
            {"/api/models/definitions/:id", model_api_handler, []},
            {"/api/models/validate", model_api_handler, []},
            {"/api/models/generate/:id/:language", model_api_handler, []},
            
            %% External Integrations
            {"/api/pipedream/apps", pipedream_api_handler, []},
            {"/api/pipedream/apps/:app_slug", pipedream_api_handler, []},
            {"/api/pipedream/apps/:app_slug/tools", pipedream_api_handler, []},
            {"/api/pipedream/discover", pipedream_api_handler, []},
            {"/api/pipedream/connections", pipedream_api_handler, []},
            {"/api/pipedream/connections/:app_slug", pipedream_api_handler, []},
            {"/api/pipedream/connect", pipedream_api_handler, []},
            {"/api/pipedream/disconnect", pipedream_api_handler, []},
            {"/api/pipedream/refresh", pipedream_api_handler, []},
            {"/api/pipedream/tools", pipedream_api_handler, []},
            {"/api/pipedream/tools/call", pipedream_api_handler, []},
            {"/api/pipedream/oauth/callback", pipedream_api_handler, []},
            {"/api/pipedream/discovery/refresh", pipedream_api_handler, []},
            {"/api/pipedream/user/apps/register", pipedream_api_handler, []},
            {"/api/pipedream/stats", pipedream_api_handler, []},
            {"/api/mcp-registry/[...]", mcp_github_registry_handler, []},
            
            %% Agent Scheduling & Timeline
            {"/api/scheduling/tasks", agent_scheduling_handler, []},
            {"/api/scheduling/tasks/:task_id", agent_scheduling_handler, []},
            {"/api/scheduling/tasks/:task_id/execute", agent_scheduling_handler, []},
            {"/api/scheduling/agents/:agent_id/tasks", agent_scheduling_handler, []},
            {"/api/scheduling/agents/:agent_id/temporal", agent_scheduling_handler, []},
            {"/api/scheduling/agents/:agent_id/temporal/enable", agent_scheduling_handler, []},
            {"/api/scheduling/upcoming", agent_scheduling_handler, []},
            
            %% Development & Utilities
            {"/api/reload", hot_reload_handler, []},
            {"/api/logs/interactions", logs_api_handler, []},
            {"/api/logs/errors", logs_api_handler, []},
            {"/api/stats", stats_control_handler, []},
            {"/api/stats/reset", stats_control_handler, []},
            {"/api/stats/display", stats_control_handler, []},
            {"/api/examples", examples_handler, []},
            {"/api/examples/:type", examples_handler, []},
            {"/api/knowledge/[...]", knowledge_base_handler, []},
            {"/api/templates", agent_templates_handler, []},
            {"/api/upload/image", image_upload_handler, []},
            {"/api/claude-cli", claude_cli_handler, []},
            {"/api/discovery/[...]", discovery_mesh_handler, []},
            
            %% WebSocket Endpoints
            {"/ws", agent_ws_handler, []},
            {"/ws/super-agent", super_agent_handler, []},
            {"/ws/system/metrics", system_metrics_handler, []},
            {"/ws/crashes", crash_report_ws_handler, []},
            {"/ws/errors", error_ws_handler, []},
            {"/ws/timeline", timeline_ws_handler, []}
        ]}
    ]),
    %% HTTP configuration
    %% Do not inherit Cowboy's all-interface default accidentally.  A
    %% deployment that intentionally serves a network interface can set the
    %% existing `host` application environment value explicitly.
    HttpTransportOpts = [{ip, bind_address()}, {port, Port}],
    RequestTimeout = positive_timeout(request_timeout, 60000),
    %% An infinite connection idle timeout lets abandoned clients retain
    %% Cowboy/ranch resources forever.  Keep the deployment override explicit
    %% but use a bounded default.
    IdleTimeout = positive_timeout(idle_timeout, 120000),
    ProtocolOpts = #{
        env => #{dispatch => Dispatch},
        middlewares => [agent_web_auth_middleware, cowboy_router, cowboy_handler],
        request_timeout => RequestTimeout,
        idle_timeout => IdleTimeout
    },
    
    HttpCowboySpec = #{
        id => agent_web_http_listener,
        start => {cowboy, start_clear, [
            agent_web_http_listener,
            HttpTransportOpts,
            ProtocolOpts
        ]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [cowboy]
    },
    
    ServicesSpec = #{
        id => agent_web_services_sup,
        start => {agent_web_services_sup, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [agent_web_services_sup]
    },

    BootstrapSpec = #{
        id => agent_web_bootstrap,
        start => {agent_web_bootstrap, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [agent_web_bootstrap]
    },

    ChildSpecs = [
        ServicesSpec,
        HttpCowboySpec,
        BootstrapSpec
    ],

    {ok, {SupFlags, ChildSpecs}}.

positive_timeout(Key, Default) ->
    case application:get_env(agent_web, Key, Default) of
        Value when is_integer(Value), Value > 0 -> Value;
        _ -> Default
    end.

bind_address() ->
    case application:get_env(agent_web, host, "127.0.0.1") of
        Address when is_tuple(Address) ->
            Address;
        Address when is_binary(Address) ->
            parse_bind_address(binary_to_list(Address));
        Address when is_list(Address) ->
            parse_bind_address(Address);
        _ ->
            {127, 0, 0, 1}
    end.

parse_bind_address(Address) ->
    case inet:parse_address(Address) of
        {ok, Parsed} -> Parsed;
        {error, _} -> {127, 0, 0, 1}
    end.
