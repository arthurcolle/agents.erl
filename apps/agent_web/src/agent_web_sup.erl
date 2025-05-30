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
    io:format("[SUP] Using port ~p for web server~n", [Port]),
    
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", agent_web_handler, []},
            {"/api/agents", agent_api_handler, []},
            {"/api/agents/:id", agent_api_handler, []},
            {"/api/agents/:id/execute", agent_execute_handler, []},
            {"/api/templates", agent_templates_handler, []},
            {"/api/examples/:type", examples_handler, []},
            {"/api/monitoring/[...]", agent_monitoring_handler, []},
            {"/api/knowledge/[...]", knowledge_base_handler, []},
            {"/api/mcp/servers", mcp_api_handler, []},
            {"/api/mcp/servers/:id", mcp_api_handler, []},
            {"/api/mcp/servers/:id/test", mcp_api_handler, []},
            {"/api/mcp/servers/:id/connect", mcp_api_handler, []},
            {"/api/mcp/servers/:id/disconnect", mcp_api_handler, []},
            {"/api/mcp/servers/:id/tools", mcp_api_handler, []},
            {"/api/mcp/servers/:id/resources", mcp_api_handler, []},
            {"/api/mcp/servers/:id/prompts", mcp_api_handler, []},
            {"/api/mcp/local/[...]", mcp_management_handler, []},
            {"/api/mcp/remote/[...]", mcp_management_handler, []},
            {"/api/mcp/discover", mcp_management_handler, []},
            {"/api/mcp/status", mcp_management_handler, []},
            {"/api/mcp/auto_connect", mcp_management_handler, []},
            {"/api/mcp/execute_tool", mcp_management_handler, []},
            {"/api/mcp/read_resource", mcp_management_handler, []},
            {"/api/mcp/get_prompt", mcp_management_handler, []},
            {"/api/mcp/export/server/:id", mcp_api_handler, []},
            {"/api/mcp/export/servers", mcp_api_handler, []},
            {"/mcp", mcp_sse_handler, #{server_id => <<"agents_main">>}},
            {"/mcp/:server_id", mcp_sse_handler, []},
            {"/ws", agent_ws_handler, []},
            {"/vite.svg", cowboy_static, {priv_file, agent_web, "static/dist/vite.svg"}},
            {"/assets/[...]", cowboy_static, {priv_dir, agent_web, "static/dist/assets"}},
            {"/static/[...]", cowboy_static, {priv_dir, agent_web, "static"}}
        ]}
    ]),
    io:format("[SUP] Compiled Cowboy routes~n"),
    
    TransportOpts = [{port, Port}],
    ProtocolOpts = #{
        env => #{dispatch => Dispatch},
        middlewares => [cowboy_router, cowboy_handler],
        request_timeout => 60000,
        idle_timeout => infinity
    },
    io:format("[SUP] Configured Cowboy with transport opts: ~p~n", [TransportOpts]),
    
    CowboySpec = ranch:child_spec(
        agent_web_listener,
        ranch_tcp,
        TransportOpts,
        cowboy_clear,
        ProtocolOpts
    ),
    io:format("[SUP] Created Cowboy child spec~n"),
    
    McpRegistrySpec = #{
        id => mcp_registry,
        start => {mcp_registry, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [mcp_registry]
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
    
    McpAdvancedLoggerSpec = #{
        id => mcp_advanced_logger,
        start => {mcp_advanced_logger, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [mcp_advanced_logger]
    },
    
    ChildSpecs = [McpAdvancedLoggerSpec, McpRegistrySpec, McpConnectionManagerSpec, McpManagerSpec, CowboySpec],
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