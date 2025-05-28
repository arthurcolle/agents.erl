-module(agent_web_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    
    Port = application:get_env(agent_web, port, 8080),
    
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
            {"/ws", agent_ws_handler, []},
            {"/vite.svg", cowboy_static, {priv_file, agent_web, "static/dist/vite.svg"}},
            {"/assets/[...]", cowboy_static, {priv_dir, agent_web, "static/dist/assets"}},
            {"/static/[...]", cowboy_static, {priv_dir, agent_web, "static"}}
        ]}
    ]),
    
    TransportOpts = [{port, Port}],
    ProtocolOpts = #{env => #{dispatch => Dispatch}},
    
    CowboySpec = ranch:child_spec(
        agent_web_listener,
        ranch_tcp,
        TransportOpts,
        cowboy_clear,
        ProtocolOpts
    ),
    
    ChildSpecs = [CowboySpec],
    {ok, {SupFlags, ChildSpecs}}.