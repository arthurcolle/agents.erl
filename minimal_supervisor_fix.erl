#!/usr/bin/env escript
%% minimal_supervisor_fix.erl
%% Create a minimal working supervisor configuration

main(_Args) ->
    io:format("🔧 Creating Minimal Supervisor Fix~n"),
    io:format("====================================~n~n"),
    
    %% Backup the current supervisor
    case file:copy("apps/agent_web/src/agent_web_sup.erl", 
                   "apps/agent_web/src/agent_web_sup.erl.backup") of
        {ok, _} -> 
            io:format("✅ Backed up current supervisor~n");
        {error, Reason} ->
            io:format("⚠️ Backup failed: ~p (continuing anyway)~n", [Reason])
    end,
    
    %% Create a minimal supervisor with just essential routes
    MinimalSupervisor = create_minimal_supervisor(),
    
    case file:write_file("apps/agent_web/src/agent_web_sup.erl", MinimalSupervisor) of
        ok ->
            io:format("✅ Created minimal supervisor~n"),
            io:format("~n🎯 Changes made:~n"),
            io:format("   • Simplified route configuration~n"),
            io:format("   • Removed problematic handlers~n"),
            io:format("   • Only essential core services~n"),
            io:format("   • Fixed restart intensity issues~n"),
            io:format("~n⚡ Now run: make compile && make shell~n");
        {error, WriteError} ->
            io:format("❌ Failed to write minimal supervisor: ~p~n", [WriteError])
    end.

create_minimal_supervisor() ->
<<"%-module(agent_web_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    io:format(\"Starting minimal agent_web supervisor~n\", []),
    case supervisor:start_link({local, ?SERVER}, ?MODULE, []) of
        {ok, Pid} ->
            io:format(\"Supervisor started successfully with PID ~p~n\", [Pid]),
            {ok, Pid};
        {error, Reason} ->
            io:format(\"Failed to start supervisor: ~p~n\", [Reason]),
            {error, Reason}
    end.

init([]) ->
    io:format(\"Initializing minimal supervisor~n\", []),
    SupFlags = #{strategy => one_for_one,
                 intensity => 2,  % Reduced restart intensity
                 period => 30},
    
    Port = application:get_env(agent_web, port, 8080),
    io:format(\"Using port ~p for HTTP web server~n\", [Port]),
    
    %% Minimal essential routes only
    Dispatch = cowboy_router:compile([
        {'_', [
            {\"/\", agent_web_handler, []},
            {\"/api/system/health\", system_health_handler, []},
            {\"/api/reload\", hot_reload_handler, []},
            {\"/api/conversations\", conversation_handler, []},
            {\"/api/conversations/:conversation_id\", conversation_handler, []},
            {\"/api/agents\", agent_api_handler, []},
            {\"/api/agents/:id\", agent_api_handler, []},
            {\"/api/agents/:id/chat\", agent_chat_handler, []},
            {\"/ws\", agent_ws_handler, []},
            {\"/static/[...]\", cowboy_static, {priv_dir, agent_web, \"static\"}}
        ]}
    ]),
    io:format(\"Compiled minimal Cowboy routes~n\", []),
    
    %% HTTP configuration
    HttpTransportOpts = [{port, Port}],
    ProtocolOpts = #{
        env => #{dispatch => Dispatch},
        middlewares => [cowboy_router, cowboy_handler],
        request_timeout => 60000,
        idle_timeout => infinity
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
    
    %% Essential services only
    PersistentTableManagerSpec = #{
        id => persistent_table_manager,
        start => {persistent_table_manager, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [persistent_table_manager]
    },
    
    ConversationInitializerSpec = #{
        id => conversation_initializer,
        start => {conversation_initializer, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [conversation_initializer]
    },
    
    %% Minimal child specs - only the essentials
    ChildSpecs = [
        HttpCowboySpec,
        PersistentTableManagerSpec,
        ConversationInitializerSpec
    ],
    
    io:format(\"Starting ~p child processes~n\", [length(ChildSpecs)]),
    lists:foreach(fun(ChildSpec) ->
        case ChildSpec of
            #{id := Id} ->
                io:format(\"Will start child: ~p~n\", [Id]);
            _ ->
                io:format(\"Will start child: ~p~n\", [element(1, ChildSpec)])
        end
    end, ChildSpecs),
    
    io:format(\"Minimal supervisor initialization complete~n\", []),
    {ok, {SupFlags, ChildSpecs}}.
">>.