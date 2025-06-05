%% @doc MCP Streamable HTTP Supervisor
%%
%% Supervisor for all MCP Streamable HTTP transport components
-module(mcp_streamable_http_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%% ============================================================================
%%% API Functions
%%% ============================================================================

%% @doc Start the supervisor
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Config).

%%% ============================================================================
%%% Supervisor Implementation
%%% ============================================================================

%% @doc Initialize supervisor
init(Config) ->
    % Define child specifications
    Children = [
        % Session manager
        #{
            id => mcp_http_session_manager,
            start => {mcp_http_session_manager, start_link, [Config]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [mcp_http_session_manager]
        },
        
        % Message buffer
        #{
            id => mcp_http_message_buffer,
            start => {mcp_http_message_buffer, start_link, [Config]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [mcp_http_message_buffer]
        },
        
        % Main transport server
        #{
            id => mcp_transport_streamable_http,
            start => {mcp_transport_streamable_http, start_link, [Config]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [mcp_transport_streamable_http]
        }
    ],
    
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    
    {ok, {SupFlags, Children}}.