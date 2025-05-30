-module(mcp_registry).
-behaviour(gen_server).

-export([start_link/0, register_server/3, unregister_server/1, list_servers/0,
         get_server/1, update_server/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    servers = #{},
    next_id = 1
}).

-record(mcp_server, {
    id,
    name,
    url,
    config,
    status = disconnected,
    registered_at,
    last_seen
}).

start_link() ->
    io:format("[MCP_REG] Starting MCP registry~n"),
    case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
        {ok, Pid} ->
            io:format("[MCP_REG] Registry started successfully with PID ~p~n", [Pid]),
            {ok, Pid};
        {error, Reason} ->
            io:format("[ERROR] Failed to start MCP registry: ~p~n", [Reason]),
            {error, Reason}
    end.

register_server(Name, Url, Config) ->
    gen_server:call(?MODULE, {register_server, Name, Url, Config}).

unregister_server(ServerId) ->
    gen_server:call(?MODULE, {unregister_server, ServerId}).

list_servers() ->
    gen_server:call(?MODULE, list_servers).

get_server(ServerId) ->
    gen_server:call(?MODULE, {get_server, ServerId}).

update_server(ServerId, Config) ->
    gen_server:call(?MODULE, {update_server, ServerId, Config}).

init([]) ->
    io:format("[MCP_REG] Initializing registry state~n"),
    {ok, #state{}}.

handle_call({register_server, Name, Url, Config}, _From, State) ->
    ServerId = integer_to_binary(State#state.next_id),
    Server = #mcp_server{
        id = ServerId,
        name = Name,
        url = Url,
        config = Config,
        registered_at = erlang:system_time(second)
    },
    NewServers = maps:put(ServerId, Server, State#state.servers),
    NewState = State#state{
        servers = NewServers,
        next_id = State#state.next_id + 1
    },
    
    % Notify websocket clients about new server
    agent_ws_handler:broadcast(#{
        type => <<"mcp_server_registered">>,
        server => server_to_map(Server)
    }),
    
    {reply, {ok, ServerId}, NewState};

handle_call({unregister_server, ServerId}, _From, State) ->
    case maps:take(ServerId, State#state.servers) of
        {Server, NewServers} ->
            NewState = State#state{servers = NewServers},
            
            % Notify websocket clients about server removal
            agent_ws_handler:broadcast(#{
                type => <<"mcp_server_unregistered">>,
                server_id => ServerId
            }),
            
            {reply, ok, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(list_servers, _From, State) ->
    ServerList = [server_to_map(Server) || Server <- maps:values(State#state.servers)],
    {reply, ServerList, State};

handle_call({get_server, ServerId}, _From, State) ->
    case maps:find(ServerId, State#state.servers) of
        {ok, Server} ->
            {reply, {ok, server_to_map(Server)}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({update_server, ServerId, Config}, _From, State) ->
    case maps:find(ServerId, State#state.servers) of
        {ok, Server} ->
            UpdatedServer = Server#mcp_server{config = Config},
            NewServers = maps:put(ServerId, UpdatedServer, State#state.servers),
            NewState = State#state{servers = NewServers},
            
            % Notify websocket clients about server update
            agent_ws_handler:broadcast(#{
                type => <<"mcp_server_updated">>,
                server => server_to_map(UpdatedServer)
            }),
            
            {reply, ok, NewState};
        error ->
            {reply, {error, not_found}, State}
    end.

handle_cast({update_server_status, ServerId, Status}, State) ->
    case maps:find(ServerId, State#state.servers) of
        {ok, Server} ->
            UpdatedServer = Server#mcp_server{
                status = Status,
                last_seen = erlang:system_time(second)
            },
            NewServers = maps:put(ServerId, UpdatedServer, State#state.servers),
            NewState = State#state{servers = NewServers},
            
            % Notify websocket clients about status change
            agent_ws_handler:broadcast(#{
                type => <<"mcp_server_status_changed">>,
                server_id => ServerId,
                status => atom_to_binary(Status)
            }),
            
            {noreply, NewState};
        error ->
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
server_to_map(#mcp_server{id = Id, name = Name, url = Url, config = Config, 
                         status = Status, registered_at = RegisteredAt, last_seen = LastSeen}) ->
    #{
        id => Id,
        name => Name,
        url => Url,
        config => Config,
        status => atom_to_binary(Status),
        registered_at => RegisteredAt,
        last_seen => LastSeen
    }.