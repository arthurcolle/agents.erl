-module(mcp_registry).
-behaviour(gen_server).

-export([start_link/0, register_server/3, unregister_server/1, list_servers/0,
         get_server/1, update_server/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Enhanced logging utility with line numbers
-define(LOG(Level, Format, Args), 
    io:format("[~s:~p] [~s] " ++ Format ++ "~n", 
              [?MODULE, ?LINE, Level | Args])).

-define(LOG_INFO(Format, Args), ?LOG("INFO", Format, Args)).
-define(LOG_WARN(Format, Args), ?LOG("WARN", Format, Args)).
-define(LOG_ERROR(Format, Args), ?LOG("ERROR", Format, Args)).
-define(LOG_DEBUG(Format, Args), ?LOG("DEBUG", Format, Args)).

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
    ?LOG_INFO("Starting MCP registry", []),
    case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
        {ok, Pid} ->
            ?LOG_INFO("Registry started successfully with PID ~p", [Pid]),
            {ok, Pid};
        {error, Reason} ->
            ?LOG_ERROR("Failed to start MCP registry: ~p", [Reason]),
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
    ?LOG_INFO("Initializing registry state", []),
    {ok, #state{}}.

handle_call({register_server, Name, Url, Config}, _From, State) ->
    ServerId = integer_to_binary(State#state.next_id),
    ?LOG_INFO("Registering server: ~s (~s) with URL: ~s", [Name, ServerId, Url]),
    ?LOG_DEBUG("Server config: ~p", [Config]),
    
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
    
    ?LOG_INFO("Successfully registered server ~s with ID: ~s", [Name, ServerId]),
    
    % Notify websocket clients about new server
    try
        agent_ws_handler:broadcast(#{
            type => <<"mcp_server_registered">>,
            server => server_to_map(Server)
        }),
        ?LOG_DEBUG("Notified websocket clients about new server: ~s", [ServerId])
    catch
        Class:Error:Stack ->
            ?LOG_WARN("Failed to notify websocket clients about server ~s: ~p:~p", [ServerId, Class, Error])
    end,
    
    {reply, {ok, ServerId}, NewState};

handle_call({unregister_server, ServerId}, _From, State) ->
    ?LOG_INFO("Unregistering server: ~s", [ServerId]),
    case maps:take(ServerId, State#state.servers) of
        {Server, NewServers} ->
            ServerName = Server#mcp_server.name,
            ?LOG_INFO("Successfully unregistered server ~s (~s)", [ServerName, ServerId]),
            NewState = State#state{servers = NewServers},
            
            % Notify websocket clients about server removal
            try
                agent_ws_handler:broadcast(#{
                    type => <<"mcp_server_unregistered">>,
                    server_id => ServerId
                }),
                ?LOG_DEBUG("Notified websocket clients about server removal: ~s", [ServerId])
            catch
                Class:Error:Stack ->
                    ?LOG_WARN("Failed to notify websocket clients about server removal ~s: ~p:~p", [ServerId, Class, Error])
            end,
            
            {reply, ok, NewState};
        error ->
            ?LOG_WARN("Cannot unregister - server not found: ~s", [ServerId]),
            {reply, {error, not_found}, State}
    end;

handle_call(list_servers, _From, State) ->
    ServerCount = maps:size(State#state.servers),
    ?LOG_DEBUG("Listing ~p registered servers", [ServerCount]),
    ServerList = [server_to_map(Server) || Server <- maps:values(State#state.servers)],
    {reply, ServerList, State};

handle_call({get_server, ServerId}, _From, State) ->
    ?LOG_DEBUG("Getting server details for: ~s", [ServerId]),
    case maps:find(ServerId, State#state.servers) of
        {ok, Server} ->
            ?LOG_DEBUG("Found server: ~s", [ServerId]),
            {reply, {ok, server_to_map(Server)}, State};
        error ->
            ?LOG_DEBUG("Server not found in registry: ~s", [ServerId]),
            {reply, {error, not_found}, State}
    end;

handle_call({update_server, ServerId, Config}, _From, State) ->
    ?LOG_INFO("Updating server configuration: ~s", [ServerId]),
    ?LOG_DEBUG("Update config: ~p", [Config]),
    case maps:find(ServerId, State#state.servers) of
        {ok, Server} ->
            UpdatedServer = Server#mcp_server{config = Config},
            NewServers = maps:put(ServerId, UpdatedServer, State#state.servers),
            NewState = State#state{servers = NewServers},
            
            ?LOG_INFO("Successfully updated server: ~s", [ServerId]),
            
            % Notify websocket clients about server update
            try
                agent_ws_handler:broadcast(#{
                    type => <<"mcp_server_updated">>,
                    server => server_to_map(UpdatedServer)
                }),
                ?LOG_DEBUG("Notified websocket clients about server update: ~s", [ServerId])
            catch
                Class:Error:Stack ->
                    ?LOG_WARN("Failed to notify websocket clients about server update ~s: ~p:~p", [ServerId, Class, Error])
            end,
            
            {reply, ok, NewState};
        error ->
            ?LOG_WARN("Cannot update - server not found: ~s", [ServerId]),
            {reply, {error, not_found}, State}
    end.

handle_cast({update_server_status, ServerId, Status}, State) ->
    ?LOG_DEBUG("Updating server status: ~s -> ~p", [ServerId, Status]),
    case maps:find(ServerId, State#state.servers) of
        {ok, Server} ->
            UpdatedServer = Server#mcp_server{
                status = Status,
                last_seen = erlang:system_time(second)
            },
            NewServers = maps:put(ServerId, UpdatedServer, State#state.servers),
            NewState = State#state{servers = NewServers},
            
            ?LOG_DEBUG("Updated server ~s status to: ~p", [ServerId, Status]),
            
            % Notify websocket clients about status change
            try
                agent_ws_handler:broadcast(#{
                    type => <<"mcp_server_status_changed">>,
                    server_id => ServerId,
                    status => atom_to_binary(Status)
                }),
                ?LOG_DEBUG("Notified websocket clients about status change: ~s -> ~p", [ServerId, Status])
            catch
                Class:Error:Stack ->
                    ?LOG_WARN("Failed to notify websocket clients about status change ~s: ~p:~p", [ServerId, Class, Error])
            end,
            
            {noreply, NewState};
        error ->
            ?LOG_WARN("Cannot update status - server not found: ~s", [ServerId]),
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