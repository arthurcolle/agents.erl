-module(mcp_advanced_config).
-behaviour(gen_server).

%% API
-export([start_link/0,
         find_available_port/1,
         get_port_allocation/1,
         release_port/1,
         get_all_allocations/0,
         configure_server/1,
         get_server_config/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT_RANGE, {8700, 8799}).
-define(PORT_CHECK_TIMEOUT, 100).

-record(state, {
    port_allocations = #{},      % port => {server_id, allocated_at}
    server_configs = #{},        % server_id => config
    port_range = ?DEFAULT_PORT_RANGE
}).

%% API Functions
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

find_available_port(ServerId) ->
    gen_server:call(?SERVER, {find_available_port, ServerId}).

get_port_allocation(Port) ->
    gen_server:call(?SERVER, {get_port_allocation, Port}).

release_port(Port) ->
    gen_server:call(?SERVER, {release_port, Port}).

get_all_allocations() ->
    gen_server:call(?SERVER, get_all_allocations).

configure_server(Config) ->
    gen_server:call(?SERVER, {configure_server, Config}).

get_server_config(ServerId) ->
    gen_server:call(?SERVER, {get_server_config, ServerId}).

%% gen_server callbacks
init([]) ->
    process_flag(trap_exit, true),
    State = #state{},
    % Check and allocate existing ports
    CheckedState = check_existing_ports(State),
    {ok, CheckedState}.

handle_call({find_available_port, ServerId}, _From, State) ->
    {StartPort, EndPort} = State#state.port_range,
    case find_free_port(StartPort, EndPort, State#state.port_allocations) of
        {ok, Port} ->
            NewAllocations = maps:put(Port, {ServerId, erlang:system_time(second)}, 
                                     State#state.port_allocations),
            NewState = State#state{port_allocations = NewAllocations},
            {reply, {ok, Port}, NewState};
        {error, no_free_ports} ->
            {reply, {error, no_free_ports}, State}
    end;

handle_call({get_port_allocation, Port}, _From, State) ->
    Reply = maps:find(Port, State#state.port_allocations),
    {reply, Reply, State};

handle_call({release_port, Port}, _From, State) ->
    NewAllocations = maps:remove(Port, State#state.port_allocations),
    NewState = State#state{port_allocations = NewAllocations},
    {reply, ok, NewState};

handle_call(get_all_allocations, _From, State) ->
    {reply, State#state.port_allocations, State};

handle_call({configure_server, Config}, _From, State) ->
    ServerId = maps:get(server_id, Config),
    % Auto-allocate port if not specified or if port is in use
    {ConfigWithPort, UpdatedState} = case maps:get(port, Config, undefined) of
        undefined ->
            {StartPort, EndPort} = State#state.port_range,
            case find_free_port(StartPort, EndPort, State#state.port_allocations) of
                {ok, Port} ->
                    NewAllocations = maps:put(Port, {ServerId, erlang:system_time(second)}, 
                                             State#state.port_allocations),
                    NewState = State#state{port_allocations = NewAllocations},
                    {Config#{port => Port}, NewState};
                {error, no_free_ports} -> 
                    {Config, State}
            end;
        ExistingPort ->
            case is_port_available(ExistingPort) of
                true -> {Config, State};
                false ->
                    io:format("[MCP_CONFIG] Port ~p in use, finding alternative~n", [ExistingPort]),
                    {StartPort, EndPort} = State#state.port_range,
                    case find_free_port(StartPort, EndPort, State#state.port_allocations) of
                        {ok, NewPort} -> 
                            io:format("[MCP_CONFIG] Allocated port ~p for server ~s~n", 
                                     [NewPort, ServerId]),
                            NewAllocations = maps:put(NewPort, {ServerId, erlang:system_time(second)}, 
                                                     State#state.port_allocations),
                            NewState = State#state{port_allocations = NewAllocations},
                            {Config#{port => NewPort}, NewState};
                        {error, no_free_ports} -> 
                            {Config, State}
                    end
            end
    end,
    
    NewConfigs = maps:put(ServerId, ConfigWithPort, UpdatedState#state.server_configs),
    FinalState = UpdatedState#state{server_configs = NewConfigs},
    {reply, {ok, ConfigWithPort}, FinalState};

handle_call({get_server_config, ServerId}, _From, State) ->
    Reply = maps:find(ServerId, State#state.server_configs),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
find_free_port(Current, End, _Allocations) when Current > End ->
    {error, no_free_ports};
find_free_port(Current, End, Allocations) ->
    case maps:is_key(Current, Allocations) of
        true ->
            find_free_port(Current + 1, End, Allocations);
        false ->
            case is_port_available(Current) of
                true -> {ok, Current};
                false -> find_free_port(Current + 1, End, Allocations)
            end
    end.

is_port_available(Port) ->
    case gen_tcp:listen(Port, [{reuseaddr, true}]) of
        {ok, Socket} ->
            gen_tcp:close(Socket),
            true;
        {error, eaddrinuse} ->
            false;
        {error, _} ->
            false
    end.

find_available_port_internal(ServerId, State) ->
    #state{port_allocations = Allocations, port_range = {Start, End}} = State,
    case find_free_port(Start, End, Allocations) of
        {ok, Port} ->
            Timestamp = erlang:system_time(second),
            NewAllocations = maps:put(Port, {ServerId, Timestamp}, Allocations),
            NewState = State#state{port_allocations = NewAllocations},
            {ok, Port, NewState};
        {error, no_free_ports} ->
            {error, no_free_ports}
    end.

check_existing_ports(State) ->
    % Check known ports that might be in use
    KnownPorts = [8767, 8080, 8443],
    Allocations = lists:foldl(
        fun(Port, Acc) ->
            case is_port_available(Port) of
                false ->
                    maps:put(Port, {<<"system">>, erlang:system_time(second)}, Acc);
                true ->
                    Acc
            end
        end,
        State#state.port_allocations,
        KnownPorts
    ),
    State#state{port_allocations = Allocations}.