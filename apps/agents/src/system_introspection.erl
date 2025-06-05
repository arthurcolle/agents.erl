-module(system_introspection).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    get_system_state/0,
    get_agent_context/1,
    get_agent_peers/1,
    get_agent_supervisor/1,
    get_system_metrics/0,
    get_running_processes/0,
    get_process_relationships/1,
    get_system_capabilities/0,
    get_agent_lineage/1,
    get_communication_paths/1,
    analyze_system_topology/0,
    get_resource_usage/0,
    get_system_history/0,
    register_system_event/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    system_events = [] :: list(),
    start_time :: erlang:timestamp(),
    event_limit = 1000 :: integer()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Get complete system state
get_system_state() ->
    gen_server:call(?MODULE, get_system_state, 10000).

%% Get context for a specific agent (where it is in the system)
get_agent_context(AgentPid) when is_pid(AgentPid) ->
    gen_server:call(?MODULE, {get_agent_context, AgentPid}).

%% Get peer agents (siblings under same supervisor)
get_agent_peers(AgentPid) when is_pid(AgentPid) ->
    gen_server:call(?MODULE, {get_agent_peers, AgentPid}).

%% Get the supervisor of an agent
get_agent_supervisor(AgentPid) when is_pid(AgentPid) ->
    gen_server:call(?MODULE, {get_agent_supervisor, AgentPid}).

%% Get system-wide metrics
get_system_metrics() ->
    gen_server:call(?MODULE, get_system_metrics).

%% Get all running processes categorized by type
get_running_processes() ->
    gen_server:call(?MODULE, get_running_processes).

%% Get relationships for a process (supervisor, siblings, children)
get_process_relationships(Pid) when is_pid(Pid) ->
    gen_server:call(?MODULE, {get_process_relationships, Pid}).

%% Get system capabilities (available tools, services, etc.)
get_system_capabilities() ->
    gen_server:call(?MODULE, get_system_capabilities).

%% Get lineage of an agent (supervisor chain up to root)
get_agent_lineage(AgentPid) when is_pid(AgentPid) ->
    gen_server:call(?MODULE, {get_agent_lineage, AgentPid}).

%% Get possible communication paths from an agent
get_communication_paths(AgentPid) when is_pid(AgentPid) ->
    gen_server:call(?MODULE, {get_communication_paths, AgentPid}).

%% Analyze the system topology
analyze_system_topology() ->
    gen_server:call(?MODULE, analyze_system_topology, 10000).

%% Get resource usage information
get_resource_usage() ->
    gen_server:call(?MODULE, get_resource_usage).

%% Get system history
get_system_history() ->
    gen_server:call(?MODULE, get_system_history).

%% Register a system event
register_system_event(EventType, EventData) ->
    gen_server:cast(?MODULE, {register_event, EventType, EventData}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    % Start monitoring system events
    process_flag(trap_exit, true),
    {ok, #state{start_time = erlang:timestamp()}}.

handle_call(get_system_state, _From, State) ->
    SystemState = #{
        node => node(),
        uptime => calculate_uptime(State#state.start_time),
        applications => application:which_applications(),
        total_processes => length(processes()),
        schedulers => erlang:system_info(schedulers),
        memory => erlang:memory(),
        supervision_tree => get_full_supervision_tree(),
        agents => get_all_agents(),
        services => get_all_services(),
        capabilities => get_system_capabilities_internal()
    },
    {reply, {ok, SystemState}, State};

handle_call({get_agent_context, AgentPid}, _From, State) ->
    Context = build_agent_context(AgentPid),
    {reply, {ok, Context}, State};

handle_call({get_agent_peers, AgentPid}, _From, State) ->
    Peers = find_agent_peers(AgentPid),
    {reply, {ok, Peers}, State};

handle_call({get_agent_supervisor, AgentPid}, _From, State) ->
    Supervisor = find_process_supervisor(AgentPid),
    {reply, {ok, Supervisor}, State};

handle_call(get_system_metrics, _From, State) ->
    Metrics = collect_system_metrics(),
    {reply, {ok, Metrics}, State};

handle_call(get_running_processes, _From, State) ->
    Processes = categorize_processes(),
    {reply, {ok, Processes}, State};

handle_call({get_process_relationships, Pid}, _From, State) ->
    Relationships = analyze_process_relationships(Pid),
    {reply, {ok, Relationships}, State};

handle_call(get_system_capabilities, _From, State) ->
    Capabilities = get_system_capabilities_internal(),
    {reply, {ok, Capabilities}, State};

handle_call({get_agent_lineage, AgentPid}, _From, State) ->
    Lineage = trace_agent_lineage(AgentPid),
    {reply, {ok, Lineage}, State};

handle_call({get_communication_paths, AgentPid}, _From, State) ->
    Paths = analyze_communication_paths(AgentPid),
    {reply, {ok, Paths}, State};

handle_call(analyze_system_topology, _From, State) ->
    Topology = build_system_topology(),
    {reply, {ok, Topology}, State};

handle_call(get_resource_usage, _From, State) ->
    Usage = collect_resource_usage(),
    {reply, {ok, Usage}, State};

handle_call(get_system_history, _From, State) ->
    History = lists:reverse(State#state.system_events),
    {reply, {ok, History}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({register_event, EventType, EventData}, State) ->
    Event = #{
        type => EventType,
        data => EventData,
        timestamp => erlang:timestamp(),
        node => node()
    },
    NewEvents = [Event | State#state.system_events],
    % Keep only the last N events
    TrimmedEvents = case length(NewEvents) > State#state.event_limit of
        true -> lists:sublist(NewEvents, State#state.event_limit);
        false -> NewEvents
    end,
    {noreply, State#state{system_events = TrimmedEvents}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

calculate_uptime(StartTime) ->
    Now = erlang:timestamp(),
    timer:now_diff(Now, StartTime) div 1000000. % Convert to seconds

get_full_supervision_tree() ->
    RootSupervisors = [agent_web_sup, openai_sup],
    Trees = lists:map(fun(Sup) ->
        case whereis(Sup) of
            undefined -> {Sup, not_running};
            Pid -> {Sup, build_supervision_subtree(Pid)}
        end
    end, RootSupervisors),
    
    % Also get dynamic supervisors
    case dynamic_supervisor_manager:get_supervision_tree() of
        {ok, DynamicTree} -> [{dynamic, DynamicTree} | Trees];
        _ -> Trees
    end.

build_supervision_subtree(SupPid) ->
    try
        Children = supervisor:which_children(SupPid),
        ChildInfo = lists:map(fun({Id, ChildPid, Type, _Modules}) ->
            case Type of
                supervisor when is_pid(ChildPid) ->
                    #{
                        id => Id,
                        pid => ChildPid,
                        type => supervisor,
                        children => build_supervision_subtree(ChildPid)
                    };
                _ ->
                    #{
                        id => Id,
                        pid => ChildPid,
                        type => Type,
                        info => get_process_info(ChildPid)
                    }
            end
        end, Children),
        #{
            pid => SupPid,
            children => ChildInfo,
            count => supervisor:count_children(SupPid)
        }
    catch
        _:_ -> #{pid => SupPid, error => failed_to_inspect}
    end.

get_process_info(Pid) when is_pid(Pid) ->
    try
        Info = process_info(Pid, [registered_name, current_function, 
                                  message_queue_len, memory, reductions]),
        maps:from_list(Info)
    catch
        _:_ -> #{error => not_accessible}
    end;
get_process_info(_) ->
    #{error => not_a_pid}.

get_all_agents() ->
    % Get agents from the agent registry
    case whereis(agent) of
        undefined -> [];
        AgentPid ->
            case catch gen_server:call(AgentPid, list_agents) of
                {ok, Agents} -> Agents;
                _ -> []
            end
    end.

get_all_services() ->
    % List all registered services
    RegisteredNames = registered(),
    Services = lists:filter(fun(Name) ->
        case atom_to_list(Name) of
            "agent_" ++ _ -> false;
            "code_server" -> false;
            "kernel_" ++ _ -> false;
            "rex" -> false;
            "global_" ++ _ -> false;
            "inet_" ++ _ -> false;
            "erl_" ++ _ -> false;
            _ -> true
        end
    end, RegisteredNames),
    
    lists:map(fun(Service) ->
        Pid = whereis(Service),
        #{
            name => Service,
            pid => Pid,
            info => get_process_info(Pid)
        }
    end, Services).

build_agent_context(AgentPid) ->
    % Get the agent's place in the system
    #{
        pid => AgentPid,
        node => node(AgentPid),
        supervisor => find_process_supervisor(AgentPid),
        siblings => find_agent_peers(AgentPid),
        state => get_agent_state(AgentPid),
        capabilities => get_agent_capabilities(AgentPid),
        relationships => analyze_process_relationships(AgentPid),
        lineage => trace_agent_lineage(AgentPid)
    }.

find_process_supervisor(Pid) ->
    % Walk up the supervision tree to find the supervisor
    case process_info(Pid, links) of
        {links, Links} ->
            PotentialSups = lists:filter(fun(LinkedPid) ->
                case process_info(LinkedPid, registered_name) of
                    {registered_name, Name} ->
                        case atom_to_list(Name) of
                            "_sup" ++ _ -> true;
                            _ -> is_supervisor(LinkedPid)
                        end;
                    _ -> is_supervisor(LinkedPid)
                end
            end, Links),
            case PotentialSups of
                [Sup | _] -> Sup;
                [] -> undefined
            end;
        _ -> undefined
    end.

is_supervisor(Pid) ->
    case catch sys:get_state(Pid) of
        {state, _, _, _, _, _, _, _, _, _, _} -> true; % Supervisor state tuple
        _ -> false
    end.

find_agent_peers(AgentPid) ->
    case find_process_supervisor(AgentPid) of
        undefined -> [];
        SupPid ->
            Children = supervisor:which_children(SupPid),
            lists:filtermap(fun({_Id, ChildPid, worker, [agent_instance]}) 
                              when ChildPid =/= AgentPid, is_pid(ChildPid) ->
                {true, ChildPid};
            (_) -> false
            end, Children)
    end.

get_agent_state(AgentPid) ->
    try
        agent_instance:get_state(AgentPid)
    catch
        _:_ -> #{error => not_accessible}
    end.

get_agent_capabilities(AgentPid) ->
    case get_agent_state(AgentPid) of
        #{tools := Tools} = State ->
            #{
                tools => Tools,
                model => maps:get(model, State, unknown),
                api_preference => maps:get(api_preference, State, unknown)
            };
        _ -> #{error => not_accessible}
    end.

trace_agent_lineage(AgentPid) ->
    trace_lineage(AgentPid, []).

trace_lineage(Pid, Acc) ->
    case find_process_supervisor(Pid) of
        undefined -> lists:reverse([Pid | Acc]);
        SupPid -> trace_lineage(SupPid, [Pid | Acc])
    end.

analyze_process_relationships(Pid) ->
    #{
        supervisor => find_process_supervisor(Pid),
        siblings => find_process_siblings(Pid),
        links => get_process_links(Pid),
        monitors => get_process_monitors(Pid),
        message_queue => get_message_queue_info(Pid)
    }.

find_process_siblings(Pid) ->
    case find_process_supervisor(Pid) of
        undefined -> [];
        SupPid ->
            Children = supervisor:which_children(SupPid),
            lists:filtermap(fun({_Id, ChildPid, _, _}) 
                              when ChildPid =/= Pid, is_pid(ChildPid) ->
                {true, ChildPid};
            (_) -> false
            end, Children)
    end.

get_process_links(Pid) ->
    case process_info(Pid, links) of
        {links, Links} -> Links;
        _ -> []
    end.

get_process_monitors(Pid) ->
    case process_info(Pid, monitors) of
        {monitors, Monitors} -> Monitors;
        _ -> []
    end.

get_message_queue_info(Pid) ->
    case process_info(Pid, [message_queue_len, messages]) of
        [{message_queue_len, Len}, {messages, Messages}] ->
            #{
                length => Len,
                messages => case Len > 10 of
                    true -> lists:sublist(Messages, 10) ++ [more];
                    false -> Messages
                end
            };
        _ -> #{error => not_accessible}
    end.

analyze_communication_paths(AgentPid) ->
    % Analyze possible communication paths from this agent
    #{
        direct_calls => get_registered_processes_accessible(),
        via_supervisor => find_process_supervisor(AgentPid),
        via_peers => find_agent_peers(AgentPid),
        via_services => get_accessible_services(),
        via_tools => get_agent_tools(AgentPid),
        via_mcp => get_mcp_connections()
    }.

get_registered_processes_accessible() ->
    lists:filter(fun(Name) ->
        case atom_to_list(Name) of
            "agent" ++ _ -> true;
            "mcp" ++ _ -> true;
            "dynamic_supervisor_manager" -> true;
            "runtime_system" -> true;
            _ -> false
        end
    end, registered()).

get_accessible_services() ->
    [agent, agent_tools, mcp_manager, mcp_connection_manager, 
     dynamic_supervisor_manager, system_introspection].

get_agent_tools(AgentPid) ->
    case get_agent_state(AgentPid) of
        #{tools := Tools} -> Tools;
        _ -> []
    end.

get_mcp_connections() ->
    case whereis(mcp_connection_manager) of
        undefined -> [];
        Pid ->
            try
                gen_server:call(Pid, list_connections)
            catch
                _:_ -> []
            end
    end.

categorize_processes() ->
    AllProcesses = processes(),
    Categorized = lists:foldl(fun(Pid, Acc) ->
        Category = categorize_process(Pid),
        maps:update_with(Category, fun(L) -> [Pid | L] end, [Pid], Acc)
    end, #{}, AllProcesses),
    
    % Add counts
    maps:map(fun(_Cat, Pids) ->
        #{
            count => length(Pids),
            pids => lists:sublist(Pids, 20) % Limit to first 20
        }
    end, Categorized).

categorize_process(Pid) ->
    case process_info(Pid, registered_name) of
        {registered_name, Name} ->
            NameStr = atom_to_list(Name),
            case NameStr of
                "agent" ++ _ -> agent;
                "mcp" ++ _ -> mcp;
                "_sup" ++ _ -> supervisor;
                "cowboy" ++ _ -> web;
                "ranch" ++ _ -> web;
                _ -> 
                    case is_supervisor(Pid) of
                        true -> supervisor;
                        false -> service
                    end
            end;
        _ ->
            case process_info(Pid, initial_call) of
                {initial_call, {supervisor, _, _}} -> supervisor;
                {initial_call, {gen_server, _, _}} -> gen_server;
                {initial_call, {gen_statem, _, _}} -> gen_statem;
                {initial_call, {gen_event, _, _}} -> gen_event;
                _ -> other
            end
    end.

collect_system_metrics() ->
    #{
        process_count => length(processes()),
        run_queue => statistics(run_queue),
        reductions => element(1, statistics(reductions)),
        memory => erlang:memory(),
        io => erlang:statistics(io),
        garbage_collection => erlang:statistics(garbage_collection),
        scheduler_usage => agent_scheduler:utilization(1)
    }.

get_system_capabilities_internal() ->
    #{
        tools => get_available_tools(),
        services => get_available_services(),
        apis => get_available_apis(),
        mcp_servers => get_mcp_servers(),
        supervisors => get_supervisor_list()
    }.

get_available_tools() ->
    case whereis(agent_tools) of
        undefined -> [];
        Pid ->
            try
                gen_server:call(Pid, list_tools)
            catch
                _:_ -> []
            end
    end.

get_available_services() ->
    [S || S <- registered(), is_service(S)].

is_service(Name) ->
    NameStr = atom_to_list(Name),
    lists:any(fun(Prefix) ->
        lists:prefix(Prefix, NameStr)
    end, ["agent", "mcp", "oauth", "system", "runtime"]).

get_available_apis() ->
    [chat, responses, mcp, websocket, rest].

get_mcp_servers() ->
    case whereis(mcp_manager) of
        undefined -> [];
        Pid ->
            try
                gen_server:call(Pid, list_servers)
            catch
                _:_ -> []
            end
    end.

get_supervisor_list() ->
    RootSups = [agent_web_sup, openai_sup],
    DynamicSups = case dynamic_supervisor_manager:list_supervisors() of
        {ok, List} -> [maps:get(name, S) || S <- List];
        _ -> []
    end,
    RootSups ++ DynamicSups.

build_system_topology() ->
    % Build a comprehensive topology map
    #{
        nodes => [node() | nodes()],
        supervision_hierarchy => get_full_supervision_tree(),
        agent_network => build_agent_network(),
        service_dependencies => analyze_service_dependencies(),
        communication_graph => build_communication_graph()
    }.

build_agent_network() ->
    case get_all_agents() of
        [] -> #{};
        Agents ->
            Network = lists:map(fun(Agent) ->
                AgentId = maps:get(id, Agent),
                AgentPid = maps:get(pid, Agent),
                #{
                    id => AgentId,
                    pid => AgentPid,
                    connections => find_agent_connections(AgentPid)
                }
            end, Agents),
            #{
                agents => Network,
                total => length(Agents)
            }
    end.

find_agent_connections(AgentPid) ->
    % Find connections to other agents
    Links = get_process_links(AgentPid),
    lists:filtermap(fun(LinkedPid) ->
        case is_agent_process(LinkedPid) of
            true -> {true, LinkedPid};
            false -> false
        end
    end, Links).

is_agent_process(Pid) ->
    case process_info(Pid, initial_call) of
        {initial_call, {agent_instance, _, _}} -> true;
        _ -> false
    end.

analyze_service_dependencies() ->
    % Analyze which services depend on which
    Services = get_available_services(),
    lists:map(fun(Service) ->
        case whereis(Service) of
            undefined -> {Service, []};
            Pid ->
                Links = get_process_links(Pid),
                Deps = lists:filtermap(fun(LinkedPid) ->
                    case process_info(LinkedPid, registered_name) of
                        {registered_name, Name} when Name =/= Service ->
                            case is_service(Name) of
                                true -> {true, Name};
                                false -> false
                            end;
                        _ -> false
                    end
                end, Links),
                {Service, Deps}
        end
    end, Services).

build_communication_graph() ->
    % Build a graph of potential communication paths
    AllProcesses = get_named_processes(),
    Graph = lists:map(fun({Name, Pid}) ->
        {Name, #{
            pid => Pid,
            can_communicate_with => find_communication_targets(Name, Pid)
        }}
    end, AllProcesses),
    maps:from_list(Graph).

get_named_processes() ->
    [{Name, whereis(Name)} || Name <- registered(), whereis(Name) =/= undefined].

find_communication_targets(FromName, FromPid) ->
    % Determine which processes this one can communicate with
    Targets = case atom_to_list(FromName) of
        "agent" ++ _ ->
            % Agents can talk to tools, MCP, other agents
            [agent, agent_tools, mcp_manager, dynamic_supervisor_manager];
        "mcp" ++ _ ->
            % MCP services can talk to agents and each other
            get_available_services();
        _ ->
            % Other services have specific patterns
            []
    end,
    lists:filter(fun(T) -> T =/= FromName end, Targets).

collect_resource_usage() ->
    #{
        cpu => get_cpu_usage(),
        memory => get_memory_details(),
        disk => get_disk_usage(),
        network => get_network_stats(),
        processes => get_process_resource_usage()
    }.

get_cpu_usage() ->
    SchedulerUsage = agent_scheduler:utilization(1),
    #{
        scheduler_usage => SchedulerUsage,
        run_queue => statistics(run_queue),
        reductions_since_last => element(1, statistics(reductions))
    }.

get_memory_details() ->
    Memory = erlang:memory(),
    #{
        total => proplists:get_value(total, Memory),
        processes => proplists:get_value(processes, Memory),
        ets => proplists:get_value(ets, Memory),
        atom => proplists:get_value(atom, Memory),
        code => proplists:get_value(code, Memory),
        system => proplists:get_value(system, Memory)
    }.

get_disk_usage() ->
    % Placeholder - would need OS-specific implementation
    #{available => true}.

get_network_stats() ->
    {Input, Output} = statistics(io),
    #{
        bytes_in => Input,
        bytes_out => Output
    }.

get_process_resource_usage() ->
    Processes = processes(),
    TopProcesses = lists:sublist(
        lists:reverse(
            lists:sort(
                fun(A, B) ->
                    AMemory = element(2, process_info(A, memory)),
                    BMemory = element(2, process_info(B, memory)),
                    AMemory =< BMemory
                end,
                Processes
            )
        ),
        10
    ),
    
    lists:map(fun(Pid) ->
        Info = process_info(Pid, [registered_name, memory, reductions, 
                                  message_queue_len, current_function]),
        maps:from_list(Info)
    end, TopProcesses).