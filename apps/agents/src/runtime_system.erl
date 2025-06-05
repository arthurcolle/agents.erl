-module(runtime_system).

%% Shell convenience functions for runtime system augmentation
-export([
    % Supervisor management
    create_sup/2,
    create_sup/3,
    stop_sup/1,
    list_sups/0,
    add_child/3,
    add_child/4,
    remove_child/2,
    sup_info/1,
    tree/0,
    
    % Agent management with supervisors
    spawn_agent_under/2,
    spawn_agent_under/3,
    spawn_agent_group/3,
    
    % System augmentation
    augment/2,
    augment/3,
    add_service/2,
    add_service/3,
    
    % Inspection
    inspect/0,
    inspect/1,
    health/0,
    
    % Hot code loading helpers
    reload/1,
    reload_all/0
]).

%%====================================================================
%% Supervisor Management
%%====================================================================

%% Create a supervisor with default flags
create_sup(Name, Strategy) ->
    create_sup(Name, Strategy, []).

%% Create a supervisor with optional initial children
create_sup(Name, Strategy, Children) when is_atom(Name), is_atom(Strategy) ->
    SupFlags = #{
        strategy => Strategy,
        intensity => 10,
        period => 60
    },
    case dynamic_supervisor_manager:create_supervisor(Name, SupFlags, Children) of
        {ok, Pid} ->
            io:format("✓ Created supervisor ~p with PID ~p~n", [Name, Pid]),
            {ok, Pid};
        {error, Reason} ->
            io:format("✗ Failed to create supervisor: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Stop a supervisor
stop_sup(Name) when is_atom(Name) ->
    case dynamic_supervisor_manager:stop_supervisor(Name) of
        ok ->
            io:format("✓ Stopped supervisor ~p~n", [Name]),
            ok;
        {error, Reason} ->
            io:format("✗ Failed to stop supervisor: ~p~n", [Reason]),
            {error, Reason}
    end.

%% List all supervisors
list_sups() ->
    case dynamic_supervisor_manager:list_supervisors() of
        {ok, Supervisors} ->
            io:format("~nActive Supervisors:~n"),
            io:format("==================~n"),
            lists:foreach(fun(#{name := Name, pid := Pid, children := Children}) ->
                io:format("• ~p (PID: ~p) - ~p children~n", [Name, Pid, length(Children)])
            end, Supervisors),
            ok;
        {error, Reason} ->
            io:format("✗ Failed to list supervisors: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Add a child to a supervisor
add_child(SupName, ChildId, Module) ->
    add_child(SupName, ChildId, Module, []).

add_child(SupName, ChildId, Module, Args) when is_atom(SupName), is_atom(ChildId), is_atom(Module) ->
    ChildSpec = #{
        id => ChildId,
        start => {Module, start_link, Args},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [Module]
    },
    case dynamic_supervisor_manager:add_child_to_supervisor(SupName, ChildId, ChildSpec) of
        {ok, Pid} ->
            io:format("✓ Added child ~p to supervisor ~p (PID: ~p)~n", [ChildId, SupName, Pid]),
            {ok, Pid};
        {error, Reason} ->
            io:format("✗ Failed to add child: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Remove a child from a supervisor
remove_child(SupName, ChildId) when is_atom(SupName), is_atom(ChildId) ->
    case dynamic_supervisor_manager:remove_child_from_supervisor(SupName, ChildId) of
        ok ->
            io:format("✓ Removed child ~p from supervisor ~p~n", [ChildId, SupName]),
            ok;
        {error, Reason} ->
            io:format("✗ Failed to remove child: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Get supervisor info
sup_info(Name) when is_atom(Name) ->
    case dynamic_supervisor_manager:get_supervisor_info(Name) of
        {ok, Info} ->
            #{name := Name, pid := Pid, children := Children, child_count := Count} = Info,
            io:format("~nSupervisor: ~p~n", [Name]),
            io:format("PID: ~p~n", [Pid]),
            io:format("Child Count: ~p~n", [Count]),
            io:format("Children:~n"),
            lists:foreach(fun({Id, ChildPid, Type, _}) ->
                io:format("  • ~p (~p) - PID: ~p~n", [Id, Type, ChildPid])
            end, Children),
            ok;
        {error, Reason} ->
            io:format("✗ Failed to get supervisor info: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Display supervision tree
tree() ->
    case dynamic_supervisor_manager:get_supervision_tree() of
        {ok, Tree} ->
            io:format("~nSupervision Tree:~n"),
            io:format("=================~n"),
            print_tree(Tree, 0),
            ok;
        {error, Reason} ->
            io:format("✗ Failed to get supervision tree: ~p~n", [Reason]),
            {error, Reason}
    end.

%%====================================================================
%% Agent Management with Supervisors
%%====================================================================

%% Spawn an agent under a specific supervisor
spawn_agent_under(SupName, AgentConfig) ->
    spawn_agent_under(SupName, make_ref(), AgentConfig).

spawn_agent_under(SupName, AgentId, AgentConfig) when is_atom(SupName), is_atom(AgentId) ->
    ChildSpec = #{
        id => AgentId,
        start => {agent_instance, start_link, [AgentConfig]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [agent_instance]
    },
    case dynamic_supervisor_manager:add_child_to_supervisor(SupName, AgentId, ChildSpec) of
        {ok, Pid} ->
            io:format("✓ Spawned agent ~p under supervisor ~p (PID: ~p)~n", [AgentId, SupName, Pid]),
            {ok, Pid};
        {error, Reason} ->
            io:format("✗ Failed to spawn agent: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Spawn a group of agents under a new supervisor
spawn_agent_group(GroupName, NumAgents, AgentConfigTemplate) when is_atom(GroupName), is_integer(NumAgents) ->
    % Create supervisor for the group
    case create_sup(GroupName, one_for_one) of
        {ok, _SupPid} ->
            % Spawn agents under the supervisor
            Results = lists:map(fun(N) ->
                AgentId = list_to_atom(atom_to_list(GroupName) ++ "_agent_" ++ integer_to_list(N)),
                AgentConfig = maps:put(id, atom_to_binary(AgentId, utf8), AgentConfigTemplate),
                spawn_agent_under(GroupName, AgentId, AgentConfig)
            end, lists:seq(1, NumAgents)),
            
            SuccessCount = length([ok || {ok, _} <- Results]),
            io:format("~n✓ Created agent group ~p with ~p/~p agents~n", [GroupName, SuccessCount, NumAgents]),
            {ok, GroupName, Results};
        Error ->
            Error
    end.

%%====================================================================
%% System Augmentation
%%====================================================================

%% Augment the system with a new service
augment(ServiceName, ServiceModule) ->
    augment(ServiceName, ServiceModule, []).

augment(ServiceName, ServiceModule, Args) when is_atom(ServiceName), is_atom(ServiceModule) ->
    % Create a supervisor for the service
    SupName = list_to_atom(atom_to_list(ServiceName) ++ "_sup"),
    case create_sup(SupName, one_for_one) of
        {ok, _} ->
            % Add the service as a child
            case add_child(SupName, ServiceName, ServiceModule, Args) of
                {ok, Pid} ->
                    io:format("~n✓ System augmented with service ~p (PID: ~p)~n", [ServiceName, Pid]),
                    {ok, ServiceName, Pid};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%% Add a service directly (simpler interface)
add_service(Name, Module) ->
    add_service(Name, Module, []).

add_service(Name, Module, Args) when is_atom(Name), is_atom(Module) ->
    % Try to add to the main supervisor first
    case add_child(agent_web_sup, Name, Module, Args) of
        {ok, Pid} ->
            io:format("✓ Added service ~p (PID: ~p)~n", [Name, Pid]),
            {ok, Pid};
        {error, _} ->
            % If that fails, try dynamic supervisor
            augment(Name, Module, Args)
    end.

%%====================================================================
%% Inspection
%%====================================================================

%% Inspect the system
inspect() ->
    io:format("~n=== System Inspection ===~n"),
    list_sups(),
    io:format("~n"),
    health(),
    ok.

%% Inspect a specific component
inspect(Component) when is_atom(Component) ->
    case whereis(Component) of
        undefined ->
            io:format("✗ Component ~p not found~n", [Component]),
            {error, not_found};
        Pid ->
            Info = process_info(Pid),
            io:format("~nComponent: ~p~n", [Component]),
            io:format("PID: ~p~n", [Pid]),
            io:format("Status: ~p~n", [proplists:get_value(status, Info)]),
            io:format("Memory: ~p bytes~n", [proplists:get_value(memory, Info)]),
            io:format("Message Queue: ~p messages~n", [proplists:get_value(message_queue_len, Info)]),
            io:format("Current Function: ~p~n", [proplists:get_value(current_function, Info)]),
            ok
    end.

%% System health check
health() ->
    io:format("~n=== System Health ===~n"),
    
    % Check key processes
    KeyProcesses = [
        dynamic_supervisor_manager,
        agent_web_sup,
        mcp_manager,
        mcp_connection_manager
    ],
    
    lists:foreach(fun(Process) ->
        case whereis(Process) of
            undefined ->
                io:format("• ~p: ✗ DOWN~n", [Process]);
            Pid ->
                case is_process_alive(Pid) of
                    true ->
                        QueueLen = element(2, process_info(Pid, message_queue_len)),
                        if
                            QueueLen > 100 ->
                                io:format("• ~p: ⚠ WARNING (queue: ~p)~n", [Process, QueueLen]);
                            true ->
                                io:format("• ~p: ✓ OK~n", [Process])
                        end;
                    false ->
                        io:format("• ~p: ✗ DEAD~n", [Process])
                end
        end
    end, KeyProcesses),
    ok.

%%====================================================================
%% Hot Code Loading
%%====================================================================

%% Reload a module
reload(Module) when is_atom(Module) ->
    case code:load_file(Module) of
        {module, Module} ->
            io:format("✓ Reloaded module ~p~n", [Module]),
            ok;
        {error, Reason} ->
            io:format("✗ Failed to reload ~p: ~p~n", [Module, Reason]),
            {error, Reason}
    end.

%% Reload all changed modules
reload_all() ->
    io:format("~nReloading changed modules...~n"),
    Changed = code:modified_modules(),
    case Changed of
        [] ->
            io:format("No modules need reloading~n");
        _ ->
            lists:foreach(fun(Module) ->
                reload(Module)
            end, Changed)
    end,
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

print_tree(#{main := Main, dynamic := Dynamic}, Indent) ->
    print_supervisor_node(Main, Indent),
    lists:foreach(fun(Node) ->
        print_supervisor_node(Node, Indent)
    end, Dynamic);
print_tree(Node, Indent) when is_map(Node) ->
    print_supervisor_node(Node, Indent).

print_supervisor_node(#{name := Name, pid := Pid, children := Children}, Indent) ->
    Spaces = lists:duplicate(Indent, " "),
    io:format("~s├─ ~p (PID: ~p)~n", [Spaces, Name, Pid]),
    lists:foreach(fun(Child) ->
        print_child_node(Child, Indent + 2)
    end, Children);
print_supervisor_node(#{name := Name, pid := Pid, error := Error}, Indent) ->
    Spaces = lists:duplicate(Indent, " "),
    io:format("~s├─ ~p (PID: ~p) [ERROR: ~p]~n", [Spaces, Name, Pid, Error]).

print_child_node(#{id := Id, pid := Pid, type := supervisor, children := Children}, Indent) ->
    Spaces = lists:duplicate(Indent, " "),
    io:format("~s├─ ~p [supervisor] (PID: ~p)~n", [Spaces, Id, Pid]),
    lists:foreach(fun(Child) ->
        print_child_node(Child, Indent + 2)
    end, Children);
print_child_node(#{id := Id, pid := Pid, type := Type}, Indent) ->
    Spaces = lists:duplicate(Indent, " "),
    io:format("~s├─ ~p [~p] (PID: ~p)~n", [Spaces, Id, Type, Pid]).