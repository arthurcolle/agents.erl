-module(dynamic_supervisor_manager).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    create_supervisor/2,
    create_supervisor/3,
    stop_supervisor/1,
    list_supervisors/0,
    add_child_to_supervisor/3,
    remove_child_from_supervisor/2,
    get_supervisor_info/1,
    get_supervision_tree/0
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
    supervisors = #{} :: #{atom() => pid()},
    sup_configs = #{} :: #{atom() => map()}
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Create a new supervisor with the given name and supervisor flags
create_supervisor(Name, SupFlags) ->
    create_supervisor(Name, SupFlags, []).

create_supervisor(Name, SupFlags, InitialChildren) ->
    gen_server:call(?MODULE, {create_supervisor, Name, SupFlags, InitialChildren}).

%% Stop a supervisor
stop_supervisor(Name) ->
    gen_server:call(?MODULE, {stop_supervisor, Name}).

%% List all managed supervisors
list_supervisors() ->
    gen_server:call(?MODULE, list_supervisors).

%% Add a child spec to an existing supervisor
add_child_to_supervisor(SupName, ChildId, ChildSpec) ->
    gen_server:call(?MODULE, {add_child, SupName, ChildId, ChildSpec}).

%% Remove a child from a supervisor
remove_child_from_supervisor(SupName, ChildId) ->
    gen_server:call(?MODULE, {remove_child, SupName, ChildId}).

%% Get information about a specific supervisor
get_supervisor_info(Name) ->
    gen_server:call(?MODULE, {get_supervisor_info, Name}).

%% Get the entire supervision tree structure
get_supervision_tree() ->
    gen_server:call(?MODULE, get_supervision_tree).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    {Supervisors, Configs} = discover_supervisors(),
    {ok, #state{supervisors = Supervisors, sup_configs = Configs}}.

handle_call({create_supervisor, Name, SupFlags, InitialChildren}, _From, State) ->
    case maps:is_key(Name, State#state.supervisors) of
        true ->
            {reply, {error, already_exists}, State};
        false ->
            case start_dynamic_supervisor(Name, SupFlags, InitialChildren) of
                {ok, Pid} ->
                    NewSupervisors = maps:put(Name, Pid, State#state.supervisors),
                    NewConfigs = maps:put(Name, #{
                        flags => SupFlags,
                        children => InitialChildren,
                        created_at => erlang:timestamp()
                    }, State#state.sup_configs),
                    NewState = State#state{
                        supervisors = NewSupervisors,
                        sup_configs = NewConfigs
                    },
                    {reply, {ok, Pid}, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({stop_supervisor, Name}, _From, State) ->
    case maps:is_key(Name, State#state.supervisors) of
        false ->
            {reply, {error, not_found}, State};
        true ->
            case remove_dynamic_supervisor(Name) of
                ok ->
                    NewSupervisors = maps:remove(Name, State#state.supervisors),
                    NewConfigs = maps:remove(Name, State#state.sup_configs),
                    NewState = State#state{
                        supervisors = NewSupervisors,
                        sup_configs = NewConfigs
                    },
                    {reply, ok, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call(list_supervisors, _From, State) ->
    SupervisorList = maps:fold(fun(Name, _StoredPid, Acc) ->
        Config = maps:get(Name, State#state.sup_configs, #{}),
        Info = case lookup_supervisor(Name) of
            {ok, Pid} ->
                #{
                    name => Name,
                    pid => Pid,
                    status => running,
                    config => Config,
                    children => get_children_info(Pid)
                };
            {error, Reason} ->
                #{
                    name => Name,
                    pid => undefined,
                    status => Reason,
                    config => Config,
                    children => []
                }
        end,
        [Info | Acc]
    end, [], State#state.supervisors),
    {reply, {ok, SupervisorList}, State};

handle_call({add_child, SupName, ChildId, ChildSpec}, _From, State) ->
    case lookup_managed_supervisor(SupName, State) of
        {error, not_found} ->
            {reply, {error, supervisor_not_found}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State};
        {ok, Pid} ->
            case supervisor:start_child(Pid, ChildSpec) of
                {ok, ChildPid} ->
                    {reply, {ok, ChildPid}, State};
                {ok, ChildPid, _Info} ->
                    {reply, {ok, ChildPid}, State};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({remove_child, SupName, ChildId}, _From, State) ->
    case lookup_managed_supervisor(SupName, State) of
        {error, not_found} ->
            {reply, {error, supervisor_not_found}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State};
        {ok, Pid} ->
            case supervisor:terminate_child(Pid, ChildId) of
                ok ->
                    case supervisor:delete_child(Pid, ChildId) of
                        ok ->
                            {reply, ok, State};
                        {error, Reason} ->
                            {reply, {error, Reason}, State}
                    end;
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({get_supervisor_info, Name}, _From, State) ->
    case lookup_managed_supervisor(Name, State) of
        {error, not_found} ->
            {reply, {error, not_found}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State};
        {ok, Pid} ->
            Config = maps:get(Name, State#state.sup_configs, #{}),
            Children = supervisor:which_children(Pid),
            Count = supervisor:count_children(Pid),
            Info = #{
                name => Name,
                pid => Pid,
                config => Config,
                children => Children,
                child_count => Count
            },
            {reply, {ok, Info}, State}
    end;

handle_call(get_supervision_tree, _From, State) ->
    Tree = build_supervision_tree(State),
    {reply, {ok, Tree}, State};

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

%%====================================================================
%% Internal functions
%%====================================================================

start_dynamic_supervisor(Name, SupFlags, InitialChildren) ->
    % Create a supervisor module dynamically
    ChildSpec = #{
        id => {dynamic_sup, Name},
        start => {dynamic_supervisor_wrapper, start_link, [Name, SupFlags, InitialChildren]},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [dynamic_supervisor_wrapper]
    },
    
    % Runtime-created supervisors belong to the core runtime, never the web
    % application's root supervisor.
    case supervisor:start_child(dynamic_supervisors_sup, ChildSpec) of
        {ok, Pid} ->
            {ok, Pid};
        {ok, Pid, _Info} ->
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

remove_dynamic_supervisor(Name) ->
    ChildId = {dynamic_sup, Name},
    case supervisor:terminate_child(dynamic_supervisors_sup, ChildId) of
        ok ->
            case supervisor:delete_child(dynamic_supervisors_sup, ChildId) of
                ok -> ok;
                {error, not_found} -> ok;
                Error -> Error
            end;
        {error, not_found} ->
            {error, not_found};
        Error ->
            Error
    end.

lookup_managed_supervisor(Name, State) ->
    case maps:is_key(Name, State#state.supervisors) of
        true -> lookup_supervisor(Name);
        false -> {error, not_found}
    end.

lookup_supervisor(Name) ->
    try supervisor:which_children(dynamic_supervisors_sup) of
        Children ->
            case lists:keyfind({dynamic_sup, Name}, 1, Children) of
                {{dynamic_sup, Name}, Pid, supervisor, _Modules}
                        when is_pid(Pid) ->
                    {ok, Pid};
                {{dynamic_sup, Name}, restarting, supervisor, _Modules} ->
                    {error, restarting};
                {{dynamic_sup, Name}, undefined, supervisor, _Modules} ->
                    {error, stopped};
                false ->
                    {error, not_found}
            end
    catch
        exit:{noproc, _} -> {error, host_not_running};
        _:_ -> {error, unavailable}
    end.

discover_supervisors() ->
    try supervisor:which_children(dynamic_supervisors_sup) of
        Children ->
            lists:foldl(
                fun
                    ({{dynamic_sup, Name}, Pid, supervisor, _Modules},
                     {Supervisors, Configs}) ->
                        {
                            maps:put(Name, Pid, Supervisors),
                            maps:put(Name, #{recovered => true}, Configs)
                        };
                    (_, Acc) ->
                        Acc
                end,
                {#{}, #{}},
                Children
            )
    catch
        _:_ -> {#{}, #{}}
    end.

get_children_info(SupPid) ->
    try
        Children = supervisor:which_children(SupPid),
        lists:map(fun({Id, ChildPid, Type, Modules}) ->
            #{
                id => Id,
                pid => ChildPid,
                type => Type,
                modules => Modules
            }
        end, Children)
    catch
        _:_ -> []
    end.

build_supervision_tree(State) ->
    MainTree = get_main_supervision_tree(),
    DynamicTrees = maps:fold(fun(Name, Pid, Acc) ->
        Tree = build_supervisor_subtree(Name, Pid),
        [Tree | Acc]
    end, [], State#state.supervisors),
    #{
        main => MainTree,
        dynamic => DynamicTrees
    }.

get_main_supervision_tree() ->
    case whereis(agents_sup) of
        undefined -> #{error => main_supervisor_not_found};
        Pid -> build_supervisor_subtree(agents_sup, Pid)
    end.

build_supervisor_subtree(Name, Pid) ->
    try
        Children = supervisor:which_children(Pid),
        ChildTrees = lists:map(fun({ChildId, ChildPid, Type, _Modules}) ->
            case Type of
                supervisor when is_pid(ChildPid) ->
                    #{
                        id => ChildId,
                        pid => ChildPid,
                        type => supervisor,
                        children => build_supervisor_subtree(ChildId, ChildPid)
                    };
                _ ->
                    #{
                        id => ChildId,
                        pid => ChildPid,
                        type => Type
                    }
            end
        end, Children),
        #{
            name => Name,
            pid => Pid,
            children => ChildTrees
        }
    catch
        _:_ ->
            #{
                name => Name,
                pid => Pid,
                error => failed_to_get_children
            }
    end.
