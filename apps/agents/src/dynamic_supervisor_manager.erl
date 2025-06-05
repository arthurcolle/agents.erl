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
    process_flag(trap_exit, true),
    {ok, #state{}}.

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
    case maps:get(Name, State#state.supervisors, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Pid ->
            ok = supervisor:terminate_child(agent_web_sup, {dynamic_sup, Name}),
            ok = supervisor:delete_child(agent_web_sup, {dynamic_sup, Name}),
            NewSupervisors = maps:remove(Name, State#state.supervisors),
            NewConfigs = maps:remove(Name, State#state.sup_configs),
            NewState = State#state{
                supervisors = NewSupervisors,
                sup_configs = NewConfigs
            },
            {reply, ok, NewState}
    end;

handle_call(list_supervisors, _From, State) ->
    SupervisorList = maps:fold(fun(Name, Pid, Acc) ->
        Config = maps:get(Name, State#state.sup_configs, #{}),
        Info = #{
            name => Name,
            pid => Pid,
            config => Config,
            children => get_children_info(Pid)
        },
        [Info | Acc]
    end, [], State#state.supervisors),
    {reply, {ok, SupervisorList}, State};

handle_call({add_child, SupName, ChildId, ChildSpec}, _From, State) ->
    case maps:get(SupName, State#state.supervisors, undefined) of
        undefined ->
            {reply, {error, supervisor_not_found}, State};
        Pid ->
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
    case maps:get(SupName, State#state.supervisors, undefined) of
        undefined ->
            {reply, {error, supervisor_not_found}, State};
        Pid ->
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
    case maps:get(Name, State#state.supervisors, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Pid ->
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

handle_info({'EXIT', Pid, Reason}, State) ->
    % Handle supervisor crashes
    case find_supervisor_by_pid(Pid, State#state.supervisors) of
        {ok, Name} ->
            io:format("Dynamic supervisor ~p (PID: ~p) exited with reason: ~p~n", 
                     [Name, Pid, Reason]),
            NewSupervisors = maps:remove(Name, State#state.supervisors),
            NewConfigs = maps:remove(Name, State#state.sup_configs),
            NewState = State#state{
                supervisors = NewSupervisors,
                sup_configs = NewConfigs
            },
            {noreply, NewState};
        error ->
            {noreply, State}
    end;

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
    
    % Add to the main supervisor
    case supervisor:start_child(agent_web_sup, ChildSpec) of
        {ok, Pid} ->
            {ok, Pid};
        {ok, Pid, _Info} ->
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

find_supervisor_by_pid(Pid, Supervisors) ->
    case maps:fold(fun(Name, SupPid, Acc) ->
        case SupPid of
            Pid -> {ok, Name};
            _ -> Acc
        end
    end, error, Supervisors) of
        {ok, Name} -> {ok, Name};
        error -> error
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
    case whereis(agent_web_sup) of
        undefined -> #{error => main_supervisor_not_found};
        Pid -> build_supervisor_subtree(agent_web_sup, Pid)
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