%%%-------------------------------------------------------------------

%% Colorful logging macros
-define(LOG_INFO(Msg), colored_logger:data(processed, Msg)).
-define(LOG_INFO(Msg, Args), colored_logger:data(processed, io_lib:format(Msg, Args))).
-define(LOG_ERROR(Msg), colored_logger:fire(inferno, Msg)).
-define(LOG_ERROR(Msg, Args), colored_logger:fire(inferno, io_lib:format(Msg, Args))).
-define(LOG_WARNING(Msg), colored_logger:alarm(medium, Msg)).
-define(LOG_WARNING(Msg, Args), colored_logger:alarm(medium, io_lib:format(Msg, Args))).
-define(LOG_SUCCESS(Msg), colored_logger:complete(success, Msg)).
-define(LOG_SUCCESS(Msg, Args), colored_logger:complete(success, io_lib:format(Msg, Args))).
-define(LOG_DEBUG(Msg), colored_logger:development(debugging, Msg)).
-define(LOG_DEBUG(Msg, Args), colored_logger:development(debugging, io_lib:format(Msg, Args))).

%%% @doc Dynamic System Augmentation for Agents
%%% This module provides agents with the ability to dynamically create
%%% supervisors and modify the supervision tree at runtime without restarts.
%%% @end
%%%-------------------------------------------------------------------
-module(agent_dynamic_system).

-behaviour(gen_server).

%% API
-export([start_link/0,
         create_supervisor/3,
         add_worker/3,
         add_supervisor/3,
         remove_child/2,
         list_supervisors/0,
         get_supervisor_tree/1,
         agent_augment_system/2,
         shell_create_supervisor/2,
         shell_add_child/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    supervisors = #{} :: #{atom() => pid()},
    augmentations = [] :: [map()]
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Create a new supervisor dynamically
create_supervisor(Name, Strategy, Children) when is_atom(Name) ->
    gen_server:call(?SERVER, {create_supervisor, Name, Strategy, Children}).

%% @doc Add a worker process to an existing supervisor
add_worker(SupervisorName, ChildId, ChildSpec) ->
    gen_server:call(?SERVER, {add_worker, SupervisorName, ChildId, ChildSpec}).

%% @doc Add a supervisor as a child of another supervisor
add_supervisor(ParentName, ChildName, Strategy) ->
    gen_server:call(?SERVER, {add_supervisor, ParentName, ChildName, Strategy}).

%% @doc Remove a child from a supervisor
remove_child(SupervisorName, ChildId) ->
    gen_server:call(?SERVER, {remove_child, SupervisorName, ChildId}).

%% @doc List all dynamically created supervisors
list_supervisors() ->
    gen_server:call(?SERVER, list_supervisors).

%% @doc Get the supervision tree structure for a supervisor
get_supervisor_tree(SupervisorName) ->
    gen_server:call(?SERVER, {get_supervisor_tree, SupervisorName}).

%% @doc Agent-friendly API for system augmentation
agent_augment_system(AgentPid, Augmentation) ->
    gen_server:call(?SERVER, {agent_augment, AgentPid, Augmentation}).

%% @doc Shell-friendly commands for testing
shell_create_supervisor(Name, Strategy) ->
    create_supervisor(Name, Strategy, []).

shell_add_child(SupervisorName, WorkerName, Module) ->
    ChildSpec = #{
        id => WorkerName,
        start => {Module, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker
    },
    add_worker(SupervisorName, WorkerName, ChildSpec).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ?LOG_INFO("[DYNAMIC_SYSTEM] Starting agent dynamic system manager"),
    {ok, #state{}}.

handle_call({create_supervisor, Name, Strategy, Children}, _From, State) ->
    try
        %% Create a dynamic supervisor
        SupFlags = #{
            strategy => Strategy,
            intensity => 5,
            period => 60
        },
        
        %% Start the supervisor
        case supervisor:start_link({local, Name}, dynamic_supervisor_wrapper, {SupFlags, Children}) of
            {ok, Pid} ->
                NewState = State#state{
                    supervisors = maps:put(Name, Pid, State#state.supervisors)
                },
                ?LOG_INFO("[DYNAMIC_SYSTEM] Created supervisor ~p with PID ~p", [Name, Pid]),
                {reply, {ok, Pid}, NewState};
            {error, Reason} ->
                {reply, {error, Reason}, State}
        end
    catch
        Type:Error ->
            ?LOG_INFO("[DYNAMIC_SYSTEM] Error creating supervisor: ~p:~p", [Type, Error]),
            {reply, {error, {Type, Error}}, State}
    end;

handle_call({add_worker, SupervisorName, ChildId, ChildSpec}, _From, State) ->
    case maps:get(SupervisorName, State#state.supervisors, undefined) of
        undefined ->
            %% Try to find the supervisor by registered name
            case whereis(SupervisorName) of
                undefined ->
                    {reply, {error, supervisor_not_found}, State};
                Pid ->
                    add_child_to_supervisor(Pid, ChildSpec, State)
            end;
        Pid ->
            add_child_to_supervisor(Pid, ChildSpec, State)
    end;

handle_call({add_supervisor, ParentName, ChildName, Strategy}, _From, State) ->
    case maps:get(ParentName, State#state.supervisors, undefined) of
        undefined ->
            {reply, {error, parent_supervisor_not_found}, State};
        ParentPid ->
            %% Create child supervisor spec
            ChildSupSpec = #{
                id => ChildName,
                start => {supervisor, start_link, [
                    {local, ChildName},
                    dynamic_supervisor_wrapper,
                    {#{strategy => Strategy, intensity => 5, period => 60}, []}
                ]},
                restart => permanent,
                shutdown => infinity,
                type => supervisor
            },
            
            case supervisor:start_child(ParentPid, ChildSupSpec) of
                {ok, ChildPid} ->
                    NewState = State#state{
                        supervisors = maps:put(ChildName, ChildPid, State#state.supervisors)
                    },
                    {reply, {ok, ChildPid}, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({remove_child, SupervisorName, ChildId}, _From, State) ->
    case maps:get(SupervisorName, State#state.supervisors, undefined) of
        undefined ->
            {reply, {error, supervisor_not_found}, State};
        Pid ->
            case supervisor:terminate_child(Pid, ChildId) of
                ok ->
                    supervisor:delete_child(Pid, ChildId),
                    {reply, ok, State};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call(list_supervisors, _From, State) ->
    Supervisors = [{Name, Pid} || {Name, Pid} <- maps:to_list(State#state.supervisors),
                                  is_process_alive(Pid)],
    {reply, Supervisors, State};

handle_call({get_supervisor_tree, SupervisorName}, _From, State) ->
    case maps:get(SupervisorName, State#state.supervisors, undefined) of
        undefined ->
            {reply, {error, supervisor_not_found}, State};
        Pid ->
            Children = supervisor:which_children(Pid),
            Tree = build_tree(Children),
            {reply, {ok, Tree}, State}
    end;

handle_call({agent_augment, AgentPid, Augmentation}, _From, State) ->
    %% Process agent augmentation request
    case Augmentation of
        #{type := supervisor, name := Name, strategy := Strategy} ->
            case create_supervisor_for_agent(Name, Strategy) of
                {ok, Pid} ->
                    NewAugmentation = Augmentation#{
                        agent => AgentPid,
                        supervisor_pid => Pid,
                        timestamp => erlang:system_time(second)
                    },
                    NewState = State#state{
                        supervisors = maps:put(Name, Pid, State#state.supervisors),
                        augmentations = [NewAugmentation | State#state.augmentations]
                    },
                    {reply, {ok, Pid}, NewState};
                Error ->
                    {reply, Error, State}
            end;
        #{type := worker, supervisor := SupName, spec := Spec} ->
            case add_worker_for_agent(SupName, Spec) of
                ok ->
                    NewAugmentation = Augmentation#{
                        agent => AgentPid,
                        timestamp => erlang:system_time(second)
                    },
                    NewState = State#state{
                        augmentations = [NewAugmentation | State#state.augmentations]
                    },
                    {reply, ok, NewState};
                Error ->
                    {reply, Error, State}
            end;
        _ ->
            {reply, {error, invalid_augmentation}, State}
    end;

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

%%%===================================================================
%%% Internal functions
%%%===================================================================

add_child_to_supervisor(Pid, ChildSpec, State) ->
    case supervisor:start_child(Pid, ChildSpec) of
        {ok, _ChildPid} ->
            ?LOG_SUCCESS("[DYNAMIC_SYSTEM] Added child ~p to supervisor", [maps:get(id, ChildSpec)]),
            {reply, ok, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.

build_tree(Children) ->
    lists:map(fun({Id, Pid, Type, _Modules}) ->
        #{id => Id, pid => Pid, type => Type}
    end, Children).

create_supervisor_for_agent(Name, Strategy) ->
    SupFlags = #{
        strategy => Strategy,
        intensity => 5,
        period => 60
    },
    supervisor:start_link({local, Name}, dynamic_supervisor_wrapper, {SupFlags, []}).

add_worker_for_agent(SupName, Spec) ->
    case whereis(SupName) of
        undefined ->
            {error, supervisor_not_found};
        Pid ->
            case supervisor:start_child(Pid, Spec) of
                {ok, _} -> ok;
                Error -> Error
            end
    end.