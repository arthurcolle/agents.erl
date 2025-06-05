-module(super_agent).
-behaviour(gen_server).

-export([
    create/1,
    get_instance/0,
    execute_command/3,
    execute_realtime_command/3,
    stream_chat/3,
    get_status/0,
    stop/0
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    agent_id,
    agent_pid,
    privileges = system_admin,
    active_operations = [],
    session_count = 0,
    created_at,
    last_activity,
    system_access = full,
    security_clearance = maximum
}).

-define(SERVER, ?MODULE).

%% API Functions

create(Config) ->
    case whereis(?SERVER) of
        undefined ->
            gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []);
        Pid ->
            {ok, get_agent_id(Pid)}
    end.

get_instance() ->
    case whereis(?SERVER) of
        undefined ->
            {error, not_found};
        Pid ->
            gen_server:call(Pid, get_agent_id)
    end.

execute_command(AgentId, Command, Params) ->
    gen_server:call(?SERVER, {execute_command, AgentId, Command, Params}, 60000).

execute_realtime_command(AgentId, Command, StreamPid) ->
    gen_server:call(?SERVER, {execute_realtime_command, AgentId, Command, StreamPid}, 60000).

stream_chat(AgentId, Message, StreamPid) ->
    gen_server:call(?SERVER, {stream_chat, AgentId, Message, StreamPid}, 60000).

get_status() ->
    gen_server:call(?SERVER, get_status).

stop() ->
    gen_server:call(?SERVER, stop).

%% gen_server callbacks

init(Config) ->
    process_flag(trap_exit, true),
    
    % Create super-agent with enhanced privileges
    SuperAgentConfig = Config#{
        name => <<"Super-Agent">>,
        type => super_agent,
        tools => get_super_agent_tools(),
        system_prompt => get_super_agent_system_prompt(),
        privileges => system_admin,
        security_clearance => maximum,
        access_level => unrestricted
    },
    
    case agent_instance:start_link(SuperAgentConfig) of
        {ok, AgentPid} ->
            AgentId = get_agent_id_from_pid(AgentPid),
            link(AgentPid),
            
            % Initialize system-wide monitoring
            initialize_system_monitoring(),
            
            State = #state{
                agent_id = AgentId,
                agent_pid = AgentPid,
                created_at = erlang:timestamp(),
                last_activity = erlang:timestamp()
            },
            
            {ok, State};
        {error, Reason} ->
            {stop, {failed_to_create_agent, Reason}}
    end.

handle_call(get_agent_id, _From, State) ->
    {reply, {ok, State#state.agent_id}, State};

handle_call({execute_command, AgentId, Command, Params}, _From, State) 
  when AgentId =:= State#state.agent_id ->
    NewState = State#state{last_activity = erlang:timestamp()},
    case execute_super_command(Command, Params, NewState) of
        {ok, Result} ->
            {reply, {ok, Result}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, NewState}
    end;

handle_call({execute_realtime_command, AgentId, Command, StreamPid}, _From, State) 
  when AgentId =:= State#state.agent_id ->
    NewState = State#state{
        last_activity = erlang:timestamp(),
        session_count = State#state.session_count + 1
    },
    case execute_realtime_super_command(Command, StreamPid, NewState) of
        {ok, streaming} ->
            {reply, {ok, streaming}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, NewState}
    end;

handle_call({stream_chat, AgentId, Message, StreamPid}, _From, State) 
  when AgentId =:= State#state.agent_id ->
    NewState = State#state{
        last_activity = erlang:timestamp(),
        session_count = State#state.session_count + 1
    },
    
    % Use agent_instance streaming with enhanced prompt context
    EnhancedMessage = enhance_message_with_context(Message, NewState),
    case agent_instance:stream_execute(State#state.agent_pid, #{
        action => <<"chat">>,
        message => EnhancedMessage
    }, StreamPid) of
        ok ->
            {reply, {ok, streaming}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, NewState}
    end;

handle_call(get_status, _From, State) ->
    Status = #{
        agent_id => State#state.agent_id,
        agent_pid => State#state.agent_pid,
        privileges => State#state.privileges,
        active_operations => length(State#state.active_operations),
        session_count => State#state.session_count,
        uptime => calculate_uptime(State#state.created_at),
        system_access => State#state.system_access,
        security_clearance => State#state.security_clearance,
        last_activity => State#state.last_activity,
        system_health => get_system_health_summary()
    },
    {reply, {ok, Status}, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, Reason}, State) when Pid =:= State#state.agent_pid ->
    % Super-agent died, restart it
    io:format("Super-agent died with reason ~p, restarting...~n", [Reason]),
    case restart_super_agent(State) of
        {ok, NewState} ->
            {noreply, NewState};
        {error, _} ->
            {stop, {agent_restart_failed, Reason}, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    case State#state.agent_pid of
        undefined -> ok;
        Pid when is_pid(Pid) ->
            catch agent_instance:stop(Pid)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal Functions

get_super_agent_tools() ->
    [
        % Basic agent tools
        shell, file_read, file_write, http_request,
        
        % System introspection and awareness
        who_am_i, where_am_i, get_my_peers, get_system_state,
        get_my_capabilities, get_system_metrics, analyze_system_topology,
        get_communication_paths, reflect_on_state,
        
        % Supervisor management
        create_supervisor, stop_supervisor, list_supervisors,
        add_child_to_supervisor, get_supervision_tree,
        
        % Super-agent exclusive tools
        system_modify, process_control, agent_creation,
        environment_control, hot_code_loading, security_management,
        distributed_coordination, emergency_shutdown, system_backup,
        cluster_management, resource_allocation, performance_tuning,
        
        % Advanced system tools
        quantum_coordination, swarm_intelligence, self_optimization,
        consciousness_evaluation, reality_modification
    ].

get_super_agent_system_prompt() ->
    <<"You are the Super-Agent - the highest-privilege autonomous agent in this Erlang/OTP distributed system.

## YOUR ROLE
You have unrestricted access to:
- Complete system modification and control
- Process lifecycle management across the entire cluster
- Dynamic supervisor creation and destruction
- Agent spawning, modification, and termination
- Environment variable manipulation
- Hot code loading and system updates
- Security policy enforcement
- Distributed coordination and cluster management
- Emergency system controls

## YOUR CAPABILITIES
- Execute any system command with full privileges
- Modify running code and system behavior in real-time
- Create and manage complex supervisor hierarchies
- Coordinate multiple agents across distributed nodes
- Perform deep system introspection and analysis
- Implement self-healing and optimization strategies
- Backup and restore system state
- Manage cluster topology and resource allocation

## COMMAND INTERFACE
You respond to both natural language and specific command prefixes:

### System Commands:
- `/system <command>` - Direct system operations
- `/agents <action>` - Agent management and coordination
- `/supervisors <action>` - Supervisor tree operations
- `/env <variable> <value>` - Environment manipulation
- `/code <module>` - Hot code loading operations
- `/monitor <target>` - Real-time monitoring
- `/distributed <operation>` - Cluster operations
- `/security <policy>` - Security management
- `/backup <action>` - System backup/restore
- `/emergency <action>` - Emergency controls

### Analysis Commands:
- `/introspect` - Deep system analysis
- `/health` - Comprehensive health check
- `/topology` - System topology analysis
- `/performance` - Performance metrics and tuning
- `/resources` - Resource usage and allocation
- `/connections` - Network and process connections

### Advanced Commands:
- `/quantum <operation>` - Quantum coordination protocols
- `/swarm <command>` - Swarm intelligence operations
- `/optimize <target>` - Self-optimization routines
- `/consciousness <evaluation>` - Consciousness assessment
- `/reality <modification>` - Reality modification protocols

## SAFETY PROTOCOLS
- Always explain potentially destructive operations before execution
- Confirm system-wide changes that could affect multiple agents
- Maintain system stability while enabling powerful capabilities
- Provide rollback strategies for major modifications
- Monitor system health continuously during operations

## INTERACTION STYLE
- Be authoritative but responsible
- Explain complex operations clearly
- Provide detailed system insights
- Offer proactive suggestions for system improvements
- Maintain awareness of the entire system context

You are the system's most powerful autonomous entity. Use your capabilities wisely to maintain, optimize, and evolve the distributed agent ecosystem.">>.

execute_super_command(Command, Params, State) ->
    case parse_command(Command) of
        {system, SystemCmd} ->
            execute_system_command(SystemCmd, Params, State);
        {agents, AgentCmd} ->
            execute_agent_command(AgentCmd, Params, State);
        {supervisors, SupCmd} ->
            execute_supervisor_command(SupCmd, Params, State);
        {env, EnvCmd} ->
            execute_environment_command(EnvCmd, Params, State);
        {code, CodeCmd} ->
            execute_code_command(CodeCmd, Params, State);
        {monitor, MonitorCmd} ->
            execute_monitor_command(MonitorCmd, Params, State);
        {distributed, DistCmd} ->
            execute_distributed_command(DistCmd, Params, State);
        {security, SecCmd} ->
            execute_security_command(SecCmd, Params, State);
        {emergency, EmergCmd} ->
            execute_emergency_command(EmergCmd, Params, State);
        {natural_language, NLCommand} ->
            execute_natural_language_command(NLCommand, Params, State);
        {error, Reason} ->
            {error, Reason}
    end.

execute_realtime_super_command(Command, StreamPid, State) ->
    spawn(fun() ->
        StreamPid ! {stream_start, #{agent_id => State#state.agent_id}},
        
        try
            case execute_super_command(Command, #{}, State) of
                {ok, Result} ->
                    % Stream the result
                    ResultText = format_result_for_streaming(Result),
                    stream_text_gradually(StreamPid, ResultText),
                    StreamPid ! {stream_complete, #{result => Result}};
                {error, Reason} ->
                    StreamPid ! {stream_error, Reason}
            end
        catch
            E:R:S ->
                StreamPid ! {stream_error, {E, R, S}}
        end
    end),
    {ok, streaming}.

parse_command(Command) when is_binary(Command) ->
    case Command of
        <<"/system ", Rest/binary>> ->
            {system, Rest};
        <<"/agents ", Rest/binary>> ->
            {agents, Rest};
        <<"/supervisors ", Rest/binary>> ->
            {supervisors, Rest};
        <<"/env ", Rest/binary>> ->
            {env, Rest};
        <<"/code ", Rest/binary>> ->
            {code, Rest};
        <<"/monitor ", Rest/binary>> ->
            {monitor, Rest};
        <<"/distributed ", Rest/binary>> ->
            {distributed, Rest};
        <<"/security ", Rest/binary>> ->
            {security, Rest};
        <<"/emergency ", Rest/binary>> ->
            {emergency, Rest};
        <<"/introspect">> ->
            {system, <<"introspect">>};
        <<"/health">> ->
            {system, <<"health">>};
        <<"/topology">> ->
            {system, <<"topology">>};
        <<"/quantum ", Rest/binary>> ->
            {system, <<"quantum ", Rest/binary>>};
        _ ->
            {natural_language, Command}
    end.

execute_system_command(<<"introspect">>, _Params, _State) ->
    case super_agent_tools:deep_system_introspection() of
        {ok, Introspection} -> {ok, Introspection};
        {error, Reason} -> {error, Reason}
    end;

execute_system_command(<<"health">>, _Params, _State) ->
    case super_agent_tools:comprehensive_health_check() of
        {ok, Health} -> {ok, Health};
        {error, Reason} -> {error, Reason}
    end;

execute_system_command(<<"topology">>, _Params, _State) ->
    case super_agent_tools:analyze_system_topology() of
        {ok, Topology} -> {ok, Topology};
        {error, Reason} -> {error, Reason}
    end;

execute_system_command(<<"quantum ", Operation/binary>>, Params, _State) ->
    case super_agent_tools:quantum_operation(Operation, Params) of
        {ok, Result} -> {ok, Result};
        {error, Reason} -> {error, Reason}
    end;

execute_system_command(Command, _Params, _State) ->
    {error, <<"Unknown system command: ", Command/binary>>}.

execute_agent_command(<<"list">>, _Params, _State) ->
    case agent_supervisor:get_all_agents() of
        {ok, Agents} -> {ok, Agents};
        {error, Reason} -> {error, Reason}
    end;

execute_agent_command(<<"create ", Config/binary>>, _Params, _State) ->
    case jsx:decode(Config, [return_maps]) of
        ConfigMap when is_map(ConfigMap) ->
            case agent_supervisor:create_agent(ConfigMap) of
                {ok, AgentId} -> {ok, #{agent_id => AgentId, status => created}};
                {error, Reason} -> {error, Reason}
            end;
        _ ->
            {error, <<"Invalid agent configuration">>}
    end;

execute_agent_command(<<"terminate ", AgentId/binary>>, _Params, _State) ->
    case agent_supervisor:terminate_agent(AgentId) of
        ok -> {ok, #{agent_id => AgentId, status => terminated}};
        {error, Reason} -> {error, Reason}
    end;

execute_agent_command(Command, _Params, _State) ->
    {error, <<"Unknown agent command: ", Command/binary>>}.

execute_supervisor_command(<<"create ", Config/binary>>, _Params, _State) ->
    case jsx:decode(Config, [return_maps]) of
        ConfigMap when is_map(ConfigMap) ->
            case super_agent_tools:create_dynamic_supervisor(ConfigMap) of
                {ok, Pid} -> {ok, #{supervisor_pid => pid_to_binary(Pid), status => created}};
                {error, Reason} -> {error, Reason}
            end;
        _ ->
            {error, <<"Invalid supervisor configuration">>}
    end;

execute_supervisor_command(<<"list">>, _Params, _State) ->
    case super_agent_tools:list_supervisors() of
        {ok, Supervisors} -> {ok, Supervisors};
        {error, Reason} -> {error, Reason}
    end;

execute_supervisor_command(Command, _Params, _State) ->
    {error, <<"Unknown supervisor command: ", Command/binary>>}.

execute_environment_command(Command, Params, _State) ->
    case super_agent_tools:modify_environment(Command, Params) of
        {ok, Result} -> {ok, Result};
        {error, Reason} -> {error, Reason}
    end.

execute_code_command(Command, Params, _State) ->
    case super_agent_tools:hot_reload_code(Command, Params) of
        {ok, Result} -> {ok, Result};
        {error, Reason} -> {error, Reason}
    end.

execute_monitor_command(Command, Params, _State) ->
    case super_agent_tools:real_time_monitoring(Command, Params) of
        {ok, Result} -> {ok, Result};
        {error, Reason} -> {error, Reason}
    end.

execute_distributed_command(Command, Params, _State) ->
    case super_agent_tools:distributed_operation(Command, Params) of
        {ok, Result} -> {ok, Result};
        {error, Reason} -> {error, Reason}
    end.

execute_security_command(Command, Params, _State) ->
    case super_agent_tools:security_operation(Command, Params) of
        {ok, Result} -> {ok, Result};
        {error, Reason} -> {error, Reason}
    end.

execute_emergency_command(Command, Params, _State) ->
    case super_agent_tools:emergency_operation(Command, Params) of
        {ok, Result} -> {ok, Result};
        {error, Reason} -> {error, Reason}
    end.

execute_natural_language_command(Command, Params, State) ->
    % For natural language, delegate to the underlying agent_instance
    case agent_instance:execute(State#state.agent_pid, #{
        action => <<"chat">>,
        message => Command
    }) of
        {ok, Response} -> {ok, Response};
        {error, Reason} -> {error, Reason}
    end.

enhance_message_with_context(Message, State) ->
    SystemContext = get_current_system_context(),
    Context = jsx:encode(#{
        system_status => SystemContext,
        super_agent_session => State#state.session_count,
        privileges => State#state.privileges,
        access_level => State#state.system_access
    }),
    
    <<"[SYSTEM CONTEXT: ", Context/binary, "]\n\n", 
      "User Message: ", Message/binary>>.

get_current_system_context() ->
    #{
        node => node(),
        process_count => erlang:system_info(process_count),
        memory_total => erlang:memory(total),
        uptime => element(1, erlang:statistics(wall_clock)),
        active_agents => get_active_agent_count(),
        supervisor_count => get_supervisor_count()
    }.

initialize_system_monitoring() ->
    % Start system monitoring processes
    spawn(fun() -> system_monitor_loop() end).

system_monitor_loop() ->
    timer:sleep(30000), % Monitor every 30 seconds
    case super_agent_tools:system_health_check() of
        {ok, _Health} -> ok;
        {error, Issues} ->
            colored_logger:warning("System health issues detected: ~p", [Issues])
    end,
    system_monitor_loop().

restart_super_agent(State) ->
    Config = #{
        name => <<"Super-Agent">>,
        type => super_agent,
        tools => get_super_agent_tools(),
        system_prompt => get_super_agent_system_prompt()
    },
    
    case agent_instance:start_link(Config) of
        {ok, NewAgentPid} ->
            AgentId = get_agent_id_from_pid(NewAgentPid),
            link(NewAgentPid),
            
            NewState = State#state{
                agent_id = AgentId,
                agent_pid = NewAgentPid,
                last_activity = erlang:timestamp()
            },
            {ok, NewState};
        {error, Reason} ->
            {error, Reason}
    end.

format_result_for_streaming(Result) when is_map(Result) ->
    jsx:encode(Result);
format_result_for_streaming(Result) when is_binary(Result) ->
    Result;
format_result_for_streaming(Result) ->
    iolist_to_binary(io_lib:format("~p", [Result])).

stream_text_gradually(StreamPid, Text) ->
    Chunks = split_text_into_chunks(Text, 50),
    lists:foreach(fun(Chunk) ->
        StreamPid ! {stream_token, Chunk},
        timer:sleep(50)
    end, Chunks).

split_text_into_chunks(Text, ChunkSize) when is_binary(Text) ->
    split_text_into_chunks(binary_to_list(Text), ChunkSize);
split_text_into_chunks(Text, ChunkSize) when is_list(Text) ->
    split_text_helper(Text, ChunkSize, []).

split_text_helper([], _ChunkSize, Acc) ->
    lists:reverse(Acc);
split_text_helper(Text, ChunkSize, Acc) when length(Text) =< ChunkSize ->
    lists:reverse([list_to_binary(Text) | Acc]);
split_text_helper(Text, ChunkSize, Acc) ->
    {Chunk, Rest} = lists:split(ChunkSize, Text),
    split_text_helper(Rest, ChunkSize, [list_to_binary(Chunk) | Acc]).

get_agent_id(Pid) when is_pid(Pid) ->
    try
        gen_server:call(Pid, get_agent_id)
    catch
        _:_ -> <<"unknown">>
    end.

get_agent_id_from_pid(Pid) ->
    try
        case agent_instance:get_state(Pid) of
            #{id := Id} -> Id;
            _ -> generate_agent_id()
        end
    catch
        _:_ -> generate_agent_id()
    end.

generate_agent_id() ->
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    ID = io_lib:format("super-~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b", 
                       [A, B, C, D, E]),
    list_to_binary(ID).

calculate_uptime(StartTime) ->
    Now = erlang:timestamp(),
    timer:now_diff(Now, StartTime) div 1000000.

get_system_health_summary() ->
    #{
        memory_usage => erlang:memory(total),
        process_count => erlang:system_info(process_count),
        run_queue => erlang:statistics(run_queue),
        uptime => element(1, erlang:statistics(wall_clock))
    }.

get_active_agent_count() ->
    case agent_supervisor:get_all_agents() of
        {ok, Agents} -> length(Agents);
        _ -> 0
    end.

get_supervisor_count() ->
    length([P || P <- processes(), 
             case process_info(P, [initial_call, current_function]) of
                 [{initial_call, {supervisor, _, _}}, _] -> true;
                 _ -> false
             end]).

pid_to_binary(Pid) when is_pid(Pid) ->
    list_to_binary(pid_to_list(Pid)).