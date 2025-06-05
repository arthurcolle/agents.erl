-module(super_agent_handler).
-behaviour(cowboy_handler).

-export([init/2]).
-export([websocket_init/1, websocket_handle/2, websocket_info/2]).

-record(state, {
    agent_id,
    privileges = system_admin,
    session_id,
    active_commands = [],
    system_access = full
}).

%% HTTP handler for super-agent REST API with WebSocket upgrade support
init(Req0, Opts) ->
    case cowboy_req:parse_header(<<"upgrade">>, Req0) of
        [<<"websocket">>] ->
            % WebSocket upgrade request
            {cowboy_websocket, Req0, #state{}};
        _ ->
            % Regular HTTP request
            Method = cowboy_req:method(Req0),
            Path = cowboy_req:path_info(Req0),
            handle_request(Method, Path, Req0, Opts)
    end.

handle_request(<<"GET">>, [<<"api">>, <<"super-agent">>, <<"capabilities">>], Req, _Opts) ->
    Capabilities = #{
        system_modification => true,
        process_control => true,
        agent_management => true,
        environment_control => true,
        supervisor_creation => true,
        hot_code_loading => true,
        system_introspection => true,
        distributed_coordination => true
    },
    Response = jsx:encode(Capabilities),
    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req),
    {ok, Req2, #{}};

handle_request(<<"POST">>, [<<"api">>, <<"super-agent">>, <<"chat">>], Req, _Opts) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    case jsx:decode(Body, [return_maps]) of
        #{<<"message">> := Message} = Params ->
            case create_super_agent_if_needed() of
                {ok, AgentId} ->
                    case super_agent:execute_command(AgentId, Message, Params) of
                        {ok, Response} ->
                            ResponseJson = jsx:encode(#{
                                status => success,
                                response => Response,
                                agent_id => AgentId,
                                capabilities => get_active_capabilities()
                            }),
                            Req3 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, 
                                                 ResponseJson, Req2),
                            {ok, Req3, #{}};
                        {error, Reason} ->
                            ErrorJson = jsx:encode(#{status => error, reason => Reason}),
                            Req3 = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, 
                                                 ErrorJson, Req2),
                            {ok, Req3, #{}}
                    end;
                {error, Reason} ->
                    ErrorJson = jsx:encode(#{status => error, reason => Reason}),
                    Req3 = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, 
                                         ErrorJson, Req2),
                    {ok, Req3, #{}}
            end;
        _ ->
            ErrorJson = jsx:encode(#{status => error, reason => <<"Invalid request format">>}),
            Req3 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, 
                                 ErrorJson, Req2),
            {ok, Req3, #{}}
    end;

handle_request(<<"POST">>, [<<"api">>, <<"super-agent">>, <<"system-command">>], Req, _Opts) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    case jsx:decode(Body, [return_maps]) of
        #{<<"command">> := Command, <<"args">> := Args} ->
            case execute_system_command(Command, Args) of
                {ok, Result} ->
                    ResponseJson = jsx:encode(#{
                        status => success,
                        result => Result,
                        timestamp => erlang:timestamp()
                    }),
                    Req3 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, 
                                         ResponseJson, Req2),
                    {ok, Req3, #{}};
                {error, Reason} ->
                    ErrorJson = jsx:encode(#{status => error, reason => Reason}),
                    Req3 = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, 
                                         ErrorJson, Req2),
                    {ok, Req3, #{}}
            end;
        _ ->
            ErrorJson = jsx:encode(#{status => error, reason => <<"Invalid command format">>}),
            Req3 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, 
                                 ErrorJson, Req2),
            {ok, Req3, #{}}
    end;

handle_request(<<"GET">>, [<<"api">>, <<"super-agent">>, <<"system-status">>], Req, _Opts) ->
    Status = get_comprehensive_system_status(),
    ResponseJson = jsx:encode(Status),
    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, ResponseJson, Req),
    {ok, Req2, #{}};

handle_request(Method, Path, Req, _Opts) ->
    io:format("[SUPER_AGENT] Unmatched request - Method: ~p, Path: ~p~n", [Method, Path]),
    ErrorJson = jsx:encode(#{status => error, reason => <<"Endpoint not found">>}),
    Req2 = cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>}, ErrorJson, Req),
    {ok, Req2, #{}}.

%% WebSocket handler for real-time super-agent interaction
websocket_init(State) ->
    SessionId = generate_session_id(),
    case create_super_agent_if_needed() of
        {ok, AgentId} ->
            NewState = State#state{
                agent_id = AgentId,
                session_id = SessionId
            },
            WelcomeMsg = jsx:encode(#{
                type => <<"welcome">>,
                message => <<"Super-Agent Shell Active">>,
                agent_id => AgentId,
                session_id => SessionId,
                capabilities => get_active_capabilities(),
                system_info => get_system_info()
            }),
            {reply, {text, WelcomeMsg}, NewState};
        {error, Reason} ->
            ErrorMsg = jsx:encode(#{
                type => <<"error">>,
                message => <<"Failed to initialize super-agent">>,
                reason => Reason
            }),
            {reply, {text, ErrorMsg}, State}
    end.

websocket_handle({text, Msg}, State) ->
    case jsx:decode(Msg, [return_maps]) of
        #{<<"type">> := <<"command">>, <<"data">> := Command} ->
            handle_websocket_command(Command, State);
        #{<<"type">> := <<"chat">>, <<"message">> := Message} ->
            handle_websocket_chat(Message, State);
        #{<<"type">> := <<"system_control">>, <<"action">> := Action, <<"params">> := Params} ->
            handle_system_control(Action, Params, State);
        _ ->
            ErrorMsg = jsx:encode(#{
                type => <<"error">>,
                message => <<"Invalid message format">>
            }),
            {reply, {text, ErrorMsg}, State}
    end;

websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info({stream_token, Token}, State) ->
    TokenMsg = jsx:encode(#{
        type => <<"stream_token">>,
        token => Token,
        agent_id => State#state.agent_id
    }),
    {reply, {text, TokenMsg}, State};

websocket_info({stream_complete, Response}, State) ->
    CompleteMsg = jsx:encode(#{
        type => <<"stream_complete">>,
        response => Response,
        agent_id => State#state.agent_id
    }),
    {reply, {text, CompleteMsg}, State};

websocket_info({system_event, Event}, State) ->
    EventMsg = jsx:encode(#{
        type => <<"system_event">>,
        event => Event,
        timestamp => erlang:timestamp()
    }),
    {reply, {text, EventMsg}, State};

websocket_info(_Info, State) ->
    {ok, State}.

%% Internal functions

create_super_agent_if_needed() ->
    case super_agent:get_instance() of
        {ok, AgentId} ->
            {ok, AgentId};
        {error, not_found} ->
            Config = #{
                name => <<"Super-Agent">>,
                type => super_agent,
                model => <<"gpt-4-turbo">>,
                system_prompt => get_super_agent_prompt(),
                tools => super_agent_tools:get_all_tools(),
                privileges => system_admin,
                access_level => unrestricted
            },
            super_agent:create(Config);
        {error, Reason} ->
            {error, Reason}
    end.

get_super_agent_prompt() ->
    <<"You are a Super-Agent with full system administration privileges in an Erlang/OTP distributed agent system. 

You have access to:
- System introspection and modification capabilities
- Process lifecycle management
- Dynamic supervisor creation and management  
- Agent creation, modification, and coordination
- Environment variable manipulation
- Hot code loading and system updates
- Distributed system coordination
- Real-time system monitoring and health checks

You can execute system commands, modify the running system, create new agents and supervisors, 
and coordinate complex distributed operations. Always explain what you're doing and potential 
impacts before making significant system changes.

Available command prefixes:
- /system - Direct system commands
- /agents - Agent management  
- /supervisors - Supervisor operations
- /env - Environment management
- /code - Hot code loading
- /monitor - System monitoring
- /distributed - Cluster operations

Use your capabilities responsibly and provide detailed explanations of system changes.">>.

handle_websocket_command(Command, State) ->
    case super_agent:execute_realtime_command(State#state.agent_id, Command, self()) of
        {ok, streaming} ->
            AckMsg = jsx:encode(#{
                type => <<"command_started">>,
                command => Command,
                agent_id => State#state.agent_id
            }),
            {reply, {text, AckMsg}, State};
        {ok, Result} ->
            ResultMsg = jsx:encode(#{
                type => <<"command_result">>,
                result => Result,
                agent_id => State#state.agent_id
            }),
            {reply, {text, ResultMsg}, State};
        {error, Reason} ->
            ErrorMsg = jsx:encode(#{
                type => <<"command_error">>,
                error => Reason,
                agent_id => State#state.agent_id
            }),
            {reply, {text, ErrorMsg}, State}
    end.

handle_websocket_chat(Message, State) ->
    case super_agent:stream_chat(State#state.agent_id, Message, self()) of
        {ok, streaming} ->
            StartMsg = jsx:encode(#{
                type => <<"chat_started">>,
                message => Message,
                agent_id => State#state.agent_id
            }),
            {reply, {text, StartMsg}, State};
        {error, Reason} ->
            ErrorMsg = jsx:encode(#{
                type => <<"chat_error">>,
                error => Reason,
                agent_id => State#state.agent_id
            }),
            {reply, {text, ErrorMsg}, State}
    end.

handle_system_control(Action, Params, State) ->
    case execute_system_control_action(Action, Params) of
        {ok, Result} ->
            ResultMsg = jsx:encode(#{
                type => <<"system_control_result">>,
                action => Action,
                result => Result,
                timestamp => erlang:timestamp()
            }),
            {reply, {text, ResultMsg}, State};
        {error, Reason} ->
            ErrorMsg = jsx:encode(#{
                type => <<"system_control_error">>,
                action => Action,
                error => Reason
            }),
            {reply, {text, ErrorMsg}, State}
    end.

execute_system_command(<<"create_supervisor">>, Args) ->
    case super_agent_tools:create_dynamic_supervisor(Args) of
        {ok, Pid} -> {ok, #{supervisor_pid => pid_to_binary(Pid), status => created}};
        {error, Reason} -> {error, Reason}
    end;

execute_system_command(<<"modify_agent">>, Args) ->
    case super_agent_tools:modify_agent_behavior(Args) of
        {ok, Result} -> {ok, Result};
        {error, Reason} -> {error, Reason}
    end;

execute_system_command(<<"system_introspect">>, _Args) ->
    case super_agent_tools:deep_system_introspection() of
        {ok, Introspection} -> {ok, Introspection};
        {error, Reason} -> {error, Reason}
    end;

execute_system_command(<<"hot_reload">>, Args) ->
    case super_agent_tools:hot_reload_code(Args) of
        {ok, Result} -> {ok, Result};
        {error, Reason} -> {error, Reason}
    end;

execute_system_command(Command, _Args) ->
    {error, <<"Unknown system command: ", Command/binary>>}.

execute_system_control_action(<<"restart_supervisor">>, #{<<"name">> := Name}) ->
    super_agent_tools:restart_supervisor(Name);

execute_system_control_action(<<"scale_agents">>, #{<<"count">> := Count}) ->
    super_agent_tools:scale_agent_pool(Count);

execute_system_control_action(<<"system_health_check">>, _Params) ->
    super_agent_tools:comprehensive_health_check();

execute_system_control_action(Action, _Params) ->
    {error, <<"Unknown system control action: ", Action/binary>>}.

get_active_capabilities() ->
    [
        <<"system_modification">>,
        <<"process_control">>,
        <<"agent_management">>,
        <<"environment_control">>,
        <<"supervisor_creation">>,
        <<"hot_code_loading">>,
        <<"system_introspection">>,
        <<"distributed_coordination">>,
        <<"real_time_monitoring">>,
        <<"security_management">>
    ].

get_system_info() ->
    #{
        node => node(),
        system_version => erlang:system_info(version),
        process_count => erlang:system_info(process_count),
        memory_total => erlang:memory(total),
        memory_processes => erlang:memory(processes),
        uptime => element(1, erlang:statistics(wall_clock)),
        scheduler_count => erlang:system_info(schedulers)
    }.

get_comprehensive_system_status() ->
    #{
        system_info => get_system_info(),
        agent_status => get_agent_status(),
        supervisor_status => get_supervisor_status(),
        network_status => get_network_status(),
        resource_usage => get_resource_usage(),
        active_capabilities => get_active_capabilities()
    }.

get_agent_status() ->
    case agent_supervisor:get_all_agents() of
        {ok, Agents} ->
            #{
                total_agents => length(Agents),
                active_agents => count_active_agents(Agents),
                agent_types => get_agent_type_distribution(Agents)
            };
        {error, Reason} ->
            #{error => Reason}
    end.

get_supervisor_status() ->
    case system_introspection:get_supervisor_tree() of
        {ok, Tree} ->
            #{
                supervisor_tree => Tree,
                total_supervisors => count_supervisors(Tree)
            };
        {error, Reason} ->
            #{error => Reason}
    end.

get_network_status() ->
    #{
        node => node(),
        nodes => nodes(),
        distributed => erlang:is_alive(),
        connections => length(nodes())
    }.

get_resource_usage() ->
    #{
        memory => erlang:memory(),
        process_count => erlang:system_info(process_count),
        port_count => erlang:system_info(port_count),
        ets_count => length(ets:all()),
        scheduler_utilization => erlang:statistics(scheduler_wall_time_all)
    }.

count_active_agents(Agents) ->
    length([A || A <- Agents, is_agent_active(A)]).

is_agent_active(AgentInfo) ->
    case maps:get(status, AgentInfo, undefined) of
        active -> true;
        running -> true;
        _ -> false
    end.

get_agent_type_distribution(Agents) ->
    Types = [maps:get(type, A, unknown) || A <- Agents],
    count_types(Types, #{}).

count_types([], Acc) -> Acc;
count_types([Type | Rest], Acc) ->
    Count = maps:get(Type, Acc, 0),
    count_types(Rest, Acc#{Type => Count + 1}).

count_supervisors(Tree) when is_list(Tree) ->
    lists:sum([count_supervisors(Child) || Child <- Tree]);
count_supervisors(#{type := supervisor, children := Children}) ->
    1 + count_supervisors(Children);
count_supervisors(_) ->
    0.

generate_session_id() ->
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    ID = io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b", 
                       [A, B, C, D, E]),
    list_to_binary(ID).

pid_to_binary(Pid) when is_pid(Pid) ->
    list_to_binary(pid_to_list(Pid)).