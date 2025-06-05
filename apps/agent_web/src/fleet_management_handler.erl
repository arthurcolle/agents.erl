%% fleet_management_handler.erl
%% Web handler for fleet management operations
-module(fleet_management_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0 = #{method := <<"GET">>, path := <<"/api/fleet/agents">>}, State) ->
    % List all agents in the fleet
    Response = handle_list_fleet_agents(),
    Req = cowboy_req:reply(200, 
        #{<<"content-type">> => <<"application/json">>}, 
        jsx:encode(Response), Req0),
    {ok, Req, State};

init(Req0 = #{method := <<"POST">>, path := <<"/api/fleet/agents/create">>}, State) ->
    % Create multiple agents (fleet creation)
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    try jsx:decode(Body, [return_maps]) of
        Request ->
            Response = handle_create_fleet(Request),
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(Response), Req1),
            {ok, Req, State}
    catch
        _:_ ->
            ErrorResponse = #{success => false, error => <<"Invalid JSON">>},
            Req = cowboy_req:reply(400,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(ErrorResponse), Req1),
            {ok, Req, State}
    end;

init(Req0 = #{method := <<"POST">>, path := <<"/api/fleet/autonomous/enable">>}, State) ->
    % Enable autonomous mode for entire fleet
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    try jsx:decode(Body, [return_maps]) of
        Request ->
            Response = handle_fleet_autonomous_enable(Request),
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(Response), Req1),
            {ok, Req, State}
    catch
        _:_ ->
            ErrorResponse = #{success => false, error => <<"Invalid JSON">>},
            Req = cowboy_req:reply(400,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(ErrorResponse), Req1),
            {ok, Req, State}
    end;

init(Req0 = #{method := <<"POST">>, path := <<"/api/fleet/autonomous/execute">>}, State) ->
    % Execute autonomous operations across fleet
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    try jsx:decode(Body, [return_maps]) of
        Request ->
            Response = handle_fleet_autonomous_execute(Request),
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(Response), Req1),
            {ok, Req, State}
    catch
        _:_ ->
            ErrorResponse = #{success => false, error => <<"Invalid JSON">>},
            Req = cowboy_req:reply(400,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(ErrorResponse), Req1),
            {ok, Req, State}
    end;

init(Req0 = #{method := <<"GET">>, path := <<"/api/fleet/status">>}, State) ->
    % Get comprehensive fleet status
    Response = handle_fleet_status(),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Response), Req0),
    {ok, Req, State};

init(Req0 = #{method := <<"POST">>, path := <<"/api/fleet/broadcast">>}, State) ->
    % Broadcast message to entire fleet
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    try jsx:decode(Body, [return_maps]) of
        Request ->
            Response = handle_fleet_broadcast(Request),
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(Response), Req1),
            {ok, Req, State}
    catch
        _:_ ->
            ErrorResponse = #{success => false, error => <<"Invalid JSON">>},
            Req = cowboy_req:reply(400,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(ErrorResponse), Req1),
            {ok, Req, State}
    end;

init(Req0, State) ->
    % Handle unsupported methods/paths
    ErrorResponse = #{success => false, error => <<"Endpoint not found">>},
    Req = cowboy_req:reply(404,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(ErrorResponse), Req0),
    {ok, Req, State}.

%% Internal Functions

handle_list_fleet_agents() ->
    try
        % Get all agents from the registry
        case agent_registry:list_agents() of
            {ok, Agents} ->
                AgentDetails = lists:map(fun({AgentId, AgentPid, Metadata}) ->
                    AgentState = try
                        agent_instance:get_state(AgentPid)
                    catch
                        _:_ -> #{id => AgentId, status => <<"offline">>}
                    end,
                    
                    EnhancedState = case is_map(AgentState) of
                        true -> AgentState#{metadata => Metadata};
                        false -> #{id => AgentId, status => <<"error">>, metadata => Metadata}
                    end,
                    
                    EnhancedState
                end, Agents),
                
                #{
                    success => true,
                    fleet_size => length(AgentDetails),
                    agents => AgentDetails,
                    timestamp => erlang:system_time(millisecond)
                };
            {error, Reason} ->
                #{success => false, error => iolist_to_binary(io_lib:format("~p", [Reason]))};
            [] ->
                #{success => true, fleet_size => 0, agents => [], timestamp => erlang:system_time(millisecond)}
        end
    catch
        E:R ->
            #{success => false, error => iolist_to_binary(io_lib:format("~p:~p", [E, R]))}
    end.

handle_create_fleet(#{<<"count">> := Count, <<"template">> := Template}) ->
    try
        FleetSize = min(Count, 20), % Limit fleet size for safety
        
        Results = lists:map(fun(N) ->
            AgentName = iolist_to_binary(io_lib:format("Fleet Agent ~p", [N])),
            AgentConfig = Template#{
                <<"name">> => AgentName,
                <<"id">> => generate_fleet_agent_id(N)
            },
            
            case create_agent_from_config(AgentConfig) of
                {ok, AgentId, _AgentPid} ->
                    #{success => true, agent_id => AgentId, name => AgentName};
                {error, Reason} ->
                    #{success => false, agent_id => null, name => AgentName, error => Reason}
            end
        end, lists:seq(1, FleetSize)),
        
        SuccessCount = length([Result || Result = #{success := true} <- Results]),
        
        #{
            success => true,
            created_count => SuccessCount,
            requested_count => FleetSize,
            results => Results,
            timestamp => erlang:system_time(millisecond)
        }
    catch
        E:R ->
            #{success => false, error => iolist_to_binary(io_lib:format("~p:~p", [E, R]))}
    end;

handle_create_fleet(#{<<"agents">> := AgentConfigs}) ->
    % Create specific agents from individual configurations
    try
        Results = lists:map(fun(Config) ->
            case create_agent_from_config(Config) of
                {ok, AgentId, _AgentPid} ->
                    #{success => true, agent_id => AgentId, config => Config};
                {error, Reason} ->
                    #{success => false, agent_id => null, config => Config, error => Reason}
            end
        end, AgentConfigs),
        
        SuccessCount = length([Result || Result = #{success := true} <- Results]),
        
        #{
            success => true,
            created_count => SuccessCount,
            requested_count => length(AgentConfigs),
            results => Results,
            timestamp => erlang:system_time(millisecond)
        }
    catch
        E:R ->
            #{success => false, error => iolist_to_binary(io_lib:format("~p:~p", [E, R]))}
    end.

handle_fleet_autonomous_enable(#{<<"agent_ids">> := AgentIds}) ->
    try
        Results = lists:map(fun(AgentId) ->
            case agent_registry:get_agent(AgentId) of
                {ok, AgentPid} ->
                    case agent_instance:enable_autonomous_mode(AgentPid) of
                        ok ->
                            #{agent_id => AgentId, success => true};
                        {error, Reason} ->
                            #{agent_id => AgentId, success => false, error => Reason}
                    end;
                {error, not_found} ->
                    #{agent_id => AgentId, success => false, error => <<"Agent not found">>}
            end
        end, AgentIds),
        
        SuccessCount = length([Result || Result = #{success := true} <- Results]),
        
        #{
            success => true,
            enabled_count => SuccessCount,
            total_count => length(AgentIds),
            results => Results,
            timestamp => erlang:system_time(millisecond)
        }
    catch
        E:R ->
            #{success => false, error => iolist_to_binary(io_lib:format("~p:~p", [E, R]))}
    end.

handle_fleet_autonomous_execute(#{<<"message">> := Message, <<"agent_ids">> := AgentIds}) ->
    try
        Action = #{action => <<"chat">>, message => Message},
        
        % Execute autonomously on all specified agents in parallel
        ParentPid = self(),
        Refs = lists:map(fun(AgentId) ->
            Ref = make_ref(),
            spawn_link(fun() ->
                Result = case agent_registry:get_agent(AgentId) of
                    {ok, AgentPid} ->
                        case agent_instance:autonomous_execute(AgentPid, Action) of
                            {ok, Response} ->
                                #{agent_id => AgentId, success => true, response => Response};
                            {error, Reason} ->
                                #{agent_id => AgentId, success => false, error => Reason}
                        end;
                    {error, not_found} ->
                        #{agent_id => AgentId, success => false, error => <<"Agent not found">>}
                end,
                ParentPid ! {autonomous_result, Ref, Result}
            end),
            {Ref, AgentId}
        end, AgentIds),
        
        % Collect results
        Results = collect_autonomous_results(Refs, []),
        SuccessCount = length([Result || Result = #{success := true} <- Results]),
        
        #{
            success => true,
            executed_count => SuccessCount,
            total_count => length(AgentIds),
            results => Results,
            message => Message,
            timestamp => erlang:system_time(millisecond)
        }
    catch
        E:R ->
            #{success => false, error => iolist_to_binary(io_lib:format("~p:~p", [E, R]))}
    end.

handle_fleet_status() ->
    try
        case handle_list_fleet_agents() of
            #{success := true, agents := Agents} = FleetData ->
                % Analyze fleet status
                AutonomousCount = length([Agent || Agent = #{autonomous_mode := true} <- Agents]),
                ActiveCount = length([Agent || Agent = #{status := S} <- Agents, S =/= <<"offline">>]),
                
                % Calculate fleet metrics
                FleetMetrics = #{
                    total_agents => length(Agents),
                    active_agents => ActiveCount,
                    autonomous_agents => AutonomousCount,
                    offline_agents => length(Agents) - ActiveCount,
                    fleet_health => calculate_fleet_health(Agents)
                },
                
                FleetData#{metrics => FleetMetrics};
            Error ->
                Error
        end
    catch
        E:R ->
            #{success => false, error => iolist_to_binary(io_lib:format("~p:~p", [E, R]))}
    end.

handle_fleet_broadcast(#{<<"message">> := Message}) ->
    try
        case agent_registry:list_agents() of
            {ok, Agents} ->
                Results = lists:map(fun({AgentId, AgentPid, _Metadata}) ->
                    try
                        agent_instance:self_message(AgentPid, Message),
                        #{agent_id => AgentId, success => true}
                    catch
                        _:_ ->
                            #{agent_id => AgentId, success => false, error => <<"Message delivery failed">>}
                    end
                end, Agents),
                
                SuccessCount = length([Result || Result = #{success := true} <- Results]),
                
                #{
                    success => true,
                    delivered_count => SuccessCount,
                    total_count => length(Agents),
                    message => Message,
                    results => Results,
                    timestamp => erlang:system_time(millisecond)
                };
            _ ->
                #{success => false, error => <<"No agents available for broadcast">>}
        end
    catch
        E:R ->
            #{success => false, error => iolist_to_binary(io_lib:format("~p:~p", [E, R]))}
    end.

%% Helper Functions

generate_fleet_agent_id(N) ->
    iolist_to_binary(io_lib:format("fleet-agent-~p-~p", [N, erlang:system_time(millisecond)])).

create_agent_from_config(Config) ->
    try
        % Use the existing agent creation system
        Type = maps:get(<<"type">>, Config, <<"ai">>),
        Name = maps:get(<<"name">>, Config, <<"Unnamed Fleet Agent">>),
        Tools = maps:get(<<"tools">>, Config, []),
        
        % Convert to internal format
        InternalConfig = #{
            type => binary_to_atom(Type, utf8),
            name => Name,
            tools => Tools,
            system_prompt => maps:get(<<"system_prompt">>, Config, <<"You are a fleet agent capable of autonomous operation.">>),
            autonomous_mode => maps:get(<<"autonomous_mode">>, Config, false),
            max_autonomous_turns => maps:get(<<"max_autonomous_turns">>, Config, 10)
        },
        
        case agent_instance:start_link(InternalConfig) of
            {ok, AgentPid} ->
                AgentId = maps:get(id, InternalConfig, generate_fleet_agent_id(1)),
                
                % Register the agent
                Metadata = #{
                    type => Type,
                    name => Name,
                    created_via => fleet_management,
                    created_at => erlang:system_time(millisecond)
                },
                agent_registry:register_agent(AgentId, AgentPid, Metadata),
                
                {ok, AgentId, AgentPid};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        E:R ->
            {error, iolist_to_binary(io_lib:format("~p:~p", [E, R]))}
    end.

collect_autonomous_results([], Results) ->
    lists:reverse(Results);
collect_autonomous_results([{Ref, _AgentId} | Remaining], Results) ->
    receive
        {autonomous_result, Ref, Result} ->
            collect_autonomous_results(Remaining, [Result | Results])
    after 30000 ->  % 30 second timeout
        ErrorResult = #{agent_id => <<"timeout">>, success => false, error => <<"Execution timeout">>},
        collect_autonomous_results(Remaining, [ErrorResult | Results])
    end.

calculate_fleet_health(Agents) ->
    case length(Agents) of
        0 -> 0.0;
        Total ->
            ActiveCount = length([Agent || Agent = #{status := S} <- Agents, S =/= <<"offline">>]),
            (ActiveCount / Total) * 100.0
    end.