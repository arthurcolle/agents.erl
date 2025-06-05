%% neural_swarm_api_handler.erl
%% HTTP API handler for neural swarm coordination system
-module(neural_swarm_api_handler).

%% Logging macros
-define(LOG_INFO(Msg), colored_logger:data(processed, Msg)).
-define(LOG_INFO(Msg, Args), colored_logger:data(processed, io_lib:format(Msg, Args))).
-define(LOG_ERROR(Msg), colored_logger:fire(inferno, Msg)).
-define(LOG_ERROR(Msg, Args), colored_logger:fire(inferno, io_lib:format(Msg, Args))).
-define(LOG_WARNING(Msg), colored_logger:alarm(medium, Msg)).
-define(LOG_WARNING(Msg, Args), colored_logger:alarm(medium, io_lib:format(Msg, Args))).
-define(LOG_WARN(Msg, Args), colored_logger:alarm(medium, io_lib:format(Msg, Args))).

-export([init/2, terminate/3]).
-export([handle_get/3, handle_post/3, handle_put/3, handle_delete/3]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    PathInfo = cowboy_req:path_info(Req0),
    
    ?LOG_INFO("[NEURAL_API] ~s ~p", [Method, PathInfo]),
    
    try
        Req1 = case Method of
            <<"GET">> -> handle_get(Req0, PathInfo, State);
            <<"POST">> -> handle_post(Req0, PathInfo, State);
            <<"PUT">> -> handle_put(Req0, PathInfo, State);
            <<"DELETE">> -> handle_delete(Req0, PathInfo, State);
            _ -> 
                cowboy_req:reply(405, #{<<"content-type">> => <<"application/json">>}, 
                               jsx:encode(#{error => <<"Method not allowed">>}), Req0)
        end,
        {ok, Req1, State}
    catch
        Class:Error:Stack ->
            ?LOG_ERROR("[NEURAL_API] Error in ~s ~p: ~p:~p", [Method, PathInfo, Class, Error]),
            ?LOG_ERROR("[NEURAL_API] Stack: ~p", [Stack]),
            ErrorResp = jsx:encode(#{
                error => <<"Internal server error">>,
                details => iolist_to_binary(io_lib:format("~p:~p", [Class, Error]))
            }),
            ReqError = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, 
                                  ErrorResp, Req0),
            {ok, ReqError, State}
    end.

terminate(_Reason, _Req, _State) ->
    ok.

%% GET handlers
handle_get(Req, [<<"networks">>], _State) ->
    % List all neural networks
    Networks = get_all_networks(),
    Response = jsx:encode(#{networks => Networks}),
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req);

handle_get(Req, [<<"networks">>, NetworkId], _State) ->
    % Get specific network state
    case neural_swarm_coordinator:get_network_state(NetworkId) of
        {ok, NetworkState} ->
            Response = jsx:encode(NetworkState),
            cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req);
        {error, network_not_found} ->
            cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>}, 
                           jsx:encode(#{error => <<"Network not found">>}), Req);
        {error, Reason} ->
            cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, 
                           jsx:encode(#{error => Reason}), Req)
    end;

handle_get(Req, [<<"status">>], _State) ->
    % Get overall neural swarm status
    Status = get_neural_swarm_status(),
    Response = jsx:encode(Status),
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req);

handle_get(Req, [<<"consciousness">>, <<"metrics">>], _State) ->
    % Get consciousness metrics across all networks
    Metrics = get_consciousness_metrics(),
    Response = jsx:encode(Metrics),
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req);

handle_get(Req, PathInfo, _State) ->
    ?LOG_WARN("[NEURAL_API] Unknown GET path: ~p", [PathInfo]),
    cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>}, 
                   jsx:encode(#{error => <<"Endpoint not found">>}), Req).

%% POST handlers
handle_post(Req, [<<"create">>], _State) ->
    % Create a new neural network
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    
    try
        Params = jsx:decode(Body, [return_maps]),
        NetworkId = maps:get(<<"network_id">>, Params),
        Topology = binary_to_atom(maps:get(<<"topology">>, Params, <<"dynamic">>), utf8),
        
        Options = #{
            node_count => maps:get(<<"node_count">>, Params, 50),
            learning_rate => maps:get(<<"learning_rate">>, Params, 0.01),
            emergence_threshold => maps:get(<<"emergence_threshold">>, Params, 0.7)
        },
        
        case neural_swarm_coordinator:create_neural_network(NetworkId, Topology, Options) of
            {ok, CreatedNetworkId} ->
                Response = jsx:encode(#{
                    success => true,
                    network_id => CreatedNetworkId,
                    topology => Topology,
                    options => Options
                }),
                cowboy_req:reply(201, #{<<"content-type">> => <<"application/json">>}, Response, Req1);
            {error, Reason} ->
                cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, 
                               jsx:encode(#{error => Reason}), Req1)
        end
    catch
        _:DecodeError ->
            cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, 
                           jsx:encode(#{error => <<"Invalid JSON">>, details => DecodeError}), Req1)
    end;

handle_post(Req, [<<"propagate">>], _State) ->
    % Propagate a signal through neural network
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    
    try
        Params = jsx:decode(Body, [return_maps]),
        NetworkId = maps:get(<<"network_id">>, Params),
        SourceNode = maps:get(<<"source_node">>, Params),
        Signal = maps:get(<<"signal">>, Params),
        
        neural_swarm_coordinator:propagate_signal(NetworkId, SourceNode, Signal),
        
        Response = jsx:encode(#{
            success => true,
            message => <<"Signal propagated successfully">>
        }),
        cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req1)
    catch
        _:DecodeError ->
            cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, 
                           jsx:encode(#{error => <<"Invalid JSON">>}), Req1)
    end;

handle_post(Req, [<<"evolve">>], _State) ->
    % Evolve network topology
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    
    try
        Params = jsx:decode(Body, [return_maps]),
        NetworkId = maps:get(<<"network_id">>, Params),
        
        % Simple fitness function for now
        FitnessFunction = fun(_Network) -> rand:uniform() end,
        
        case neural_swarm_coordinator:evolve_topology(NetworkId, FitnessFunction) of
            {ok, evolution_complete} ->
                Response = jsx:encode(#{
                    success => true,
                    message => <<"Network topology evolved successfully">>
                }),
                cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req1);
            {error, Reason} ->
                cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, 
                               jsx:encode(#{error => Reason}), Req1)
        end
    catch
        _:DecodeError ->
            cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, 
                           jsx:encode(#{error => <<"Invalid JSON">>}), Req1)
    end;

handle_post(Req, [<<"inject_noise">>], _State) ->
    % Inject quantum noise for enhanced creativity
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    
    try
        Params = jsx:decode(Body, [return_maps]),
        NetworkId = maps:get(<<"network_id">>, Params),
        NoiseLevel = maps:get(<<"noise_level">>, Params, 0.1),
        
        neural_swarm_coordinator:inject_quantum_noise(NetworkId, NoiseLevel),
        
        Response = jsx:encode(#{
            success => true,
            message => <<"Quantum noise injected successfully">>,
            noise_level => NoiseLevel
        }),
        cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req1)
    catch
        _:DecodeError ->
            cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, 
                           jsx:encode(#{error => <<"Invalid JSON">>}), Req1)
    end;

handle_post(Req, [<<"hybrid_consciousness">>], _State) ->
    % Create hybrid human-AI consciousness
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    
    try
        Params = jsx:decode(Body, [return_maps]),
        NetworkId = maps:get(<<"network_id">>, Params),
        HumanInputs = maps:get(<<"human_inputs">>, Params, []),
        
        case neural_swarm_coordinator:create_hybrid_consciousness(NetworkId, HumanInputs) of
            {ok, hybrid_consciousness_created} ->
                Response = jsx:encode(#{
                    success => true,
                    message => <<"Hybrid consciousness bridge created successfully">>
                }),
                cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req1);
            {error, Reason} ->
                cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, 
                               jsx:encode(#{error => Reason}), Req1)
        end
    catch
        _:DecodeError ->
            cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, 
                           jsx:encode(#{error => <<"Invalid JSON">>}), Req1)
    end;

handle_post(Req, PathInfo, _State) ->
    ?LOG_WARN("[NEURAL_API] Unknown POST path: ~p", [PathInfo]),
    cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>}, 
                   jsx:encode(#{error => <<"Endpoint not found">>}), Req).

%% PUT handlers
handle_put(Req, PathInfo, _State) ->
    ?LOG_WARN("[NEURAL_API] PUT not implemented for path: ~p", [PathInfo]),
    cowboy_req:reply(501, #{<<"content-type">> => <<"application/json">>}, 
                   jsx:encode(#{error => <<"Not implemented">>}), Req).

%% DELETE handlers
handle_delete(Req, PathInfo, _State) ->
    ?LOG_WARN("[NEURAL_API] DELETE not implemented for path: ~p", [PathInfo]),
    cowboy_req:reply(501, #{<<"content-type">> => <<"application/json">>}, 
                   jsx:encode(#{error => <<"Not implemented">>}), Req).

%% Internal helper functions

get_all_networks() ->
    % Mock implementation - in real system this would query the coordinator
    [
        #{
            id => <<"network_alpha">>,
            topology => <<"quantum_entangled">>,
            node_count => 50,
            consciousness_level => 0.3,
            emergence_score => 0.15
        },
        #{
            id => <<"network_beta">>,
            topology => <<"hierarchical">>,
            node_count => 75,
            consciousness_level => 0.45,
            emergence_score => 0.22
        }
    ].

get_neural_swarm_status() ->
    #{
        total_networks => 2,
        total_nodes => 125,
        total_connections => 340,
        global_consciousness => 0.38,
        system_coherence => 0.72,
        emergence_patterns => #{
            synchronization_clusters => 3,
            oscillation_patterns => 5,
            coherence_waves => 2
        },
        performance_metrics => #{
            cpu_usage => 15.3,
            memory_usage => 234.7,
            network_throughput => 1250.8
        }
    }.

get_consciousness_metrics() ->
    #{
        global_consciousness_level => 0.42,
        consciousness_distribution => [
            #{network_id => <<"network_alpha">>, consciousness => 0.35},
            #{network_id => <<"network_beta">>, consciousness => 0.48}
        ],
        consciousness_trends => #{
            last_hour_change => 0.05,
            last_day_change => 0.15,
            peak_consciousness => 0.67,
            low_consciousness => 0.12
        },
        quantum_metrics => #{
            coherence => 0.73,
            entanglement => 0.45,
            superposition => 0.62
        },
        emergence_metrics => #{
            pattern_complexity => 0.58,
            self_organization => 0.41,
            adaptive_capacity => 0.67
        }
    }.