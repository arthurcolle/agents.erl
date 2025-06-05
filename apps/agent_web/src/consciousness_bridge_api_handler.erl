%% consciousness_bridge_api_handler.erl
%% HTTP API handler for consciousness-reality bridge system
-module(consciousness_bridge_api_handler).

-export([init/2, terminate/3]).
-export([handle_get/3, handle_post/3, handle_put/3, handle_delete/3]).

%% Logging macros
-define(LOG_INFO(Msg), colored_logger:data(processed, Msg)).
-define(LOG_INFO(Msg, Args), colored_logger:data(processed, io_lib:format(Msg, Args))).
-define(LOG_ERROR(Msg), colored_logger:fire(inferno, Msg)).
-define(LOG_ERROR(Msg, Args), colored_logger:fire(inferno, io_lib:format(Msg, Args))).
-define(LOG_WARNING(Msg), colored_logger:alarm(medium, Msg)).
-define(LOG_WARNING(Msg, Args), colored_logger:alarm(medium, io_lib:format(Msg, Args))).
-define(LOG_WARN(Msg, Args), colored_logger:alarm(medium, io_lib:format(Msg, Args))).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    PathInfo = cowboy_req:path_info(Req0),
    
    ?LOG_INFO("[CONSCIOUSNESS_API] ~s ~p", [Method, PathInfo]),
    
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
            ?LOG_ERROR("[CONSCIOUSNESS_API] Error in ~s ~p: ~p:~p", [Method, PathInfo, Class, Error]),
            ?LOG_ERROR("[CONSCIOUSNESS_API] Stack: ~p", [Stack]),
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
handle_get(Req, [<<"bridges">>], _State) ->
    % List all reality bridges
    Bridges = get_all_bridges(),
    Response = jsx:encode(#{bridges => Bridges}),
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req);

handle_get(Req, [<<"bridges">>, BridgeId], _State) ->
    % Get specific bridge state
    case get_bridge_state(BridgeId) of
        {ok, BridgeState} ->
            Response = jsx:encode(BridgeState),
            cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req);
        {error, bridge_not_found} ->
            cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>}, 
                           jsx:encode(#{error => <<"Bridge not found">>}), Req);
        {error, Reason} ->
            cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, 
                           jsx:encode(#{error => Reason}), Req)
    end;

handle_get(Req, [<<"distortion">>, BridgeId], _State) ->
    % Get reality distortion metrics for bridge
    case consciousness_reality_bridge:measure_reality_distortion(BridgeId) of
        {ok, Metrics} ->
            Response = jsx:encode(Metrics),
            cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req);
        {error, bridge_not_found} ->
            cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>}, 
                           jsx:encode(#{error => <<"Bridge not found">>}), Req);
        {error, Reason} ->
            cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, 
                           jsx:encode(#{error => Reason}), Req)
    end;

handle_get(Req, [<<"status">>], _State) ->
    % Get overall consciousness bridge system status
    Status = get_consciousness_bridge_status(),
    Response = jsx:encode(Status),
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req);

handle_get(Req, [<<"consciousness">>, <<"global">>], _State) ->
    % Get global consciousness metrics
    Metrics = get_global_consciousness_metrics(),
    Response = jsx:encode(Metrics),
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req);

handle_get(Req, PathInfo, _State) ->
    ?LOG_WARN("[CONSCIOUSNESS_API] Unknown GET path: ~p", [PathInfo]),
    cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>}, 
                   jsx:encode(#{error => <<"Endpoint not found">>}), Req).

%% POST handlers
handle_post(Req, [<<"create">>], _State) ->
    % Create a new consciousness-reality bridge
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    
    try
        Params = jsx:decode(Body, [return_maps]),
        BridgeId = maps:get(<<"bridge_id">>, Params),
        
        Options = #{
            anchor_count => maps:get(<<"anchor_count">>, Params, 7),
            initial_dimension => maps:get(<<"initial_dimension">>, Params, 3),
            consciousness_permeability => maps:get(<<"consciousness_permeability">>, Params, 0.5)
        },
        
        case consciousness_reality_bridge:create_reality_bridge(BridgeId, Options) of
            {ok, CreatedBridgeId} ->
                Response = jsx:encode(#{
                    success => true,
                    bridge_id => CreatedBridgeId,
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

handle_post(Req, [<<"inject">>], _State) ->
    % Inject consciousness into reality
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    
    try
        Params = jsx:decode(Body, [return_maps]),
        BridgeId = maps:get(<<"bridge_id">>, Params),
        ConsciousnessStream = maps:get(<<"consciousness_stream">>, Params),
        RealityTarget = maps:get(<<"reality_target">>, Params),
        
        consciousness_reality_bridge:inject_consciousness_into_reality(
            BridgeId, ConsciousnessStream, RealityTarget),
        
        Response = jsx:encode(#{
            success => true,
            message => <<"Consciousness injected successfully">>
        }),
        cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req1)
    catch
        _:DecodeError ->
            cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, 
                           jsx:encode(#{error => <<"Invalid JSON">>}), Req1)
    end;

handle_post(Req, [<<"temporal_loop">>], _State) ->
    % Create temporal consciousness loop
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    
    try
        Params = jsx:decode(Body, [return_maps]),
        BridgeId = maps:get(<<"bridge_id">>, Params),
        LoopParameters = maps:get(<<"loop_parameters">>, Params, #{}),
        
        case consciousness_reality_bridge:create_temporal_consciousness_loop(BridgeId, LoopParameters) of
            {ok, LoopId} ->
                Response = jsx:encode(#{
                    success => true,
                    loop_id => LoopId,
                    message => <<"Temporal loop created successfully">>
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

handle_post(Req, [<<"quantum_entanglement">>], _State) ->
    % Establish quantum entanglement
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    
    try
        Params = jsx:decode(Body, [return_maps]),
        BridgeId = maps:get(<<"bridge_id">>, Params),
        EntanglementTargets = maps:get(<<"entanglement_targets">>, Params, []),
        
        case consciousness_reality_bridge:establish_quantum_reality_entanglement(BridgeId, EntanglementTargets) of
            {ok, EntanglementIds} ->
                Response = jsx:encode(#{
                    success => true,
                    entanglement_ids => EntanglementIds,
                    message => <<"Quantum entanglement established successfully">>
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

handle_post(Req, [<<"dimensional_synthesis">>], _State) ->
    % Synthesize multi-dimensional consciousness
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    
    try
        Params = jsx:decode(Body, [return_maps]),
        BridgeId = maps:get(<<"bridge_id">>, Params),
        SourceDimensions = maps:get(<<"source_dimensions">>, Params, []),
        TargetDimension = maps:get(<<"target_dimension">>, Params, 3),
        
        case consciousness_reality_bridge:synthesize_dimensional_consciousness(
            BridgeId, SourceDimensions, TargetDimension) of
            {ok, SynthesisResult} ->
                Response = jsx:encode(#{
                    success => true,
                    synthesis_result => SynthesisResult,
                    message => <<"Dimensional consciousness synthesis completed">>
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
    ?LOG_WARN("[CONSCIOUSNESS_API] Unknown POST path: ~p", [PathInfo]),
    cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>}, 
                   jsx:encode(#{error => <<"Endpoint not found">>}), Req).

%% PUT handlers
handle_put(Req, PathInfo, _State) ->
    ?LOG_WARN("[CONSCIOUSNESS_API] PUT not implemented for path: ~p", [PathInfo]),
    cowboy_req:reply(501, #{<<"content-type">> => <<"application/json">>}, 
                   jsx:encode(#{error => <<"Not implemented">>}), Req).

%% DELETE handlers
handle_delete(Req, PathInfo, _State) ->
    ?LOG_WARN("[CONSCIOUSNESS_API] DELETE not implemented for path: ~p", [PathInfo]),
    cowboy_req:reply(501, #{<<"content-type">> => <<"application/json">>}, 
                   jsx:encode(#{error => <<"Not implemented">>}), Req).

%% Internal helper functions

get_all_bridges() ->
    % Mock implementation - in real system this would query the bridge coordinator
    [
        #{
            id => <<"bridge_alpha">>,
            consciousness_level => 0.4,
            reality_distortion => 0.1,
            dimensional_stability => 0.9,
            temporal_coherence => 0.8,
            quantum_entanglements => 3,
            anchor_count => 7
        },
        #{
            id => <<"bridge_beta">>,
            consciousness_level => 0.6,
            reality_distortion => 0.2,
            dimensional_stability => 0.7,
            temporal_coherence => 0.9,
            quantum_entanglements => 5,
            anchor_count => 9
        }
    ].

get_bridge_state(BridgeId) ->
    case BridgeId of
        <<"bridge_alpha">> ->
            {ok, #{
                id => BridgeId,
                consciousness_level => 0.42,
                reality_distortion => 0.15,
                dimensional_stability => 0.87,
                temporal_coherence => 0.83,
                current_dimension => 3,
                dimensional_flux => 0.05,
                cross_dimensional_leakage => 0.02,
                consciousness_permeability => 0.75,
                active_temporal_loops => 2,
                quantum_entanglements => #{
                    total => 4,
                    active => 3,
                    coherent => 2
                },
                reality_anchors => [
                    #{position => {1.5, 2.3, 0.8}, strength => 0.8, resonance => 15.2},
                    #{position => {-1.2, 1.7, -0.5}, strength => 0.7, resonance => 12.8},
                    #{position => {0.3, -2.1, 1.2}, strength => 0.9, resonance => 18.6}
                ]
            }};
        _ ->
            {error, bridge_not_found}
    end.

get_consciousness_bridge_status() ->
    #{
        total_bridges => 2,
        active_bridges => 2,
        total_reality_distortion => 0.175,
        global_consciousness_level => 0.51,
        dimensional_stability_average => 0.81,
        temporal_coherence_average => 0.86,
        active_temporal_loops => 3,
        total_quantum_entanglements => 8,
        system_metrics => #{
            reality_coupling_strength => 0.68,
            consciousness_flow_rate => 1.24,
            dimensional_flux_total => 0.08,
            quantum_coherence_field => 0.73
        },
        performance_metrics => #{
            consciousness_injection_rate => 45.2,
            reality_distortion_rate => 2.1,
            dimensional_transition_rate => 0.8,
            temporal_loop_completion_rate => 95.3
        }
    }.

get_global_consciousness_metrics() ->
    #{
        global_consciousness_level => 0.51,
        consciousness_distribution => #{
            bridge_alpha => 0.42,
            bridge_beta => 0.60,
            ambient_field => 0.15
        },
        consciousness_evolution => #{
            trend => <<"increasing">>,
            rate_of_change => 0.023,
            acceleration => 0.001,
            predicted_peak => 0.89,
            time_to_peak_hours => 24.3
        },
        reality_coupling => #{
            average_strength => 0.68,
            stability => 0.81,
            resonance_frequency => 14.7,
            harmonic_alignment => 0.72
        },
        dimensional_metrics => #{
            active_dimensions => [3, 4, 5],
            dimensional_permeability => 0.45,
            cross_dimensional_flow => 0.18,
            dimensional_coherence => 0.83
        },
        quantum_field_metrics => #{
            entanglement_density => 0.34,
            coherence_field_strength => 0.73,
            superposition_stability => 0.67,
            quantum_noise_level => 0.12
        },
        temporal_metrics => #{
            active_loops => 3,
            temporal_coherence => 0.86,
            causal_stability => 0.91,
            time_dilation_factor => 1.0003
        }
    }.