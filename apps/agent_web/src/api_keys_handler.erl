-module(api_keys_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path_info(Req0),
    
    Req = case {Method, Path} of
        {<<"GET">>, undefined} ->
            handle_get_all_keys(Req0);
        {<<"GET">>, [<<"requirements">>]} ->
            handle_get_requirements(Req0);
        {<<"GET">>, [<<"check">>]} ->
            handle_check_keys(Req0);
        {<<"GET">>, [Service]} ->
            handle_get_service_keys(Req0, Service);
        {<<"POST">>, [Service]} ->
            handle_save_service_keys(Req0, Service);
        {<<"POST">>, [<<"load-env">>]} ->
            handle_load_env(Req0);
        _ ->
            cowboy_req:reply(404, #{}, <<"Not Found">>, Req0)
    end,
    
    {ok, Req, State}.

handle_get_all_keys(Req) ->
    case is_api_key_manager_available() of
        false ->
            cowboy_req:reply(503,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{error => <<"API key manager not ready yet">>}),
                Req);
        true ->
            case api_key_manager:get_all_keys() of
                {ok, Keys} ->
                    %% Also include missing keys info
                    {ok, Missing} = api_key_manager:check_required_keys(),
                    
                    Response = #{
                        keys => Keys,
                        missing => format_missing_keys(Missing),
                        requirements => api_key_manager:get_api_requirements()
                    },
                    
                    cowboy_req:reply(200,
                        #{<<"content-type">> => <<"application/json">>},
                        jsx:encode(Response),
                        Req);
                {error, Reason} ->
                    cowboy_req:reply(500,
                        #{<<"content-type">> => <<"application/json">>},
                        jsx:encode(#{error => format_error(Reason)}),
                        Req)
            end
    end.

handle_get_requirements(Req) ->
    case is_api_key_manager_available() of
        false ->
            cowboy_req:reply(503,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{error => <<"API key manager not ready yet">>}),
                Req);
        true ->
            Requirements = api_key_manager:get_api_requirements(),
            
            cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{requirements => Requirements}),
                Req)
    end.

handle_check_keys(Req) ->
    case is_api_key_manager_available() of
        false ->
            cowboy_req:reply(503,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{error => <<"API key manager not ready yet">>}),
                Req);
        true ->
            {ok, Missing} = api_key_manager:check_required_keys(),
            
            Response = #{
                status => case Missing of
                    [] -> <<"all_configured">>;
                    _ -> <<"missing_keys">>
                end,
                missing => format_missing_keys(Missing),
                details => get_detailed_status()
            },
            
            cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(Response),
                Req)
    end.

handle_get_service_keys(Req, Service) ->
    case is_api_key_manager_available() of
        false ->
            cowboy_req:reply(503,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{error => <<"API key manager not ready yet">>}),
                Req);
        true ->
            ServiceAtom = binary_to_atom(Service, utf8),
            {ok, AllKeys} = api_key_manager:get_all_keys(),
            
            case maps:get(ServiceAtom, AllKeys, undefined) of
                undefined ->
                    cowboy_req:reply(404,
                        #{<<"content-type">> => <<"application/json">>},
                        jsx:encode(#{error => <<"Service not found">>}),
                        Req);
                ServiceKeys ->
                    Requirements = maps:get(ServiceAtom, api_key_manager:get_api_requirements(), #{}),
                    
                    Response = #{
                        service => Service,
                        keys => ServiceKeys,
                        requirements => Requirements
                    },
                    
                    cowboy_req:reply(200,
                        #{<<"content-type">> => <<"application/json">>},
                        jsx:encode(Response),
                        Req)
            end
    end.

handle_save_service_keys(Req0, Service) ->
    {ok, Body, Req} = cowboy_req:read_body(Req0),
    
    try
        KeyData = jsx:decode(Body, [return_maps]),
        ServiceAtom = binary_to_atom(Service, utf8),
        
        case is_api_key_manager_available() of
            false ->
                cowboy_req:reply(503,
                    #{<<"content-type">> => <<"application/json">>},
                    jsx:encode(#{error => <<"API key manager not ready yet">>}),
                    Req);
            true ->
                case api_key_manager:save_key(ServiceAtom, KeyData) of
                    ok ->
                        %% Also update environment variables
                        update_env_vars(ServiceAtom, KeyData),
                        
                        cowboy_req:reply(200,
                            #{<<"content-type">> => <<"application/json">>},
                            jsx:encode(#{status => <<"saved">>, service => Service}),
                            Req);
                    {error, Reason} ->
                        cowboy_req:reply(400,
                            #{<<"content-type">> => <<"application/json">>},
                            jsx:encode(#{error => format_error(Reason)}),
                            Req)
                end
        end
    catch
        error:badarg ->
            cowboy_req:reply(400,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{error => <<"Invalid JSON">>}),
                Req)
    end.

handle_load_env(Req) ->
    case is_api_key_manager_available() of
        false ->
            cowboy_req:reply(503,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{error => <<"API key manager not ready yet">>}),
                Req);
        true ->
            ok = api_key_manager:load_from_env(),
            
            cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{status => <<"loaded">>, message => <<"Environment variables loaded">>}),
                Req)
    end.

%% Helper functions
format_missing_keys(Missing) ->
    lists:map(fun({Service, Fields}) ->
        #{
            service => atom_to_binary(Service, utf8),
            missing_fields => [atom_to_binary(F, utf8) || F <- Fields]
        }
    end, Missing).

format_error({missing_required_field, Field}) ->
    iolist_to_binary([<<"Missing required field: ">>, atom_to_binary(Field, utf8)]);
format_error({empty_required_field, Field}) ->
    iolist_to_binary([<<"Empty required field: ">>, atom_to_binary(Field, utf8)]);
format_error({invalid_format, Field, Pattern}) ->
    iolist_to_binary([<<"Invalid format for field ">>, atom_to_binary(Field, utf8), 
                     <<". Expected pattern: ">>, Pattern]);
format_error({unknown_service, Service}) ->
    iolist_to_binary([<<"Unknown service: ">>, atom_to_binary(Service, utf8)]);
format_error(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).

get_detailed_status() ->
    Requirements = api_key_manager:get_api_requirements(),
    {ok, AllKeys} = api_key_manager:get_all_keys(),
    
    maps:map(fun(Service, ServiceReqs) ->
        ServiceKeys = maps:get(Service, AllKeys, #{}),
        RequiredFields = maps:get(required_fields, ServiceReqs, []),
        
        FieldStatus = lists:map(fun(FieldSpec) ->
            FieldKey = maps:get(key, FieldSpec),
            Value = maps:get(FieldKey, ServiceKeys, <<>>),
            #{
                field => atom_to_binary(FieldKey, utf8),
                configured => Value =/= <<>>,
                description => maps:get(description, FieldSpec, <<>>)
            }
        end, RequiredFields),
        
        #{
            name => maps:get(name, ServiceReqs),
            configured => lists:all(fun(#{configured := C}) -> C end, FieldStatus),
            fields => FieldStatus,
            documentation_url => maps:get(documentation_url, ServiceReqs, <<>>)
        }
    end, Requirements).

update_env_vars(Service, KeyData) ->
    Requirements = maps:get(Service, api_key_manager:get_api_requirements(), #{}),
    AllFields = maps:get(required_fields, Requirements, []) ++ 
                maps:get(optional_fields, Requirements, []),
    
    lists:foreach(fun(FieldSpec) ->
        FieldKey = maps:get(key, FieldSpec),
        EnvVar = maps:get(env, FieldSpec),
        
        case maps:get(atom_to_binary(FieldKey, utf8), KeyData, undefined) of
            undefined -> ok;
            Value when is_binary(Value) ->
                os:putenv(EnvVar, binary_to_list(Value));
            _ -> ok
        end
    end, AllFields).

%% Check if api_key_manager process is available
is_api_key_manager_available() ->
    case whereis(api_key_manager) of
        undefined -> false;
        Pid when is_pid(Pid) -> is_process_alive(Pid)
    end.