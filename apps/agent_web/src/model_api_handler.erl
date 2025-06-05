-module(model_api_handler).

-export([init/2, allowed_methods/2, content_types_provided/2, content_types_accepted/2]).
-export([handle_get/2, handle_post/2, handle_put/2, handle_delete/2]).
-export([resource_exists/2, allow_missing_post/2]).

-include_lib("kernel/include/logger.hrl").

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>, <<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, handle_get}
    ], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {<<"application/json">>, handle_post}
    ], Req, State}.

resource_exists(Req, State) ->
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path_info(Req),
    
    case {Method, Path} of
        {<<"GET">>, []} ->
            {true, Req, State};
        {<<"GET">>, [ModelId]} ->
            case model_definition_manager:get_model(ModelId) of
                {ok, _Model} -> {true, Req, State};
                {error, not_found} -> {false, Req, State}
            end;
        {<<"PUT">>, [ModelId]} ->
            case model_definition_manager:get_model(ModelId) of
                {ok, _Model} -> {true, Req, State};
                {error, not_found} -> {false, Req, State}
            end;
        {<<"DELETE">>, [ModelId]} ->
            case model_definition_manager:get_model(ModelId) of
                {ok, _Model} -> {true, Req, State};
                {error, not_found} -> {false, Req, State}
            end;
        {<<"POST">>, [<<"validate">>]} ->
            {true, Req, State};
        {<<"GET">>, [ModelId, _Language]} ->
            case model_definition_manager:get_model(ModelId) of
                {ok, _Model} -> {true, Req, State};
                {error, not_found} -> {false, Req, State}
            end;
        _ ->
            {false, Req, State}
    end.

allow_missing_post(Req, State) ->
    {true, Req, State}.

handle_get(Req, State) ->
    case cowboy_req:path_info(Req) of
        [] ->
            handle_get_all_models(Req, State);
        [ModelId] ->
            handle_get_model(Req, State, ModelId);
        [ModelId, Language] ->
            handle_generate_code(Req, State, ModelId, Language);
        _ ->
            ErrorResponse = jsx:encode(#{
                <<"error">> => <<"invalid_path">>,
                <<"message">> => <<"Invalid API path">>
            }),
            Req2 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, ErrorResponse, Req),
            {stop, Req2, State}
    end.

handle_get_all_models(Req, State) ->
    Result = try
        model_definition_manager:get_all_models()
    catch
        Error:Reason ->
            ?LOG_ERROR("Exception in get_all_models: ~p:~p", [Error, Reason]),
            {error, exception}
    end,
    
    case Result of
        {ok, Models} ->
            Response = jsx:encode(#{
                <<"success">> => true,
                <<"models">> => Models
            }),
            {Response, Req, State};
        {error, _Reason} ->
            ErrorResponse = jsx:encode(#{
                <<"error">> => <<"server_error">>,
                <<"message">> => <<"Failed to retrieve models">>
            }),
            Req2 = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, ErrorResponse, Req),
            {stop, Req2, State}
    end.

handle_get_model(Req, State, ModelId) ->
    Result = try
        model_definition_manager:get_model(ModelId)
    catch
        Error:Reason ->
            ?LOG_ERROR("Exception in get_model: ~p:~p", [Error, Reason]),
            {error, exception}
    end,
    
    case Result of
        {ok, Model} ->
            Response = jsx:encode(#{
                <<"success">> => true,
                <<"model">> => Model
            }),
            {Response, Req, State};
        {error, not_found} ->
            ErrorResponse = jsx:encode(#{
                <<"error">> => <<"not_found">>,
                <<"message">> => <<"Model not found">>
            }),
            Req2 = cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>}, ErrorResponse, Req),
            {stop, Req2, State};
        {error, _Reason} ->
            ErrorResponse = jsx:encode(#{
                <<"error">> => <<"server_error">>,
                <<"message">> => <<"Failed to retrieve model">>
            }),
            Req2 = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, ErrorResponse, Req),
            {stop, Req2, State}
    end.

handle_post(Req, State) ->
    case cowboy_req:path_info(Req) of
        [<<"validate">>] ->
            handle_validate_field(Req, State);
        [] ->
            handle_create_model(Req, State);
        _ ->
            ErrorResponse = jsx:encode(#{
                <<"error">> => <<"invalid_path">>,
                <<"message">> => <<"Invalid POST path">>
            }),
            Req2 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, ErrorResponse, Req),
            {stop, Req2, State}
    end.

handle_create_model(Req, State) ->
    try
        {ok, Body, Req2} = cowboy_req:read_body(Req),
        
        case jsx:decode(Body, [return_maps]) of
            ModelData when is_map(ModelData) ->
                case validate_model_data(ModelData) of
                    ok ->
                        case model_definition_manager:create_model(ModelData) of
                            {ok, ModelId} ->
                                ?LOG_INFO("Created model via API: ~p", [ModelId]),
                                
                                Response = jsx:encode(#{
                                    <<"success">> => true,
                                    <<"model_id">> => ModelId,
                                    <<"message">> => <<"Model created successfully">>
                                }),
                                
                                Req3 = cowboy_req:reply(201, #{
                                    <<"content-type">> => <<"application/json">>,
                                    <<"location">> => <<"/api/models/definitions/", ModelId/binary>>
                                }, Response, Req2),
                                {stop, Req3, State};
                            {error, Reason} ->
                                ?LOG_ERROR("Failed to create model via API: ~p", [Reason]),
                                ErrorResponse = jsx:encode(#{
                                    <<"error">> => <<"creation_failed">>,
                                    <<"message">> => <<"Failed to create model">>,
                                    <<"details">> => format_error(Reason)
                                }),
                                Req3 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, ErrorResponse, Req2),
                                {stop, Req3, State}
                        end;
                    {error, ValidationErrors} ->
                        ErrorResponse = jsx:encode(#{
                            <<"error">> => <<"validation_failed">>,
                            <<"message">> => <<"Invalid model data">>,
                            <<"errors">> => ValidationErrors
                        }),
                        Req3 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, ErrorResponse, Req2),
                        {stop, Req3, State}
                end;
            _ ->
                ErrorResponse = jsx:encode(#{
                    <<"error">> => <<"invalid_json">>,
                    <<"message">> => <<"Request body must be valid JSON object">>
                }),
                Req3 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, ErrorResponse, Req2),
                {stop, Req3, State}
        end
    catch
        Error1:Reason1 ->
            ?LOG_ERROR("Exception in handle_post: ~p:~p", [Error1, Reason1]),
            ErrorResponse1 = jsx:encode(#{
                <<"error">> => <<"server_error">>,
                <<"message">> => <<"Internal server error">>
            }),
            Req1 = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, ErrorResponse1, Req),
            {stop, Req1, State}
    end.

handle_put(Req, State) ->
    case cowboy_req:path_info(Req) of
        [ModelId] ->
            handle_put_model(Req, State, ModelId);
        _ ->
            ErrorResponse = jsx:encode(#{
                <<"error">> => <<"invalid_path">>,
                <<"message">> => <<"Model ID required for PUT requests">>
            }),
            Req2 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, ErrorResponse, Req),
            {stop, Req2, State}
    end.

handle_put_model(Req, State, ModelId) ->
    try
        {ok, Body, Req2} = cowboy_req:read_body(Req),
        
        case jsx:decode(Body, [return_maps]) of
            ModelData when is_map(ModelData) ->
                case validate_model_data(ModelData) of
                    ok ->
                        case model_definition_manager:update_model(ModelId, ModelData) of
                            {ok, UpdatedModelId} ->
                                ?LOG_INFO("Updated model via API: ~p", [UpdatedModelId]),
                                
                                Response = jsx:encode(#{
                                    <<"success">> => true,
                                    <<"model_id">> => UpdatedModelId,
                                    <<"message">> => <<"Model updated successfully">>
                                }),
                                
                                Req3 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req2),
                                {stop, Req3, State};
                            {error, not_found} ->
                                ErrorResponse = jsx:encode(#{
                                    <<"error">> => <<"not_found">>,
                                    <<"message">> => <<"Model not found">>
                                }),
                                Req3 = cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>}, ErrorResponse, Req2),
                                {stop, Req3, State};
                            {error, Reason} ->
                                ?LOG_ERROR("Failed to update model ~p via API: ~p", [ModelId, Reason]),
                                ErrorResponse = jsx:encode(#{
                                    <<"error">> => <<"update_failed">>,
                                    <<"message">> => <<"Failed to update model">>,
                                    <<"details">> => format_error(Reason)
                                }),
                                Req3 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, ErrorResponse, Req2),
                                {stop, Req3, State}
                        end;
                    {error, ValidationErrors} ->
                        ErrorResponse = jsx:encode(#{
                            <<"error">> => <<"validation_failed">>,
                            <<"message">> => <<"Invalid model data">>,
                            <<"errors">> => ValidationErrors
                        }),
                        Req3 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, ErrorResponse, Req2),
                        {stop, Req3, State}
                end;
            _ ->
                ErrorResponse = jsx:encode(#{
                    <<"error">> => <<"invalid_json">>,
                    <<"message">> => <<"Request body must be valid JSON object">>
                }),
                Req3 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, ErrorResponse, Req2),
                {stop, Req3, State}
        end
    catch
        Error2:Reason2 ->
            ?LOG_ERROR("Exception in handle_put: ~p:~p", [Error2, Reason2]),
            ErrorResponse2 = jsx:encode(#{
                <<"error">> => <<"server_error">>,
                <<"message">> => <<"Internal server error">>
            }),
            Req4 = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, ErrorResponse2, Req),
            {stop, Req4, State}
    end.

handle_delete(Req, State) ->
    case cowboy_req:path_info(Req) of
        [ModelId] ->
            handle_delete_model(Req, State, ModelId);
        _ ->
            ErrorResponse = jsx:encode(#{
                <<"error">> => <<"invalid_path">>,
                <<"message">> => <<"Model ID required for DELETE requests">>
            }),
            Req2 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, ErrorResponse, Req),
            {stop, Req2, State}
    end.

handle_delete_model(Req, State, ModelId) ->
    try
        case model_definition_manager:delete_model(ModelId) of
            ok ->
                ?LOG_INFO("Deleted model via API: ~p", [ModelId]),
                
                Response = jsx:encode(#{
                    <<"success">> => true,
                    <<"message">> => <<"Model deleted successfully">>
                }),
                
                Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req),
                {stop, Req2, State};
            {error, not_found} ->
                ErrorResponse = jsx:encode(#{
                    <<"error">> => <<"not_found">>,
                    <<"message">> => <<"Model not found">>
                }),
                Req2 = cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>}, ErrorResponse, Req),
                {stop, Req2, State};
            {error, Reason} ->
                ?LOG_ERROR("Failed to delete model ~p via API: ~p", [ModelId, Reason]),
                ErrorResponse = jsx:encode(#{
                    <<"error">> => <<"deletion_failed">>,
                    <<"message">> => <<"Failed to delete model">>,
                    <<"details">> => format_error(Reason)
                }),
                Req2 = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, ErrorResponse, Req),
                {stop, Req2, State}
        end
    catch
        Error3:Reason3 ->
            ?LOG_ERROR("Exception in handle_delete: ~p:~p", [Error3, Reason3]),
            ErrorResponse3 = jsx:encode(#{
                <<"error">> => <<"server_error">>,
                <<"message">> => <<"Internal server error">>
            }),
            Req5 = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, ErrorResponse3, Req),
            {stop, Req5, State}
    end.

%%% Internal Functions

validate_model_data(ModelData) ->
    RequiredFields = [<<"name">>],
    
    ValidationResults = [
        validate_required_fields(ModelData, RequiredFields),
        validate_name_field(ModelData),
        validate_fields_array(ModelData),
        validate_relationships_array(ModelData)
    ],
    
    Errors = lists:flatten([Error || {error, Error} <- ValidationResults]),
    
    case Errors of
        [] -> ok;
        _ -> {error, Errors}
    end.

validate_required_fields(ModelData, RequiredFields) ->
    MissingFields = [Field || Field <- RequiredFields, not maps:is_key(Field, ModelData)],
    case MissingFields of
        [] -> ok;
        _ -> {error, #{<<"missing_fields">> => MissingFields}}
    end.

validate_name_field(ModelData) ->
    case maps:get(<<"name">>, ModelData, undefined) of
        undefined -> ok;
        Name when is_binary(Name), byte_size(Name) > 0 -> ok;
        _ -> {error, #{<<"name">> => <<"must be a non-empty string">>}}
    end.

validate_fields_array(ModelData) ->
    case maps:get(<<"fields">>, ModelData, []) of
        Fields when is_list(Fields) ->
            FieldValidations = [validate_field_object(Field, Index) || {Field, Index} <- lists:zip(Fields, lists:seq(1, length(Fields)))],
            FieldErrors = [Error || {error, Error} <- FieldValidations],
            case FieldErrors of
                [] -> ok;
                _ -> {error, #{<<"fields">> => FieldErrors}}
            end;
        _ ->
            {error, #{<<"fields">> => <<"must be an array">>}}
    end.

validate_field_object(Field, Index) when is_map(Field) ->
    RequiredFieldKeys = [<<"name">>, <<"type">>],
    ValidTypes = [<<"string">>, <<"text">>, <<"integer">>, <<"boolean">>, <<"datetime">>, <<"geo">>],
    ValidIndexTypes = [<<"none">>, <<"exact">>, <<"fulltext">>, <<"vector">>, <<"numeric">>, <<"geo">>, <<"datetime">>],
    
    ValidationResults = [
        validate_required_fields(Field, RequiredFieldKeys),
        validate_field_name(Field),
        validate_field_type(Field, ValidTypes),
        validate_index_type(Field, ValidIndexTypes)
    ],
    
    Errors = lists:flatten([Error || {error, Error} <- ValidationResults]),
    case Errors of
        [] -> ok;
        _ -> {error, #{<<"field_", (integer_to_binary(Index))/binary>> => Errors}}
    end;
validate_field_object(_, Index) ->
    {error, #{<<"field_", (integer_to_binary(Index))/binary>> => <<"must be an object">>}}.

validate_field_name(Field) ->
    case maps:get(<<"name">>, Field, undefined) of
        undefined -> ok;
        Name when is_binary(Name), byte_size(Name) > 0 -> ok;
        _ -> {error, #{<<"name">> => <<"must be a non-empty string">>}}
    end.

validate_field_type(Field, ValidTypes) ->
    case maps:get(<<"type">>, Field, undefined) of
        undefined -> ok;
        Type when is_binary(Type) ->
            case lists:member(Type, ValidTypes) of
                true -> ok;
                false -> {error, #{<<"type">> => <<"invalid field type">>}}
            end;
        _ -> {error, #{<<"type">> => <<"must be a string">>}}
    end.

validate_index_type(Field, ValidIndexTypes) ->
    case maps:get(<<"index_type">>, Field, <<"none">>) of
        IndexType when is_binary(IndexType) ->
            case lists:member(IndexType, ValidIndexTypes) of
                true -> ok;
                false -> {error, #{<<"index_type">> => <<"invalid index type">>}}
            end;
        _ -> {error, #{<<"index_type">> => <<"must be a string">>}}
    end.

validate_relationships_array(ModelData) ->
    case maps:get(<<"relationships">>, ModelData, []) of
        Relationships when is_list(Relationships) -> ok;
        _ -> {error, #{<<"relationships">> => <<"must be an array">>}}
    end.

format_error({creation_failed, Reason}) ->
    iolist_to_binary(io_lib:format("~p", [Reason]));
format_error({update_failed, Reason}) ->
    iolist_to_binary(io_lib:format("~p", [Reason]));
format_error(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).

handle_validate_field(Req, State) ->
    try
        {ok, Body, Req2} = cowboy_req:read_body(Req),
        
        case jsx:decode(Body, [return_maps]) of
            FieldData when is_map(FieldData) ->
                case model_definition_manager:validate_field(FieldData) of
                    {ok, ValidationResult} ->
                        Response = jsx:encode(#{
                            <<"success">> => true,
                            <<"validation">> => ValidationResult
                        }),
                        {Response, Req2, State};
                    {error, Reason} ->
                        ?LOG_ERROR("Field validation failed: ~p", [Reason]),
                        ErrorResponse = jsx:encode(#{
                            <<"error">> => <<"validation_failed">>,
                            <<"message">> => <<"Field validation failed">>,
                            <<"details">> => format_error(Reason)
                        }),
                        Req3 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, ErrorResponse, Req2),
                        {stop, Req3, State}
                end;
            _ ->
                ErrorResponse = jsx:encode(#{
                    <<"error">> => <<"invalid_json">>,
                    <<"message">> => <<"Request body must be valid JSON object">>
                }),
                Req3 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, ErrorResponse, Req2),
                {stop, Req3, State}
        end
    catch
        Error4:Reason4 ->
            ?LOG_ERROR("Exception in handle_validate_field: ~p:~p", [Error4, Reason4]),
            ErrorResponse4 = jsx:encode(#{
                <<"error">> => <<"server_error">>,
                <<"message">> => <<"Internal server error">>
            }),
            Req6 = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, ErrorResponse4, Req),
            {stop, Req6, State}
    end.

handle_generate_code(Req, State, ModelId, Language) ->
    try
        case model_definition_manager:generate_code(ModelId, Language) of
            {ok, Code} ->
                ContentType = case Language of
                    <<"python">> -> <<"text/x-python">>;
                    <<"erlang">> -> <<"text/x-erlang">>;
                    _ -> <<"text/plain">>
                end,
                
                Response = jsx:encode(#{
                    <<"success">> => true,
                    <<"code">> => Code,
                    <<"language">> => Language,
                    <<"content_type">> => ContentType
                }),
                {Response, Req, State};
            {error, not_found} ->
                ErrorResponse = jsx:encode(#{
                    <<"error">> => <<"not_found">>,
                    <<"message">> => <<"Model not found">>
                }),
                Req2 = cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>}, ErrorResponse, Req),
                {stop, Req2, State};
            {error, Reason} ->
                ?LOG_ERROR("Code generation failed for model ~p: ~p", [ModelId, Reason]),
                ErrorResponse = jsx:encode(#{
                    <<"error">> => <<"generation_failed">>,
                    <<"message">> => <<"Code generation failed">>,
                    <<"details">> => format_error(Reason)
                }),
                Req2 = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, ErrorResponse, Req),
                {stop, Req2, State}
        end
    catch
        Error5:Reason5 ->
            ?LOG_ERROR("Exception in handle_generate_code: ~p:~p", [Error5, Reason5]),
            ErrorResponse5 = jsx:encode(#{
                <<"error">> => <<"server_error">>,
                <<"message">> => <<"Internal server error">>
            }),
            Req7 = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, ErrorResponse5, Req),
            {stop, Req7, State}
    end.