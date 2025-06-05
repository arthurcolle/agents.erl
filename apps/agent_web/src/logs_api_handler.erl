-module(logs_api_handler).

-export([init/2, allowed_methods/2, content_types_accepted/2]).
-export([handle_post/2, resource_exists/2, allow_missing_post/2]).

-include_lib("kernel/include/logger.hrl").

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>, <<"OPTIONS">>], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {<<"application/json">>, handle_post}
    ], Req, State}.

resource_exists(Req, State) ->
    {true, Req, State}.

allow_missing_post(Req, State) ->
    {true, Req, State}.

handle_post(Req, State) ->
    try
        {ok, Body, Req1} = cowboy_req:read_body(Req),
        Path = cowboy_req:path_info(Req),
        
        case jsx:decode(Body, [return_maps]) of
            LogData when is_map(LogData) ->
                case Path of
                    [<<"interactions">>] ->
                        handle_interaction_log(LogData, Req1, State);
                    [<<"errors">>] ->
                        handle_error_log(LogData, Req1, State);
                    _ ->
                        reply_error(400, <<"invalid_path">>, <<"Invalid logging endpoint">>, Req1, State)
                end;
            _ ->
                reply_error(400, <<"invalid_json">>, <<"Request body must be valid JSON">>, Req1, State)
        end
    catch
        Error:Reason ->
            ?LOG_ERROR("Exception in logs handler: ~p:~p", [Error, Reason]),
            reply_error(500, <<"server_error">>, <<"Internal server error">>, Req, State)
    end.

reply_error(Code, ErrorType, Message, Req, State) ->
    ErrorResp = jsx:encode(#{
        <<"error">> => ErrorType,
        <<"message">> => Message
    }),
    ReplyReq = cowboy_req:reply(Code, #{<<"content-type">> => <<"application/json">>}, ErrorResp, Req),
    {stop, ReplyReq, State}.

handle_interaction_log(LogData, Req, State) ->
    try
        %% Extract relevant fields
        Action = maps:get(<<"action">>, LogData, <<"unknown">>),
        Data = maps:get(<<"data">>, LogData, #{}),
        
        %% Log the interaction
        interaction_logger:log_button_click(Action, Data),
        
        %% Also log directly to console for immediate visibility
        ButtonText = maps:get(<<"buttonText">>, Data, <<"unknown">>),
        Timestamp = maps:get(<<"timestamp">>, LogData, <<"unknown">>),
        
        %% Direct console log for scripts/start_simple.sh visibility
        io:format("🖱️  BUTTON: ~s | ACTION: ~s | TIME: ~s~n", [ButtonText, Action, Timestamp]),
        
        Response = jsx:encode(#{
            <<"success">> => true,
            <<"message">> => <<"Interaction logged successfully">>
        }),
        
        Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req),
        {stop, Req1, State}
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to log interaction: ~p:~p", [Error, Reason]),
            reply_error(500, <<"logging_failed">>, <<"Failed to log interaction">>, Req, State)
    end.

handle_error_log(LogData, Req, State) ->
    try
        %% Extract error details
        Message = maps:get(<<"message">>, LogData, <<"unknown error">>),
        Severity = maps:get(<<"severity">>, LogData, <<"medium">>),
        Context = maps:get(<<"context">>, LogData, <<"unknown">>),
        Timestamp = maps:get(<<"timestamp">>, LogData, <<"unknown">>),
        
        %% Log the error
        interaction_logger:log_error(LogData, Context),
        
        %% Direct console log in RED for scripts/start_simple.sh visibility
        io:format("🔴 ERROR: ~s | SEVERITY: ~s | CONTEXT: ~s | TIME: ~s~n", 
                  [Message, Severity, Context, Timestamp]),
        
        Response = jsx:encode(#{
            <<"success">> => true,
            <<"message">> => <<"Error logged successfully">>
        }),
        
        Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req),
        {stop, Req1, State}
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to log error: ~p:~p", [Error, Reason]),
            reply_error(500, <<"logging_failed">>, <<"Failed to log error">>, Req, State)
    end.