%%%-------------------------------------------------------------------
%%% @doc
%%% MCP OAuth HTTP Handler
%%% Handles OAuth routes for MCP server authentication
%%% @end
%%%-------------------------------------------------------------------
-module(mcp_oauth_handler).

-export([init/2]).

%%%===================================================================
%%% Cowboy Handler Implementation
%%%===================================================================

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    PathInfo = cowboy_req:path_info(Req0),
    handle_request(Method, PathInfo, Req0, State).

%% GET /api/mcp/oauth/start/{server_id}
handle_request(<<"GET">>, [<<"start">>, ServerId], Req0, State) ->
    UserId = get_user_id(Req0),
    case mcp_oauth_integration:start_oauth_flow(ServerId, UserId) of
        {ok, OAuthData} ->
            ResponseBody = jsx:encode(#{
                success => true,
                auth_url => maps:get(auth_url, OAuthData),
                popup_required => maps:get(popup_required, OAuthData),
                redirect_uri => maps:get(redirect_uri, OAuthData)
            }),
            Req1 = cowboy_req:reply(200, 
                #{<<"content-type">> => <<"application/json">>}, 
                ResponseBody, Req0),
            {ok, Req1, State};
        {error, Reason} ->
            ResponseBody = jsx:encode(#{
                success => false,
                error => format_error(Reason)
            }),
            Req1 = cowboy_req:reply(400, 
                #{<<"content-type">> => <<"application/json">>}, 
                ResponseBody, Req0),
            {ok, Req1, State}
    end;

%% GET /api/mcp/oauth/callback/{server_id}
handle_request(<<"GET">>, [<<"callback">>, ServerId], Req0, State) ->
    QsVals = cowboy_req:parse_qs(Req0),
    case proplists:get_value(<<"code">>, QsVals) of
        undefined ->
            % Check for error parameter
            Error = proplists:get_value(<<"error">>, QsVals, <<"unknown_error">>),
            ErrorDesc = proplists:get_value(<<"error_description">>, QsVals, <<"OAuth authorization failed">>),
            {Status, Headers, Body} = mcp_oauth_integration:generate_popup_error_response(ErrorDesc),
            Req1 = cowboy_req:reply(Status, Headers, Body, Req0),
            {ok, Req1, State};
        AuthCode ->
            % Handle successful OAuth callback
            {Status, Headers, Body} = mcp_oauth_integration:handle_oauth_popup_callback(ServerId, AuthCode, State),
            Req1 = cowboy_req:reply(Status, Headers, Body, Req0),
            {ok, Req1, State}
    end;

%% GET /api/mcp/oauth/status/{server_id}
handle_request(<<"GET">>, [<<"status">>, ServerId], Req0, State) ->
    IsRequired = mcp_oauth_integration:is_oauth_required(ServerId),
    case oauth_handler:get_token(ServerId, <<"mcp_user">>) of
        {ok, TokenData} ->
            ResponseBody = jsx:encode(#{
                oauth_required => IsRequired,
                authenticated => true,
                server_id => ServerId,
                expires_at => format_datetime(maps:get(expires_at, TokenData))
            }),
            Req1 = cowboy_req:reply(200, 
                #{<<"content-type">> => <<"application/json">>}, 
                ResponseBody, Req0),
            {ok, Req1, State};
        {error, _} ->
            ResponseBody = jsx:encode(#{
                oauth_required => IsRequired,
                authenticated => false,
                server_id => ServerId
            }),
            Req1 = cowboy_req:reply(200, 
                #{<<"content-type">> => <<"application/json">>}, 
                ResponseBody, Req0),
            {ok, Req1, State}
    end;

%% POST /api/mcp/oauth/connect/{server_id}
handle_request(<<"POST">>, [<<"connect">>, ServerId], Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    case jsx:decode(Body, [return_maps]) of
        #{<<"auth_code">> := AuthCode} ->
            case mcp_oauth_integration:connect_with_oauth(ServerId, AuthCode) of
                {ok, Result} ->
                    ResponseBody = jsx:encode(#{
                        success => true,
                        result => Result
                    }),
                    Req2 = cowboy_req:reply(200, 
                        #{<<"content-type">> => <<"application/json">>}, 
                        ResponseBody, Req1),
                    {ok, Req2, State};
                {error, Reason} ->
                    ResponseBody = jsx:encode(#{
                        success => false,
                        error => format_error(Reason)
                    }),
                    Req2 = cowboy_req:reply(400, 
                        #{<<"content-type">> => <<"application/json">>}, 
                        ResponseBody, Req1),
                    {ok, Req2, State}
            end;
        _ ->
            ResponseBody = jsx:encode(#{
                success => false,
                error => <<"Missing auth_code parameter">>
            }),
            Req2 = cowboy_req:reply(400, 
                #{<<"content-type">> => <<"application/json">>}, 
                ResponseBody, Req1),
            {ok, Req2, State}
    end;

%% POST /api/mcp/oauth/refresh/{server_id}
handle_request(<<"POST">>, [<<"refresh">>, ServerId], Req0, State) ->
    case oauth_handler:get_token(ServerId, <<"mcp_user">>) of
        {ok, TokenData} ->
            case maps:get(refresh_token, TokenData, undefined) of
                undefined ->
                    ResponseBody = jsx:encode(#{
                        success => false,
                        error => <<"No refresh token available">>
                    }),
                    Req1 = cowboy_req:reply(400, 
                        #{<<"content-type">> => <<"application/json">>}, 
                        ResponseBody, Req0),
                    {ok, Req1, State};
                RefreshToken ->
                    case mcp_oauth_integration:refresh_oauth_token(ServerId, RefreshToken) of
                        {ok, NewTokenData} ->
                            % Store new token
                            oauth_handler:store_token(ServerId, <<"mcp_user">>, NewTokenData),
                            ResponseBody = jsx:encode(#{
                                success => true,
                                expires_at => format_datetime(maps:get(expires_at, NewTokenData))
                            }),
                            Req1 = cowboy_req:reply(200, 
                                #{<<"content-type">> => <<"application/json">>}, 
                                ResponseBody, Req0),
                            {ok, Req1, State};
                        {error, Reason} ->
                            ResponseBody = jsx:encode(#{
                                success => false,
                                error => format_error(Reason)
                            }),
                            Req1 = cowboy_req:reply(400, 
                                #{<<"content-type">> => <<"application/json">>}, 
                                ResponseBody, Req0),
                            {ok, Req1, State}
                    end
            end;
        {error, _} ->
            ResponseBody = jsx:encode(#{
                success => false,
                error => <<"No token found">>
            }),
            Req1 = cowboy_req:reply(404, 
                #{<<"content-type">> => <<"application/json">>}, 
                ResponseBody, Req0),
            {ok, Req1, State}
    end;

%% DELETE /api/mcp/oauth/revoke/{server_id}
handle_request(<<"DELETE">>, [<<"revoke">>, ServerId], Req0, State) ->
    case oauth_handler:revoke_token(ServerId, <<"mcp_user">>) of
        ok ->
            ResponseBody = jsx:encode(#{
                success => true,
                message => <<"OAuth token revoked successfully">>
            }),
            Req1 = cowboy_req:reply(200, 
                #{<<"content-type">> => <<"application/json">>}, 
                ResponseBody, Req0),
            {ok, Req1, State};
        {error, Reason} ->
            ResponseBody = jsx:encode(#{
                success => false,
                error => format_error(Reason)
            }),
            Req1 = cowboy_req:reply(400, 
                #{<<"content-type">> => <<"application/json">>}, 
                ResponseBody, Req0),
            {ok, Req1, State}
    end;

%% Handle unknown routes
handle_request(_, _, Req0, State) ->
    ResponseBody = jsx:encode(#{
        success => false,
        error => <<"Not found">>
    }),
    Req1 = cowboy_req:reply(404, 
        #{<<"content-type">> => <<"application/json">>}, 
        ResponseBody, Req0),
    {ok, Req1, State}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% Get user ID from request (simplified for demo)
get_user_id(_Req) ->
    <<"default_user">>.

%% Format error for JSON response
format_error({unsupported_server, ServerId}) ->
    iolist_to_binary(io_lib:format("Unsupported server: ~s", [ServerId]));
format_error({token_exchange_failed, Reason}) ->
    iolist_to_binary(io_lib:format("Token exchange failed: ~p", [Reason]));
format_error({connection_failed, Reason}) ->
    iolist_to_binary(io_lib:format("Connection failed: ~p", [Reason]));
format_error(Reason) when is_binary(Reason) ->
    Reason;
format_error(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).

%% Format datetime for JSON
format_datetime(DateTime) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = DateTime,
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", 
                                   [Year, Month, Day, Hour, Min, Sec])).