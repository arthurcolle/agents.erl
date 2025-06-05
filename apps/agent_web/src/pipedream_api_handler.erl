-module(pipedream_api_handler).

-export([init/2, allowed_methods/2, content_types_provided/2, content_types_accepted/2,
         handle_get/2, handle_post/2, handle_delete/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle_get}], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, handle_post}], Req, State}.

handle_get(Req, State) ->
    Path = cowboy_req:path_info(Req),
    UserId = cowboy_req:header(<<"x-user-id">>, Req),
    
    Response = case {Path, UserId} of
        {[<<"apps">>], _} ->
            handle_get_apps(Req);
        {[<<"apps">>, AppSlug], _} ->
            handle_get_app_details(AppSlug, Req);
        {[<<"apps">>, AppSlug, <<"tools">>], _} ->
            handle_get_app_tools(AppSlug, Req);
        {[<<"discover">>], _} ->
            handle_get_discovery_stats(Req);
        {[<<"connections">>], UserId} when UserId =/= undefined ->
            handle_get_user_connections(binary_to_list(UserId), Req);
        {[<<"connections">>, AppSlug], UserId} when UserId =/= undefined ->
            handle_get_connection_status(binary_to_list(UserId), binary_to_list(AppSlug), Req);
        {[<<"tools">>], UserId} when UserId =/= undefined ->
            handle_get_user_tools(binary_to_list(UserId), Req);
        {[<<"stats">>], _} ->
            handle_get_stats(Req);
        _ ->
            {error, not_found}
    end,
    
    case Response of
        {ok, Data} ->
            Body = jsx:encode(#{success => true, data => Data}),
            {Body, Req, State};
        {error, Reason} ->
            StatusCode = case Reason of
                not_found -> 404;
                unauthorized -> 401;
                bad_request -> 400;
                _ -> 500
            end,
            ErrorBody = jsx:encode(#{success => false, error => Reason}),
            Req2 = cowboy_req:reply(StatusCode, #{}, ErrorBody, Req),
            {stop, Req2, State}
    end.

handle_post(Req, State) ->
    Path = cowboy_req:path_info(Req),
    UserId = cowboy_req:header(<<"x-user-id">>, Req),
    
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    RequestData = jsx:decode(Body, [return_maps]),
    
    Response = case {Path, UserId} of
        {[<<"connect">>], UserId} when UserId =/= undefined ->
            handle_connect_app(binary_to_list(UserId), RequestData, Req2);
        {[<<"disconnect">>], UserId} when UserId =/= undefined ->
            handle_disconnect_app(binary_to_list(UserId), RequestData, Req2);
        {[<<"refresh">>], UserId} when UserId =/= undefined ->
            handle_refresh_connections(binary_to_list(UserId), Req2);
        {[<<"tools">>, <<"call">>], UserId} when UserId =/= undefined ->
            handle_tool_call(binary_to_list(UserId), RequestData, Req2);
        {[<<"oauth">>, <<"callback">>], _} ->
            handle_oauth_callback(RequestData, Req2);
        {[<<"discovery">>, <<"refresh">>], _} ->
            handle_refresh_discovery(Req2);
        {[<<"user">>, <<"apps">>, <<"register">>], UserId} when UserId =/= undefined ->
            handle_register_user_apps(binary_to_list(UserId), RequestData, Req2);
        _ ->
            {error, not_found}
    end,
    
    case Response of
        {ok, Data} ->
            ResponseBody = jsx:encode(#{success => true, data => Data}),
            Req3 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, ResponseBody, Req2),
            {stop, Req3, State};
        {error, Reason} ->
            StatusCode = case Reason of
                not_found -> 404;
                unauthorized -> 401;
                bad_request -> 400;
                _ -> 500
            end,
            ErrorBody = jsx:encode(#{success => false, error => Reason}),
            Req3 = cowboy_req:reply(StatusCode, #{<<"content-type">> => <<"application/json">>}, ErrorBody, Req2),
            {stop, Req3, State}
    end.

handle_delete(Req, State) ->
    Path = cowboy_req:path_info(Req),
    UserId = cowboy_req:header(<<"x-user-id">>, Req),
    
    Response = case {Path, UserId} of
        {[<<"connections">>, AppSlug], UserId} when UserId =/= undefined ->
            handle_disconnect_app(binary_to_list(UserId), 
                                  #{<<"app_slug">> => AppSlug}, Req);
        _ ->
            {error, not_found}
    end,
    
    case Response of
        {ok, Data} ->
            ResponseBody = jsx:encode(#{success => true, data => Data}),
            Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, ResponseBody, Req),
            {stop, Req2, State};
        {error, Reason} ->
            StatusCode = case Reason of
                not_found -> 404;
                unauthorized -> 401;
                bad_request -> 400;
                _ -> 500
            end,
            ErrorBody = jsx:encode(#{success => false, error => Reason}),
            Req2 = cowboy_req:reply(StatusCode, #{<<"content-type">> => <<"application/json">>}, ErrorBody, Req),
            {stop, Req2, State}
    end.

%% GET Handlers

handle_get_apps(_Req) ->
    case pipedream_autodiscovery:get_discovered_apps() of
        {ok, Apps} ->
            FormattedApps = lists:map(fun({Slug, App}) ->
                maps:put(<<"slug">>, Slug, App)
            end, Apps),
            {ok, FormattedApps};
        Error ->
            Error
    end.

handle_get_app_details(AppSlug, _Req) ->
    case pipedream_autodiscovery:get_app_by_slug(AppSlug) of
        {ok, App} ->
            {ok, App};
        {error, not_found} ->
            {error, not_found}
    end.

handle_get_app_tools(AppSlug, _Req) ->
    case pipedream_autodiscovery:get_app_tools(AppSlug) of
        {ok, Tools} ->
            {ok, Tools};
        Error ->
            Error
    end.

handle_get_discovery_stats(_Req) ->
    case pipedream_autodiscovery:get_discovery_stats() of
        {ok, Stats} ->
            {ok, Stats};
        Error ->
            Error
    end.

handle_get_user_connections(UserId, _Req) ->
    case pipedream_auth_handler:get_user_connections(UserId) of
        {ok, Connections} ->
            {ok, Connections};
        Error ->
            Error
    end.

handle_get_connection_status(UserId, AppSlug, _Req) ->
    case pipedream_auth_handler:verify_connection(UserId, AppSlug) of
        {ok, Status} ->
            {ok, #{status => Status}};
        Error ->
            Error
    end.

handle_get_user_tools(UserId, _Req) ->
    case pipedream_autodiscovery:get_user_tools(UserId) of
        {ok, Tools} ->
            {ok, Tools};
        Error ->
            Error
    end.

handle_get_stats(_Req) ->
    case pipedream_auth_handler:get_connection_stats() of
        {ok, AuthStats} ->
            case pipedream_autodiscovery:get_discovery_stats() of
                {ok, DiscoveryStats} ->
                    CombinedStats = #{
                        auth => AuthStats,
                        discovery => DiscoveryStats,
                        timestamp => erlang:system_time(second)
                    },
                    {ok, CombinedStats};
                {error, _} ->
                    {ok, #{auth => AuthStats, discovery => #{}, timestamp => erlang:system_time(second)}}
            end;
        Error ->
            Error
    end.

%% POST Handlers

handle_connect_app(UserId, #{<<"app_slug">> := AppSlug}, _Req) ->
    AppSlugStr = binary_to_list(AppSlug),
    case pipedream_auth_handler:generate_connection_url(UserId, AppSlugStr) of
        {ok, URL} ->
            {ok, #{connection_url => list_to_binary(URL)}};
        Error ->
            Error
    end;
handle_connect_app(_, _, _) ->
    {error, bad_request}.

handle_disconnect_app(UserId, #{<<"app_slug">> := AppSlug}, _Req) ->
    AppSlugStr = binary_to_list(AppSlug),
    case pipedream_auth_handler:disconnect_user_app(UserId, AppSlugStr) of
        ok ->
            {ok, #{status => <<"disconnected">>}};
        Error ->
            Error
    end;
handle_disconnect_app(_, _, _) ->
    {error, bad_request}.

handle_refresh_connections(UserId, _Req) ->
    case pipedream_auth_handler:refresh_user_connections(UserId) of
        {ok, Results} ->
            FormattedResults = lists:map(fun({AppSlug, Status}) ->
                #{app_slug => list_to_binary(AppSlug), status => Status}
            end, Results),
            {ok, FormattedResults};
        Error ->
            Error
    end.

handle_tool_call(UserId, RequestData, _Req) ->
    case RequestData of
        #{<<"app_slug">> := AppSlug, <<"tool_name">> := ToolName, <<"arguments">> := Arguments} ->
            AppSlugStr = binary_to_list(AppSlug),
            ToolNameStr = binary_to_list(ToolName),
            
            case pipedream_mcp_client:call_tool(UserId, AppSlugStr, ToolNameStr, Arguments) of
                {ok, Result} ->
                    {ok, #{result => Result}};
                Error ->
                    Error
            end;
        _ ->
            {error, bad_request}
    end.

handle_oauth_callback(RequestData, _Req) ->
    case RequestData of
        #{<<"auth_token">> := AuthToken, <<"code">> := Code} ->
            OAuthState = maps:get(<<"state">>, RequestData, undefined),
            AuthTokenStr = binary_to_list(AuthToken),
            CodeStr = binary_to_list(Code),
            
            case pipedream_auth_handler:handle_oauth_callback(AuthTokenStr, CodeStr, OAuthState) of
                {ok, Status} ->
                    {ok, #{status => Status}};
                Error ->
                    Error
            end;
        _ ->
            {error, bad_request}
    end.

handle_refresh_discovery(_Req) ->
    pipedream_autodiscovery:refresh_discovery(),
    {ok, #{status => <<"refresh_initiated">>}}.

handle_register_user_apps(UserId, #{<<"app_slugs">> := AppSlugs}, _Req) ->
    AppSlugStrs = [binary_to_list(Slug) || Slug <- AppSlugs],
    case pipedream_autodiscovery:register_user_apps(UserId, AppSlugStrs) of
        ok ->
            {ok, #{status => <<"registered">>, app_count => length(AppSlugStrs)}};
        Error ->
            Error
    end;
handle_register_user_apps(_, _, _) ->
    {error, bad_request}.

%% Utility functions

validate_user_id(undefined) ->
    {error, unauthorized};
validate_user_id(UserId) when is_binary(UserId), byte_size(UserId) > 0 ->
    {ok, binary_to_list(UserId)};
validate_user_id(_) ->
    {error, bad_request}.

format_error(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8);
format_error(Reason) when is_list(Reason) ->
    list_to_binary(Reason);
format_error(Reason) when is_binary(Reason) ->
    Reason;
format_error({error, Details}) ->
    format_error(Details);
format_error(Reason) ->
    list_to_binary(io_lib:format("~p", [Reason])).