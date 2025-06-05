-module(error_api_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path_info(Req0),
    
    Req = case {Method, Path} of
        {<<"GET">>, undefined} ->
            handle_get_errors(Req0);
        {<<"GET">>, [<<"summary">>]} ->
            handle_get_summary(Req0);
        {<<"GET">>, [<<"trends">>]} ->
            handle_get_trends(Req0);
        {<<"POST">>, [<<"clear">>]} ->
            handle_clear_errors(Req0);
        _ ->
            cowboy_req:reply(404, #{}, <<"Not Found">>, Req0)
    end,
    
    {ok, Req, State}.

handle_get_errors(Req) ->
    Limit = case cowboy_req:parse_qs(Req) of
        [{<<"limit">>, LimitBin}] ->
            try binary_to_integer(LimitBin) of
                L when L > 0 -> L;
                _ -> 100
            catch
                _:_ -> 100
            end;
        _ -> 100
    end,
    
    Errors = error_tracking_system:get_errors(Limit),
    
    cowboy_req:reply(200, 
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Errors),
        Req).

handle_get_summary(Req) ->
    Summary = error_tracking_system:get_error_summary(),
    
    cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Summary),
        Req).

handle_get_trends(Req) ->
    Trends = error_tracking_system:get_error_trends(),
    
    cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Trends),
        Req).

handle_clear_errors(Req) ->
    ok = error_tracking_system:clear_errors(),
    
    cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(#{status => <<"ok">>}),
        Req).