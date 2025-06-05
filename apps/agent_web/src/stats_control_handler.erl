-module(stats_control_handler).
-export([init/2]).

init(Req0 = #{method := <<"GET">>}, State) ->
    Path = cowboy_req:path(Req0),
    case Path of
        <<"/api/stats">> ->
            % Get current stats
            Stats = conversation_stats_logger:get_stats(),
            Response = jsx:encode(Stats),
            Req = cowboy_req:reply(200, #{
                <<"content-type">> => <<"application/json">>,
                <<"access-control-allow-origin">> => <<"*">>
            }, Response, Req0),
            {ok, Req, State};
        _ ->
            Req = cowboy_req:reply(404, #{}, <<>>, Req0),
            {ok, Req, State}
    end;

init(Req0 = #{method := <<"POST">>}, State) ->
    Path = cowboy_req:path(Req0),
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    
    case Path of
        <<"/api/stats/reset">> ->
            conversation_stats_logger:reset_stats(),
            Response = jsx:encode(#{status => <<"reset">>}),
            Req = cowboy_req:reply(200, #{
                <<"content-type">> => <<"application/json">>,
                <<"access-control-allow-origin">> => <<"*">>
            }, Response, Req1),
            {ok, Req, State};
            
        <<"/api/stats/display">> ->
            case jsx:decode(Body, [return_maps]) of
                #{<<"enabled">> := true} ->
                    conversation_stats_logger:enable_periodic_display(),
                    Response = jsx:encode(#{status => <<"enabled">>});
                #{<<"enabled">> := false} ->
                    conversation_stats_logger:disable_periodic_display(),
                    Response = jsx:encode(#{status => <<"disabled">>});
                _ ->
                    Response = jsx:encode(#{error => <<"Invalid request">>})
            end,
            Req = cowboy_req:reply(200, #{
                <<"content-type">> => <<"application/json">>,
                <<"access-control-allow-origin">> => <<"*">>
            }, Response, Req1),
            {ok, Req, State};
            
        _ ->
            Req = cowboy_req:reply(404, #{}, <<>>, Req1),
            {ok, Req, State}
    end;

init(Req0, State) ->
    Req = cowboy_req:reply(405, #{}, <<>>, Req0),
    {ok, Req, State}.