-module(agent_templates_handler).

-export([init/2]).

init(Req0 = #{method := <<"GET">>}, State) ->
    % List all available templates
    Templates = agent_templates:list_templates(),
    Response = jsx:encode(#{templates => Templates}),
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, Response, Req0),
    {ok, Req, State};

init(Req0 = #{method := <<"POST">>}, State) ->
    % Register a custom template
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    case jsx:decode(Body, [return_maps]) of
        #{<<"id">> := TemplateId, <<"template">> := Template} ->
            case agent_templates:register_custom_template(TemplateId, Template) of
                ok ->
                    Req = cowboy_req:reply(201, #{
                        <<"content-type">> => <<"application/json">>
                    }, jsx:encode(#{status => <<"Template registered">>}), Req1);
                {error, Reason} ->
                    Req = cowboy_req:reply(400, #{
                        <<"content-type">> => <<"application/json">>
                    }, jsx:encode(#{error => Reason}), Req1)
            end;
        _ ->
            Req = cowboy_req:reply(400, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{error => <<"Invalid request body">>}), Req1)
    end,
    {ok, Req, State};

init(Req0, State) ->
    Req = cowboy_req:reply(405, #{}, <<>>, Req0),
    {ok, Req, State}.