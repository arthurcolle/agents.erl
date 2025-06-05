-module(test_status_handler).
-export([init/2]).

init(Req0, State) ->
    Response = jsx:encode(#{
        status => <<"ok">>,
        message => <<"Test MCP server is running">>,
        timestamp => erlang:system_time(second)
    }),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Response,
        Req0),
    {ok, Req, State}.