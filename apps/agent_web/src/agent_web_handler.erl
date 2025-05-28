-module(agent_web_handler).

-export([init/2]).

init(Req0, Opts) ->
    %% Serve the built index.html file
    PrivDir = code:priv_dir(agent_web),
    HtmlPath = filename:join([PrivDir, "static", "dist", "index.html"]),
    {ok, Html} = file:read_file(HtmlPath),
    
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/html">>
    }, Html, Req0),
    
    {ok, Req, Opts}.