-module(agent_web_handler).

-export([init/2]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    Headers = cowboy_req:headers(Req0),
    io:format("[WEB] ~s ~s from ~s~n", [Method, Path, maps:get(<<"host">>, Headers, <<"unknown">>)]),
    
    %% Serve the built index.html file
    PrivDir = code:priv_dir(agent_web),
    HtmlPath = filename:join([PrivDir, "static", "dist", "index.html"]),
    io:format("[WEB] Serving index.html from: ~s~n", [HtmlPath]),
    
    case file:read_file(HtmlPath) of
        {ok, Html} ->
            io:format("[WEB] Successfully read index.html (~p bytes)~n", [byte_size(Html)]),
            Req = cowboy_req:reply(200, #{
                <<"content-type">> => <<"text/html">>
            }, Html, Req0),
            io:format("[WEB] Response sent successfully~n"),
            {ok, Req, Opts};
        {error, Reason} ->
            io:format("[ERROR] Failed to read index.html: ~p~n", [Reason]),
            Req = cowboy_req:reply(500, #{
                <<"content-type">> => <<"text/plain">>
            }, <<"Internal Server Error: Could not load index.html">>, Req0),
            {ok, Req, Opts}
    end.