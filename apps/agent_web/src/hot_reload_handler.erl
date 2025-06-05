%% hot_reload_handler.erl
%% HTTP API handler for hot code reloading
-module(hot_reload_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    handle_request(Method, Req0, State).

handle_request(<<"GET">>, Req0, State) ->
    % Get list of loaded modules
    case hot_code_reloader:get_loaded_modules() of
        Result when is_map(Result) ->
            ResponseBody = jsx:encode(Result),
            Req = cowboy_req:reply(200, 
                #{<<"content-type">> => <<"application/json">>}, 
                ResponseBody, Req0),
            {ok, Req, State};
        {error, Reason} ->
            ErrorBody = jsx:encode(#{error => Reason}),
            Req = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                ErrorBody, Req0),
            {ok, Req, State}
    end;

handle_request(<<"POST">>, Req0, State) ->
    % Handle different reload operations
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    
    case jsx:decode(Body, [return_maps]) of
        #{<<"action">> := Action} = Params ->
            Result = handle_reload_action(Action, Params),
            ResponseBody = jsx:encode(Result),
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                ResponseBody, Req1),
            {ok, Req, State};
        _ ->
            ErrorBody = jsx:encode(#{error => <<"Invalid request format">>}),
            Req = cowboy_req:reply(400,
                #{<<"content-type">> => <<"application/json">>},
                ErrorBody, Req1),
            {ok, Req, State}
    end;

handle_request(_, Req0, State) ->
    Req = cowboy_req:reply(405, 
        #{<<"allow">> => <<"GET, POST">>}, 
        <<"Method not allowed">>, Req0),
    {ok, Req, State}.

handle_reload_action(<<"reload_module">>, #{<<"module">> := ModuleBin}) ->
    Module = binary_to_atom(ModuleBin, utf8),
    case hot_code_reloader:reload_module(Module) of
        {ok, Result} -> Result;
        {error, Reason} -> #{error => Reason}
    end;

handle_reload_action(<<"reload_all">>, _Params) ->
    case hot_code_reloader:reload_all_modules() of
        Result when is_map(Result) -> Result;
        {error, Reason} -> #{error => Reason}
    end;

handle_reload_action(<<"compile_and_reload">>, #{<<"source_file">> := SourceFile}) ->
    case hot_code_reloader:compile_and_reload(binary_to_list(SourceFile)) of
        {ok, Result} -> Result;
        {error, Reason} -> #{error => Reason}
    end;

handle_reload_action(<<"watch_files">>, #{<<"files">> := Files}) ->
    FileList = [binary_to_list(F) || F <- Files],
    case hot_code_reloader:watch_files(FileList) of
        ok -> #{status => <<"watching">>, files => Files};
        {error, Reason} -> #{error => Reason}
    end;

handle_reload_action(<<"stop_watch">>, _Params) ->
    case hot_code_reloader:stop_watch() of
        ok -> #{status => <<"stopped">>};
        {error, Reason} -> #{error => Reason}
    end;

handle_reload_action(<<"get_module_info">>, #{<<"module">> := ModuleBin}) ->
    Module = binary_to_atom(ModuleBin, utf8),
    case hot_code_reloader:get_module_info(Module) of
        {ok, Result} -> Result;
        {error, Reason} -> #{error => Reason}
    end;

handle_reload_action(Action, _Params) ->
    #{error => <<"Unknown action">>, action => Action}.