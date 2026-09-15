%% hot_reload_handler.erl
%% HTTP API handler for hot code reloading
-module(hot_reload_handler).

-export([init/2]).

init(Req0, State) ->
    case application:get_env(agent_web, hot_reload_enabled, false) of
        true ->
            Method = cowboy_req:method(Req0),
            handle_request(Method, Req0, State);
        _ ->
            Req = cowboy_req:reply(404,
                #{},
                <<"Not Found">>,
                Req0),
            {ok, Req, State}
    end.

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
    case loaded_module(ModuleBin) of
        {ok, Module} ->
            case hot_code_reloader:reload_module(Module) of
                {ok, Result} -> Result;
                {error, Reason} -> #{error => Reason}
            end;
        error ->
            #{error => <<"Module must name an already loaded module">>}
    end;

handle_reload_action(<<"reload_all">>, _Params) ->
    case hot_code_reloader:reload_all_modules() of
        Result when is_map(Result) -> Result;
        {error, Reason} -> #{error => Reason}
    end;

handle_reload_action(<<"compile_and_reload">>, #{<<"source_file">> := SourceFile}) ->
    case safe_source_file(SourceFile) of
        {ok, SourcePath} ->
            case hot_code_reloader:compile_and_reload(SourcePath) of
                {ok, Result} -> Result;
                {error, Reason} -> #{error => Reason}
            end;
        error ->
            #{error => <<"source_file must be an existing Erlang source file">>}
    end;

handle_reload_action(<<"watch_files">>, #{<<"files">> := Files}) ->
    case safe_source_files(Files) of
        {ok, FileList} ->
            case hot_code_reloader:watch_files(FileList) of
                ok -> #{status => <<"watching">>, files => Files};
                {error, Reason} -> #{error => Reason}
            end;
        error ->
            #{error => <<"files must be a list of existing Erlang source files">>}
    end;

handle_reload_action(<<"stop_watch">>, _Params) ->
    case hot_code_reloader:stop_watch() of
        ok -> #{status => <<"stopped">>};
        {error, Reason} -> #{error => Reason}
    end;

handle_reload_action(<<"get_module_info">>, #{<<"module">> := ModuleBin}) ->
    case loaded_module(ModuleBin) of
        {ok, Module} ->
            case hot_code_reloader:get_module_info(Module) of
                {ok, Result} -> Result;
                {error, Reason} -> #{error => Reason}
            end;
        error ->
            #{error => <<"Module must name an already loaded module">>}
    end;

handle_reload_action(Action, _Params) ->
    #{error => <<"Unknown action">>, action => Action}.

loaded_module(ModuleBin) when is_binary(ModuleBin), byte_size(ModuleBin) > 0,
                              byte_size(ModuleBin) =< 255 ->
    try
        Module = binary_to_existing_atom(ModuleBin, utf8),
        case code:is_loaded(Module) of
            {file, _} -> {ok, Module};
            _ -> error
        end
    catch
        error:badarg -> error
    end;
loaded_module(_) -> error.

safe_source_file(SourceFile) when is_binary(SourceFile), byte_size(SourceFile) > 0,
                                  byte_size(SourceFile) =< 4096 ->
    Path = binary_to_list(SourceFile),
    case filename:extension(Path) =:= ".erl" andalso filelib:is_regular(Path) of
        true -> {ok, Path};
        false -> error
    end;
safe_source_file(_) -> error.

safe_source_files(Files) when is_list(Files), length(Files) =< 32 ->
    case lists:foldl(
        fun(File, {ok, Acc}) ->
            case safe_source_file(File) of
                {ok, Path} -> {ok, [Path | Acc]};
                error -> error
            end;
           (_File, error) -> error
        end,
        {ok, []},
        Files
    ) of
        {ok, Paths} -> {ok, lists:reverse(Paths)};
        error -> error
    end;
safe_source_files(_) -> error.
