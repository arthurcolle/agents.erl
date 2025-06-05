%% openai_files.erl
%% OpenAI Files API client for handling file uploads and inputs
-module(openai_files).
-behaviour(gen_server).

%% API exports
-export([
    start_link/0,
    upload_file/2,
    upload_file/3,
    delete_file/1,
    get_file/1,
    list_files/0,
    list_files/1,
    create_image_input/1,
    create_image_input/2,
    validate_file_input/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).
-define(DEFAULT_TIMEOUT, 60000).

%% Logging macros
-define(LOG_INFO(Msg), colored_logger:data(processed, Msg)).
-define(LOG_INFO(Msg, Args), colored_logger:data(processed, io_lib:format(Msg, Args))).
-define(LOG_ERROR(Msg), colored_logger:fire(inferno, Msg)).
-define(LOG_ERROR(Msg, Args), colored_logger:fire(inferno, io_lib:format(Msg, Args))).
-define(LOG_SUCCESS(Msg), colored_logger:complete(success, Msg)).
-define(LOG_SUCCESS(Msg, Args), colored_logger:complete(success, io_lib:format(Msg, Args))).
-define(LOG_WARNING(Msg), colored_logger:alarm(medium, Msg)).
-define(LOG_WARNING(Msg, Args), colored_logger:alarm(medium, io_lib:format(Msg, Args))).

%% Supported image types
-define(SUPPORTED_IMAGE_TYPES, [
    <<"image/png">>,
    <<"image/jpeg">>,
    <<"image/jpg">>,
    <<"image/gif">>,
    <<"image/webp">>
]).

-record(state, {
    api_key = undefined :: binary() | undefined,
    base_url = <<"https://api.openai.com/v1">> :: binary(),
    uploaded_files = #{} :: map()  % Track uploaded files locally
}).

%% Public API

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Upload a file
upload_file(FilePath, Purpose) ->
    upload_file(FilePath, Purpose, #{}).

upload_file(FilePath, Purpose, Options) ->
    ?LOG_INFO("[FILES] 📁 Uploading file: ~s (purpose: ~s)", [FilePath, Purpose]),
    
    Timeout = maps:get(timeout, Options, ?DEFAULT_TIMEOUT),
    gen_server:call(?SERVER, {upload_file, FilePath, Purpose, Options}, Timeout).

%% Delete a file
delete_file(FileId) ->
    ?LOG_INFO("[FILES] 🗑️  Deleting file: ~s", [FileId]),
    gen_server:call(?SERVER, {delete_file, FileId}).

%% Get file information
get_file(FileId) ->
    gen_server:call(?SERVER, {get_file, FileId}).

%% List files
list_files() ->
    list_files(#{}).

list_files(Options) ->
    gen_server:call(?SERVER, {list_files, Options}).

%% Create image input for Responses API
create_image_input(FilePath) ->
    create_image_input(FilePath, #{}).

create_image_input(FilePath, Options) ->
    ?LOG_INFO("[FILES] 🖼️  Creating image input from: ~s", [FilePath]),
    
    case validate_image_file(FilePath) of
        {ok, MimeType} ->
            % Upload the file first
            case upload_file(FilePath, <<"vision">>, Options) of
                {ok, #{<<"id">> := FileId}} ->
                    ?LOG_SUCCESS("[FILES] ✅ Image uploaded with ID: ~s", [FileId]),
                    
                    % Create the image input structure
                    ImageInput = #{
                        <<"type">> => <<"image_file">>,
                        <<"image_file">> => #{
                            <<"file_id">> => FileId
                        }
                    },
                    
                    {ok, ImageInput};
                {error, Reason} ->
                    ?LOG_ERROR("[FILES] ❌ Failed to upload image: ~p", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            ?LOG_ERROR("[FILES] ❌ Invalid image file: ~p", [Reason]),
            {error, Reason}
    end.

%% Validate file input structure
validate_file_input(#{<<"type">> := <<"image_file">>, <<"image_file">> := #{<<"file_id">> := FileId}}) 
    when is_binary(FileId) ->
    ?LOG_INFO("[FILES] ✅ Valid image file input: ~s", [FileId]),
    {ok, image_file};
validate_file_input(#{<<"type">> := <<"text">>, <<"text">> := Text}) 
    when is_binary(Text) ->
    {ok, text};
validate_file_input(Input) ->
    ?LOG_WARNING("[FILES] ⚠️  Invalid file input structure: ~p", [Input]),
    {error, invalid_input_structure}.

%% gen_server callbacks

init([]) ->
    ?LOG_INFO("[FILES] 🎬 Files manager starting..."),
    
    % Ensure inets application is started for httpc
    case application:ensure_all_started(inets) of
        {ok, _} -> ok;
        {error, _} = Error -> error(Error)
    end,
    
    % Get API key from environment
    ApiKey = case os:getenv("OPENAI_API_KEY") of
        false -> 
            error({missing_api_key, "OPENAI_API_KEY environment variable not set"});
        Key -> 
            list_to_binary(Key)
    end,
    
    {ok, #state{api_key = ApiKey}}.

handle_call({upload_file, FilePath, Purpose, Options}, _From, #state{api_key = ApiKey, base_url = BaseUrl} = State) ->
    case do_upload_file(FilePath, Purpose, Options, ApiKey, BaseUrl) of
        {ok, FileData} ->
            FileId = maps:get(<<"id">>, FileData, undefined),
            NewUploadedFiles = case FileId of
                undefined -> State#state.uploaded_files;
                _ -> (State#state.uploaded_files)#{FileId => #{
                    file_path => FilePath,
                    purpose => Purpose,
                    uploaded_at => os:system_time(millisecond)
                }}
            end,
            {reply, {ok, FileData}, State#state{uploaded_files = NewUploadedFiles}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({delete_file, FileId}, _From, #state{api_key = ApiKey, base_url = BaseUrl} = State) ->
    case do_delete_file(FileId, ApiKey, BaseUrl) of
        {ok, Result} ->
            NewUploadedFiles = maps:remove(FileId, State#state.uploaded_files),
            {reply, {ok, Result}, State#state{uploaded_files = NewUploadedFiles}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({get_file, FileId}, _From, #state{api_key = ApiKey, base_url = BaseUrl} = State) ->
    case do_get_file(FileId, ApiKey, BaseUrl) of
        {ok, FileData} ->
            {reply, {ok, FileData}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({list_files, Options}, _From, #state{api_key = ApiKey, base_url = BaseUrl} = State) ->
    case do_list_files(Options, ApiKey, BaseUrl) of
        {ok, Files} ->
            {reply, {ok, Files}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

do_upload_file(FilePath, Purpose, _Options, ApiKey, BaseUrl) ->
    ?LOG_INFO("[FILES] 🚀 Starting file upload..."),
    
    % Check if file exists
    case file:read_file(FilePath) of
        {ok, FileData} ->
            Filename = filename:basename(FilePath),
            ?LOG_INFO("[FILES] 📄 File size: ~p bytes", [byte_size(FileData)]),
            
            % Determine MIME type
            MimeType = guess_mime_type(FilePath),
            
            % Create multipart form data
            Boundary = "----WebKitFormBoundary" ++ integer_to_list(erlang:unique_integer([positive])),
            
            % Build multipart body
            MultipartBody = build_multipart_body([
                {<<"purpose">>, Purpose},
                {<<"file">>, FileData, Filename, MimeType}
            ], Boundary),
            
            Url = <<BaseUrl/binary, "/files">>,
            
            Headers = [
                {"Authorization", "Bearer " ++ binary_to_list(ApiKey)},
                {"Content-Type", "multipart/form-data; boundary=" ++ Boundary}
            ],
            
            ?LOG_INFO("[FILES] 📤 Uploading to OpenAI..."),
            
            case httpc:request(post, {binary_to_list(Url), Headers, "multipart/form-data", MultipartBody}, 
                              [{timeout, 120000}, {connect_timeout, 10000}], [{body_format, binary}]) of
                {ok, {{_, 200, _}, _, ResponseBody}} ->
                    case jsx:decode(ResponseBody, [return_maps]) of
                        FileResponse when is_map(FileResponse) ->
                            FileId = maps:get(<<"id">>, FileResponse, <<"unknown">>),
                            ?LOG_SUCCESS("[FILES] ✅ File uploaded successfully: ~s", [FileId]),
                            {ok, FileResponse};
                        _ ->
                            {error, invalid_response}
                    end;
                {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
                    ?LOG_ERROR("[FILES] ❌ Upload failed with status ~p", [StatusCode]),
                    case catch jsx:decode(ResponseBody, [return_maps]) of
                        #{<<"error">> := Error} ->
                            {error, {api_error, StatusCode, Error}};
                        _ ->
                            {error, {http_error, StatusCode, ResponseBody}}
                    end;
                {error, Reason} ->
                    ?LOG_ERROR("[FILES] ❌ HTTP request failed: ~p", [Reason]),
                    {error, {request_failed, Reason}}
            end;
        {error, Reason} ->
            ?LOG_ERROR("[FILES] ❌ Failed to read file ~s: ~p", [FilePath, Reason]),
            {error, {file_read_error, Reason}}
    end.

do_delete_file(FileId, ApiKey, BaseUrl) ->
    Url = <<BaseUrl/binary, "/files/", FileId/binary>>,
    
    Headers = [
        {"Authorization", "Bearer " ++ binary_to_list(ApiKey)}
    ],
    
    case httpc:request(delete, {binary_to_list(Url), Headers}, 
                      [{timeout, 60000}, {connect_timeout, 5000}], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(ResponseBody, [return_maps]) of
                Response when is_map(Response) ->
                    {ok, Response};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            case catch jsx:decode(ResponseBody, [return_maps]) of
                #{<<"error">> := Error} ->
                    {error, {api_error, StatusCode, Error}};
                _ ->
                    {error, {http_error, StatusCode, ResponseBody}}
            end;
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

do_get_file(FileId, ApiKey, BaseUrl) ->
    Url = <<BaseUrl/binary, "/files/", FileId/binary>>,
    
    Headers = [
        {"Authorization", "Bearer " ++ binary_to_list(ApiKey)}
    ],
    
    case httpc:request(get, {binary_to_list(Url), Headers}, 
                      [{timeout, 60000}, {connect_timeout, 5000}], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(ResponseBody, [return_maps]) of
                Response when is_map(Response) ->
                    {ok, Response};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            case catch jsx:decode(ResponseBody, [return_maps]) of
                #{<<"error">> := Error} ->
                    {error, {api_error, StatusCode, Error}};
                _ ->
                    {error, {http_error, StatusCode, ResponseBody}}
            end;
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

do_list_files(Options, ApiKey, BaseUrl) ->
    % Build query parameters
    QueryParams = build_query_params(Options),
    Url = case QueryParams of
        <<>> -> <<BaseUrl/binary, "/files">>;
        _ -> <<BaseUrl/binary, "/files?", QueryParams/binary>>
    end,
    
    Headers = [
        {"Authorization", "Bearer " ++ binary_to_list(ApiKey)}
    ],
    
    case httpc:request(get, {binary_to_list(Url), Headers}, 
                      [{timeout, 60000}, {connect_timeout, 5000}], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(ResponseBody, [return_maps]) of
                Response when is_map(Response) ->
                    {ok, Response};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            case catch jsx:decode(ResponseBody, [return_maps]) of
                #{<<"error">> := Error} ->
                    {error, {api_error, StatusCode, Error}};
                _ ->
                    {error, {http_error, StatusCode, ResponseBody}}
            end;
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

%% Helper functions

validate_image_file(FilePath) ->
    case file:read_file_info(FilePath) of
        {ok, _FileInfo} ->
            MimeType = guess_mime_type(FilePath),
            case lists:member(MimeType, ?SUPPORTED_IMAGE_TYPES) of
                true ->
                    {ok, MimeType};
                false ->
                    {error, {unsupported_image_type, MimeType}}
            end;
        {error, Reason} ->
            {error, {file_not_found, Reason}}
    end.

guess_mime_type(FilePath) ->
    Extension = string:lowercase(filename:extension(FilePath)),
    case Extension of
        ".png" -> <<"image/png">>;
        ".jpg" -> <<"image/jpeg">>;
        ".jpeg" -> <<"image/jpeg">>;
        ".gif" -> <<"image/gif">>;
        ".webp" -> <<"image/webp">>;
        ".txt" -> <<"text/plain">>;
        ".json" -> <<"application/json">>;
        ".pdf" -> <<"application/pdf">>;
        _ -> <<"application/octet-stream">>
    end.

build_multipart_body(Parts, Boundary) ->
    lists:flatten([
        build_multipart_part(Part, Boundary) || Part <- Parts
    ] ++ ["--" ++ Boundary ++ "--\r\n"]).

build_multipart_part({Name, Value}, Boundary) when is_binary(Name), is_binary(Value) ->
    [
        "--" ++ Boundary ++ "\r\n",
        "Content-Disposition: form-data; name=\"" ++ binary_to_list(Name) ++ "\"\r\n",
        "\r\n",
        binary_to_list(Value),
        "\r\n"
    ];
build_multipart_part({Name, FileData, Filename, MimeType}, Boundary) when is_binary(Name) ->
    [
        "--" ++ Boundary ++ "\r\n",
        "Content-Disposition: form-data; name=\"" ++ binary_to_list(Name) ++ "\"; filename=\"" ++ Filename ++ "\"\r\n",
        "Content-Type: " ++ binary_to_list(MimeType) ++ "\r\n",
        "\r\n",
        binary_to_list(FileData),
        "\r\n"
    ].

build_query_params(Options) ->
    Params = lists:foldl(fun({Key, DefaultValue}, Acc) ->
        case maps:get(Key, Options, DefaultValue) of
            DefaultValue -> Acc;
            Value ->
                ParamStr = io_lib:format("~s=~s", [Key, format_query_value(Value)]),
                [ParamStr | Acc]
        end
    end, [], [
        {purpose, undefined},
        {limit, undefined},
        {order, undefined},
        {'after', undefined}
    ]),
    case Params of
        [] -> <<>>;
        _ -> list_to_binary(string:join(lists:reverse(Params), "&"))
    end.

format_query_value(Value) when is_binary(Value) -> Value;
format_query_value(Value) when is_atom(Value) -> atom_to_list(Value);
format_query_value(Value) when is_integer(Value) -> integer_to_list(Value);
format_query_value(Value) when is_float(Value) -> float_to_list(Value);
format_query_value(Value) -> io_lib:format("~p", [Value]).