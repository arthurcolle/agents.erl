-module(jina_client).
-export([search/2, search/3, reader/2, reader/3, fact_check/2, 
         embeddings/3, embed_text/2, embed_image/2,
         rerank/4, classify/4, segment/2, segment/3,
         deep_search/2, deep_search/3, get_api_key/0]).

%% Jina AI Search Foundation API Client for Erlang Agents
%% Provides comprehensive integration with Jina AI APIs:
%% - Search API (s.jina.ai) - Web search with structured results  
%% - Reader API (r.jina.ai) - Extract content from webpages
%% - Fact Check API (g.jina.ai) - Grounding and factuality checking
%% - Embeddings API - Create vector representations of text/images
%% - Reranker API - Improve search relevance by reranking documents
%% - Classification API - Categorize text/images into predefined labels
%% - Segmenter API - Split text into semantic chunks
%% - DeepSearch API - Comprehensive search with reasoning

-define(BASE_HEADERS, [
    {<<"Content-Type">>, <<"application/json">>},
    {<<"Accept">>, <<"application/json">>}
]).

-define(DEFAULT_TIMEOUT, 30000).

%%====================================================================
%% API Functions
%%====================================================================

%% Web Search using s.jina.ai
search(Query, Opts) ->
    search(Query, Opts, get_api_key()).

search(Query, Opts, ApiKey) when is_binary(ApiKey) ->
    Url = <<"https://s.jina.ai/">>,
    Headers = add_auth_header(?BASE_HEADERS, ApiKey),
    
    % Extract options
    Site = maps:get(site, Opts, undefined),
    Num = maps:get(num, Opts, 5),
    Gl = maps:get(gl, Opts, undefined),
    Hl = maps:get(hl, Opts, undefined),
    
    % Add site header if specified
    HeadersWithSite = case Site of
        undefined -> Headers;
        _ -> [{<<"X-Site">>, Site} | Headers]
    end,
    
    % Add feature headers
    FinalHeaders = [
        {<<"X-With-Links-Summary">>, <<"true">>},
        {<<"X-With-Images-Summary">>, <<"true">>}
        | HeadersWithSite
    ],
    
    % Build request body
    Body = #{
        <<"q">> => Query,
        <<"num">> => Num
    },
    
    % Add optional parameters
    BodyWithOpts = add_optional_param(Body, <<"gl">>, Gl),
    BodyWithOpts2 = add_optional_param(BodyWithOpts, <<"hl">>, Hl),
    
    case make_request(post, Url, FinalHeaders, jsx:encode(BodyWithOpts2)) of
        {ok, Response} ->
            parse_search_response(Response);
        {error, Reason} ->
            {error, Reason}
    end;
search(_Query, _Opts, undefined) ->
    {error, no_api_key}.

%% Web Page Reader using r.jina.ai
reader(Url, Opts) ->
    reader(Url, Opts, get_api_key()).

reader(Url, Opts, ApiKey) when is_binary(ApiKey) ->
    Endpoint = <<"https://r.jina.ai/">>,
    Headers = add_auth_header(?BASE_HEADERS, ApiKey),
    
    % Extract options
    Timeout = maps:get(timeout, Opts, undefined),
    TargetSelector = maps:get(target_selector, Opts, undefined),
    RemoveSelector = maps:get(remove_selector, Opts, undefined),
    WithLinks = maps:get(with_links, Opts, true),
    WithImages = maps:get(with_images, Opts, true),
    NoCache = maps:get(no_cache, Opts, false),
    
    % Add optional headers
    HeadersWithOpts = lists:foldl(fun({Key, Value, Condition}, Acc) ->
        case Condition of
            undefined -> Acc;
            false when Key =:= <<"X-No-Cache">> -> Acc;
            false -> Acc;
            true when Key =:= <<"X-With-Links-Summary">> -> [{Key, <<"true">>} | Acc];
            true when Key =:= <<"X-With-Images-Summary">> -> [{Key, <<"true">>} | Acc];
            true when Key =:= <<"X-No-Cache">> -> [{Key, <<"true">>} | Acc];
            _ when is_binary(Value) -> [{Key, Value} | Acc];
            _ when is_integer(Value) -> [{Key, integer_to_binary(Value)} | Acc];
            _ -> Acc
        end
    end, Headers, [
        {<<"X-Timeout">>, Timeout, Timeout},
        {<<"X-Target-Selector">>, TargetSelector, TargetSelector},
        {<<"X-Remove-Selector">>, RemoveSelector, RemoveSelector},
        {<<"X-With-Links-Summary">>, true, WithLinks},
        {<<"X-With-Images-Summary">>, true, WithImages},
        {<<"X-No-Cache">>, true, NoCache}
    ]),
    
    % Build request body
    Body = #{<<"url">> => Url},
    
    case make_request(post, Endpoint, HeadersWithOpts, jsx:encode(Body)) of
        {ok, Response} ->
            parse_reader_response(Response);
        {error, Reason} ->
            {error, Reason}
    end;
reader(_Url, _Opts, undefined) ->
    {error, no_api_key}.

%% Fact Check using g.jina.ai
fact_check(Query, ApiKey) ->
    EncodedQuery = uri_string:quote(Query),
    Url = <<"https://g.jina.ai/", EncodedQuery/binary>>,
    Headers = add_auth_header(?BASE_HEADERS, ApiKey),
    
    case make_request(get, Url, Headers, <<>>) of
        {ok, Response} ->
            parse_fact_check_response(Response);
        {error, Reason} ->
            {error, Reason}
    end.

%% Embeddings API
embeddings(Input, Model, Opts) ->
    ApiKey = get_api_key(),
    case ApiKey of
        undefined -> {error, no_api_key};
        _ -> embeddings_with_key(Input, Model, Opts, ApiKey)
    end.

embeddings_with_key(Input, Model, Opts, ApiKey) ->
    Url = <<"https://api.jina.ai/v1/embeddings">>,
    Headers = add_auth_header(?BASE_HEADERS, ApiKey),
    
    % Normalize input to list
    InputList = case is_list(Input) of
        true -> Input;
        false -> [Input]
    end,
    
    % Extract options
    Normalized = maps:get(normalized, Opts, true),
    EmbeddingType = maps:get(embedding_type, Opts, <<"float">>),
    Dimensions = maps:get(dimensions, Opts, undefined),
    Task = maps:get(task, Opts, undefined),
    
    % Build request body
    Body = #{
        <<"model">> => Model,
        <<"input">> => InputList,
        <<"normalized">> => Normalized
    },
    
    % Add optional parameters
    BodyWithOpts = add_optional_param(Body, <<"embedding_type">>, EmbeddingType),
    BodyWithOpts2 = add_optional_param(BodyWithOpts, <<"dimensions">>, Dimensions),
    BodyWithOpts3 = add_optional_param(BodyWithOpts2, <<"task">>, Task),
    
    case make_request(post, Url, Headers, jsx:encode(BodyWithOpts3)) of
        {ok, Response} ->
            parse_embeddings_response(Response);
        {error, Reason} ->
            {error, Reason}
    end.

%% Text Embeddings Helper
embed_text(Text, Model) ->
    Opts = #{},
    embeddings(Text, Model, Opts).

%% Image Embeddings Helper  
embed_image(ImageSource, Model) ->
    % Process image sources to ensure proper format
    ProcessedImages = case is_list(ImageSource) of
        true -> [process_image(Img) || Img <- ImageSource];
        false -> [process_image(ImageSource)]
    end,
    
    Opts = #{},
    embeddings(ProcessedImages, Model, Opts).

%% Reranker API
rerank(Query, Documents, Model, Opts) ->
    ApiKey = get_api_key(),
    case ApiKey of
        undefined -> {error, no_api_key};
        _ -> rerank_with_key(Query, Documents, Model, Opts, ApiKey)
    end.

rerank_with_key(Query, Documents, Model, Opts, ApiKey) ->
    Url = <<"https://api.jina.ai/v1/rerank">>,
    Headers = add_auth_header(?BASE_HEADERS, ApiKey),
    
    % Extract options
    TopN = maps:get(top_n, Opts, undefined),
    ReturnDocuments = maps:get(return_documents, Opts, true),
    
    % Build request body
    Body = #{
        <<"model">> => Model,
        <<"query">> => Query,
        <<"documents">> => Documents,
        <<"return_documents">> => ReturnDocuments
    },
    
    % Add optional parameters
    BodyWithOpts = add_optional_param(Body, <<"top_n">>, TopN),
    
    case make_request(post, Url, Headers, jsx:encode(BodyWithOpts)) of
        {ok, Response} ->
            parse_rerank_response(Response);
        {error, Reason} ->
            {error, Reason}
    end.

%% Classification API
classify(Inputs, Labels, Model, IsImage) ->
    ApiKey = get_api_key(),
    case ApiKey of
        undefined -> {error, no_api_key};
        _ -> classify_with_key(Inputs, Labels, Model, IsImage, ApiKey)
    end.

classify_with_key(Inputs, Labels, Model, IsImage, ApiKey) ->
    Url = <<"https://api.jina.ai/v1/classify">>,
    Headers = add_auth_header(?BASE_HEADERS, ApiKey),
    
    % Format inputs based on type
    FormattedInputs = case IsImage of
        true -> [format_image_input(Img) || Img <- Inputs];
        false -> Inputs
    end,
    
    % Build request body
    Body = #{
        <<"model">> => Model,
        <<"input">> => FormattedInputs,
        <<"labels">> => Labels
    },
    
    case make_request(post, Url, Headers, jsx:encode(Body)) of
        {ok, Response} ->
            parse_classify_response(Response);
        {error, Reason} ->
            {error, Reason}
    end.

%% Segmenter API
segment(Content, Opts) ->
    segment(Content, Opts, get_api_key()).

segment(Content, Opts, ApiKey) when is_binary(ApiKey) ->
    Url = <<"https://segment.jina.ai/">>,
    Headers = add_auth_header(?BASE_HEADERS, ApiKey),
    
    % Extract options
    Tokenizer = maps:get(tokenizer, Opts, <<"cl100k_base">>),
    ReturnTokens = maps:get(return_tokens, Opts, false),
    ReturnChunks = maps:get(return_chunks, Opts, true),
    MaxChunkLength = maps:get(max_chunk_length, Opts, 1000),
    Head = maps:get(head, Opts, undefined),
    Tail = maps:get(tail, Opts, undefined),
    
    % Build request body
    Body = #{
        <<"content">> => Content,
        <<"tokenizer">> => Tokenizer,
        <<"return_tokens">> => ReturnTokens,
        <<"return_chunks">> => ReturnChunks,
        <<"max_chunk_length">> => MaxChunkLength
    },
    
    % Add optional parameters
    BodyWithOpts = add_optional_param(Body, <<"head">>, Head),
    BodyWithOpts2 = add_optional_param(BodyWithOpts, <<"tail">>, Tail),
    
    case make_request(post, Url, Headers, jsx:encode(BodyWithOpts2)) of
        {ok, Response} ->
            parse_segment_response(Response);
        {error, Reason} ->
            {error, Reason}
    end;
segment(_Content, _Opts, undefined) ->
    {error, no_api_key}.

%% DeepSearch API
deep_search(Messages, Opts) ->
    deep_search(Messages, Opts, get_api_key()).

deep_search(Messages, Opts, ApiKey) when is_binary(ApiKey) ->
    Url = <<"https://deepsearch.jina.ai/v1/chat/completions">>,
    Headers = add_auth_header(?BASE_HEADERS, ApiKey),
    
    % Extract options
    Model = maps:get(model, Opts, <<"jina-deepsearch-v1">>),
    ReasoningEffort = maps:get(reasoning_effort, Opts, <<"medium">>),
    MaxReturnedUrls = maps:get(max_returned_urls, Opts, 5),
    BoostHostnames = maps:get(boost_hostnames, Opts, undefined),
    BadHostnames = maps:get(bad_hostnames, Opts, undefined),
    
    % Build request body
    Body = #{
        <<"model">> => Model,
        <<"messages">> => Messages,
        <<"stream">> => false,
        <<"reasoning_effort">> => ReasoningEffort,
        <<"max_returned_urls">> => MaxReturnedUrls,
        <<"no_direct_answer">> => false
    },
    
    % Add optional parameters
    BodyWithOpts = add_optional_param(Body, <<"boost_hostnames">>, BoostHostnames),
    BodyWithOpts2 = add_optional_param(BodyWithOpts, <<"bad_hostnames">>, BadHostnames),
    
    case make_request(post, Url, Headers, jsx:encode(BodyWithOpts2)) of
        {ok, Response} ->
            parse_deep_search_response(Response);
        {error, Reason} ->
            {error, Reason}
    end;
deep_search(_Messages, _Opts, undefined) ->
    {error, no_api_key}.

%%====================================================================
%% Internal Functions
%%====================================================================

get_api_key() ->
    case os:getenv("JINA_API_KEY") of
        false -> undefined;
        Key -> list_to_binary(Key)
    end.


add_auth_header(Headers, ApiKey) ->
    AuthHeader = {<<"Authorization">>, <<"Bearer ", ApiKey/binary>>},
    [AuthHeader | Headers].

add_optional_param(Body, _Key, undefined) ->
    Body;
add_optional_param(Body, Key, Value) ->
    Body#{Key => Value}.

process_image(Image) when is_binary(Image) ->
    case binary:match(Image, [<<"http://">>, <<"https://">>, <<"data:image/">>]) of
        {0, _} -> Image; % Already a URL or data URL
        nomatch ->
            % Assume base64, add data URL prefix
            <<"data:image/jpeg;base64,", Image/binary>>
    end.

format_image_input(Image) ->
    #{<<"image">> => process_image(Image)}.

make_request(Method, Url, Headers, Body) ->
    % Ensure required applications are started with proper error handling
    ok = ensure_app_started(inets),
    ok = ensure_app_started(ssl),
    ok = ensure_app_started(crypto),
    ok = ensure_app_started(public_key),
    
    RequestOpts = [
        {timeout, ?DEFAULT_TIMEOUT},
        {connect_timeout, 10000}
    ],
    
    % Convert headers to list format for httpc
    HeadersList = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- Headers],
    
    Request = case Method of
        get -> {binary_to_list(Url), HeadersList};
        post -> {binary_to_list(Url), HeadersList, "application/json", Body};
        _ -> {binary_to_list(Url), HeadersList, "application/json", Body}
    end,
    
    case httpc:request(Method, Request, RequestOpts, []) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, ResponseBody}} ->
            try
                Decoded = jsx:decode(list_to_binary(ResponseBody), [return_maps]),
                {ok, Decoded}
            catch
                _:_ ->
                    {ok, #{<<"content">> => list_to_binary(ResponseBody)}}
            end;
        {ok, {{_Version, StatusCode, ReasonPhrase}, _Headers, ResponseBody}} ->
            {error, #{
                status_code => StatusCode,
                reason => list_to_binary(ReasonPhrase),
                body => list_to_binary(ResponseBody)
            }};
        {error, Reason} ->
            {error, #{reason => Reason}}
    end.

%% Response parsers
parse_search_response(#{<<"data">> := Results}) when is_list(Results) ->
    FormattedResults = [format_search_result(Result) || Result <- Results],
    {ok, #{results => FormattedResults}};
parse_search_response(#{<<"results">> := Content}) ->
    {ok, #{content => Content}};
parse_search_response(Other) ->
    {ok, Other}.

format_search_result(#{<<"title">> := Title, <<"url">> := Url, <<"content">> := Content}) ->
    #{
        title => Title,
        url => Url,
        content => Content
    };
format_search_result(Other) ->
    Other.

parse_reader_response(#{<<"data">> := Data}) ->
    Title = maps:get(<<"title">>, Data, <<"No title">>),
    Content = maps:get(<<"content">>, Data, <<"No content">>),
    Links = maps:get(<<"links">>, Data, #{}),
    Images = maps:get(<<"images">>, Data, #{}),
    
    {ok, #{
        title => Title,
        content => Content,
        links => Links,
        images => Images
    }};
parse_reader_response(#{<<"content">> := Content}) ->
    {ok, #{content => Content}};
parse_reader_response(Other) ->
    {ok, Other}.

parse_fact_check_response(Response) ->
    FactualityScore = maps:get(<<"factuality_score">>, Response, 0.0),
    References = maps:get(<<"references">>, Response, []),
    Analysis = maps:get(<<"analysis">>, Response, <<"No analysis available">>),
    
    {ok, #{
        factuality_score => FactualityScore,
        references => References,
        analysis => Analysis
    }}.

parse_embeddings_response(#{<<"data">> := Data}) ->
    Embeddings = [maps:get(<<"embedding">>, Item, []) || Item <- Data],
    {ok, #{embeddings => Embeddings}};
parse_embeddings_response(Other) ->
    {ok, Other}.

parse_rerank_response(#{<<"results">> := Results}) ->
    FormattedResults = [format_rerank_result(Result) || Result <- Results],
    {ok, #{results => FormattedResults}};
parse_rerank_response(Other) ->
    {ok, Other}.

format_rerank_result(#{<<"relevance_score">> := Score, <<"index">> := Index} = Result) ->
    #{
        score => Score,
        index => Index,
        document => maps:get(<<"document">>, Result, undefined)
    };
format_rerank_result(Other) ->
    Other.

parse_classify_response(#{<<"data">> := Data}) ->
    Classifications = [format_classify_result(Result) || Result <- Data],
    {ok, #{classifications => Classifications}};
parse_classify_response(Other) ->
    {ok, Other}.

format_classify_result(#{<<"prediction">> := Prediction, <<"score">> := Score} = Result) ->
    #{
        prediction => Prediction,
        score => Score,
        predictions => maps:get(<<"predictions">>, Result, [])
    };
format_classify_result(Other) ->
    Other.

parse_segment_response(Response) ->
    NumTokens = maps:get(<<"num_tokens">>, Response, 0),
    Chunks = maps:get(<<"chunks">>, Response, []),
    Tokens = maps:get(<<"tokens">>, Response, []),
    
    {ok, #{
        num_tokens => NumTokens,
        chunks => Chunks,
        tokens => Tokens
    }}.

parse_deep_search_response(#{<<"choices">> := [Choice | _]}) ->
    Message = maps:get(<<"message">>, Choice, #{}),
    Content = maps:get(<<"content">>, Message, <<"No content">>),
    Sources = maps:get(<<"sources">>, Message, []),
    
    {ok, #{
        content => Content,
        sources => Sources
    }};
parse_deep_search_response(Other) ->
    {ok, Other}.

%% Helper to ensure applications are started
ensure_app_started(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, App}} -> ok;
        {error, {not_started, Dep}} ->
            ok = ensure_app_started(Dep),
            ensure_app_started(App);
        {error, Reason} ->
            error({app_start_failed, App, Reason})
    end.