-module(jina_tools).
-export([register_all_tools/1, 
         jina_search/1, jina_read_webpage/1, jina_fact_check/1,
         jina_embed_text/1, jina_embed_image/1, jina_rerank/1,
         jina_classify/1, jina_segment/1, jina_deep_search/1]).

%% Jina AI Tools for Erlang Agents MCP Server
%% Provides comprehensive search, embedding, classification, and document processing

%%====================================================================
%% Tool Registration
%%====================================================================

register_all_tools(ServerPid) ->
    Tools = [
        {<<"jina_search">>, #{
            <<"description">> => <<"Search the web using Jina AI Search API with structured results">>,
            <<"inputSchema">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"query">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The search query">>
                    },
                    <<"num_results">> => #{
                        <<"type">> => <<"integer">>,
                        <<"description">> => <<"Maximum number of results to return (default: 5)">>,
                        <<"default">> => 5
                    },
                    <<"site">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Optional domain to restrict search to">>
                    },
                    <<"country">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Two-letter country code for search region">>
                    },
                    <<"language">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Two-letter language code for search language">>
                    }
                },
                <<"required">> => [<<"query">>]
            }
        }},
        
        {<<"jina_read_webpage">>, #{
            <<"description">> => <<"Extract and read content from a webpage using Jina AI Reader API">>,
            <<"inputSchema">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"url">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"URL of the webpage to read">>
                    },
                    <<"target_selector">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"CSS selector to focus on specific elements">>
                    },
                    <<"remove_selector">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"CSS selector to exclude certain parts">>
                    },
                    <<"timeout">> => #{
                        <<"type">> => <<"integer">>,
                        <<"description">> => <<"Maximum time to wait for page load (seconds)">>
                    },
                    <<"no_cache">> => #{
                        <<"type">> => <<"boolean">>,
                        <<"description">> => <<"Bypass cache for fresh retrieval">>,
                        <<"default">> => false
                    }
                },
                <<"required">> => [<<"url">>]
            }
        }},
        
        {<<"jina_fact_check">>, #{
            <<"description">> => <<"Fact-check a query using Jina AI grounding capabilities">>,
            <<"inputSchema">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"query">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The query or statement to fact-check">>
                    }
                },
                <<"required">> => [<<"query">>]
            }
        }},
        
        {<<"jina_embed_text">>, #{
            <<"description">> => <<"Create embeddings for text using Jina AI embedding models">>,
            <<"inputSchema">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"text">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Text to create embeddings for">>
                    },
                    <<"model">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Embedding model to use">>,
                        <<"default">> => <<"jina-embeddings-v3">>,
                        <<"enum">> => [<<"jina-embeddings-v3">>, <<"jina-embeddings-v2">>]
                    }
                },
                <<"required">> => [<<"text">>]
            }
        }},
        
        {<<"jina_embed_image">>, #{
            <<"description">> => <<"Create embeddings for images using Jina AI multimodal models">>,
            <<"inputSchema">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"image_url">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"URL or base64 string of the image">>
                    },
                    <<"model">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Embedding model to use">>,
                        <<"default">> => <<"jina-clip-v2">>,
                        <<"enum">> => [<<"jina-clip-v2">>, <<"jina-clip-v1">>]
                    }
                },
                <<"required">> => [<<"image_url">>]
            }
        }},
        
        {<<"jina_rerank">>, #{
            <<"description">> => <<"Rerank documents based on relevance to a query using Jina AI Reranker">>,
            <<"inputSchema">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"query">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The search query">>
                    },
                    <<"documents">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Documents separated by '---'">>
                    },
                    <<"model">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Reranker model to use">>,
                        <<"default">> => <<"jina-reranker-v2-base-multilingual">>,
                        <<"enum">> => [<<"jina-reranker-v2-base-multilingual">>, <<"jina-reranker-v1">>, <<"jina-colbert-v2">>]
                    },
                    <<"top_n">> => #{
                        <<"type">> => <<"integer">>,
                        <<"description">> => <<"Number of top results to return">>,
                        <<"default">> => 5
                    }
                },
                <<"required">> => [<<"query">>, <<"documents">>]
            }
        }},
        
        {<<"jina_classify">>, #{
            <<"description">> => <<"Classify text or images into predefined categories using Jina AI">>,
            <<"inputSchema">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"inputs">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Items to classify, separated by '---'">>
                    },
                    <<"labels">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Comma-separated classification labels">>
                    },
                    <<"is_image">> => #{
                        <<"type">> => <<"boolean">>,
                        <<"description">> => <<"Whether inputs are images">>,
                        <<"default">> => false
                    },
                    <<"model">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Model to use (auto-selected based on input type if not specified)">>
                    }
                },
                <<"required">> => [<<"inputs">>, <<"labels">>]
            }
        }},
        
        {<<"jina_segment">>, #{
            <<"description">> => <<"Segment text into manageable chunks using Jina AI Segmenter">>,
            <<"inputSchema">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"content">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Text content to segment">>
                    },
                    <<"tokenizer">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Tokenizer to use">>,
                        <<"default">> => <<"cl100k_base">>,
                        <<"enum">> => [<<"cl100k_base">>, <<"o200k_base">>, <<"p50k_base">>, <<"r50k_base">>, <<"gpt2">>]
                    },
                    <<"max_chunk_length">> => #{
                        <<"type">> => <<"integer">>,
                        <<"description">> => <<"Maximum characters per chunk">>,
                        <<"default">> => 1000
                    },
                    <<"return_tokens">> => #{
                        <<"type">> => <<"boolean">>,
                        <<"description">> => <<"Include token details in response">>,
                        <<"default">> => false
                    }
                },
                <<"required">> => [<<"content">>]
            }
        }},
        
        {<<"jina_deep_search">>, #{
            <<"description">> => <<"Perform comprehensive search with reasoning using Jina AI DeepSearch">>,
            <<"inputSchema">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"query">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The search query">>
                    },
                    <<"reasoning_effort">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Level of reasoning effort">>,
                        <<"default">> => <<"medium">>,
                        <<"enum">> => [<<"low">>, <<"medium">>, <<"high">>]
                    },
                    <<"max_returned_urls">> => #{
                        <<"type">> => <<"integer">>,
                        <<"description">> => <<"Maximum URLs to include in result">>,
                        <<"default">> => 5
                    },
                    <<"boost_hostnames">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Comma-separated domains to prioritize">>
                    },
                    <<"bad_hostnames">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Comma-separated domains to exclude">>
                    }
                },
                <<"required">> => [<<"query">>]
            }
        }}
    ],
    
    % Register each tool
    Results = [mcp_server:register_tool(ServerPid, ToolName, ToolDef) || {ToolName, ToolDef} <- Tools],
    
    % Check if all registrations succeeded
    case lists:all(fun(R) -> R =:= ok end, Results) of
        true -> 
            logger:info("Successfully registered ~p Jina AI tools", [length(Tools)]),
            ok;
        false -> 
            logger:warning("Some Jina AI tools failed to register"),
            {error, partial_registration}
    end.

%%====================================================================
%% Tool Implementation Functions
%%====================================================================

jina_search(Args) ->
    Query = maps:get(<<"query">>, Args),
    NumResults = maps:get(<<"num_results">>, Args, 5),
    Site = maps:get(<<"site">>, Args, undefined),
    Country = maps:get(<<"country">>, Args, undefined),
    Language = maps:get(<<"language">>, Args, undefined),
    
    Opts = #{
        num => NumResults,
        site => Site,
        gl => Country,
        hl => Language
    },
    
    case jina_client:search(Query, Opts) of
        {ok, #{results := Results}} ->
            FormattedResults = format_search_results(Results),
            {ok, #{
                <<"content">> => [#{
                    <<"type">> => <<"text">>,
                    <<"text">> => FormattedResults
                }]
            }};
        {ok, #{content := Content}} ->
            {ok, #{
                <<"content">> => [#{
                    <<"type">> => <<"text">>,
                    <<"text">> => Content
                }]
            }};
        {error, no_api_key} ->
            {error, #{
                <<"code">> => <<"JINA_API_KEY_MISSING">>,
                <<"message">> => <<"JINA_API_KEY environment variable not set. Get your free key at: https://jina.ai/?sui=apikey">>
            }};
        {error, Reason} ->
            {error, #{
                <<"code">> => <<"JINA_SEARCH_ERROR">>,
                <<"message">> => format_error(Reason)
            }}
    end.

jina_read_webpage(Args) ->
    Url = maps:get(<<"url">>, Args),
    TargetSelector = maps:get(<<"target_selector">>, Args, undefined),
    RemoveSelector = maps:get(<<"remove_selector">>, Args, undefined),
    Timeout = maps:get(<<"timeout">>, Args, undefined),
    NoCache = maps:get(<<"no_cache">>, Args, false),
    
    Opts = #{
        target_selector => TargetSelector,
        remove_selector => RemoveSelector,
        timeout => Timeout,
        no_cache => NoCache,
        with_links => true,
        with_images => true
    },
    
    case jina_client:reader(Url, Opts) of
        {ok, Result} ->
            FormattedContent = format_reader_result(Result),
            {ok, #{
                <<"content">> => [#{
                    <<"type">> => <<"text">>,
                    <<"text">> => FormattedContent
                }]
            }};
        {error, no_api_key} ->
            {error, #{
                <<"code">> => <<"JINA_API_KEY_MISSING">>,
                <<"message">> => <<"JINA_API_KEY environment variable not set. Get your free key at: https://jina.ai/?sui=apikey">>
            }};
        {error, Reason} ->
            {error, #{
                <<"code">> => <<"JINA_READER_ERROR">>,
                <<"message">> => format_error(Reason)
            }}
    end.

jina_fact_check(Args) ->
    Query = maps:get(<<"query">>, Args),
    
    case jina_client:fact_check(Query, jina_client:get_api_key()) of
        {ok, Result} ->
            FormattedResult = format_fact_check_result(Result),
            {ok, #{
                <<"content">> => [#{
                    <<"type">> => <<"text">>,
                    <<"text">> => FormattedResult
                }]
            }};
        {error, Reason} ->
            {error, #{
                <<"code">> => <<"JINA_FACT_CHECK_ERROR">>,
                <<"message">> => format_error(Reason)
            }}
    end.

jina_embed_text(Args) ->
    Text = maps:get(<<"text">>, Args),
    Model = maps:get(<<"model">>, Args, <<"jina-embeddings-v3">>),
    
    case jina_client:embed_text(Text, Model) of
        {ok, #{embeddings := [Embedding | _]}} ->
            Dimensions = length(Embedding),
            SampleValues = lists:sublist(Embedding, 5),
            FormattedSample = string:join([float_to_list(V, [{decimals, 6}]) || V <- SampleValues], ", "),
            
            Result = io_lib:format("Successfully created embedding with ~p dimensions.~n"
                                 "Sample values: ~s~s", 
                                 [Dimensions, FormattedSample, 
                                  case length(Embedding) > 5 of true -> ", ..."; false -> "" end]),
            
            {ok, #{
                <<"content">> => [#{
                    <<"type">> => <<"text">>,
                    <<"text">> => iolist_to_binary(Result)
                }]
            }};
        {ok, #{embeddings := []}} ->
            {error, #{
                <<"code">> => <<"NO_EMBEDDINGS">>,
                <<"message">> => <<"No embeddings were generated">>
            }};
        {error, no_api_key} ->
            {error, #{
                <<"code">> => <<"JINA_API_KEY_MISSING">>,
                <<"message">> => <<"JINA_API_KEY environment variable not set. Get your free key at: https://jina.ai/?sui=apikey">>
            }};
        {error, Reason} ->
            {error, #{
                <<"code">> => <<"JINA_EMBEDDING_ERROR">>,
                <<"message">> => format_error(Reason)
            }}
    end.

jina_embed_image(Args) ->
    ImageUrl = maps:get(<<"image_url">>, Args),
    Model = maps:get(<<"model">>, Args, <<"jina-clip-v2">>),
    
    case jina_client:embed_image(ImageUrl, Model) of
        {ok, #{embeddings := [Embedding | _]}} ->
            Dimensions = length(Embedding),
            SampleValues = lists:sublist(Embedding, 5),
            FormattedSample = string:join([float_to_list(V, [{decimals, 6}]) || V <- SampleValues], ", "),
            
            Result = io_lib:format("Successfully created image embedding with ~p dimensions.~n"
                                 "Sample values: ~s~s", 
                                 [Dimensions, FormattedSample, 
                                  case length(Embedding) > 5 of true -> ", ..."; false -> "" end]),
            
            {ok, #{
                <<"content">> => [#{
                    <<"type">> => <<"text">>,
                    <<"text">> => iolist_to_binary(Result)
                }]
            }};
        {ok, #{embeddings := []}} ->
            {error, #{
                <<"code">> => <<"NO_EMBEDDINGS">>,
                <<"message">> => <<"No image embeddings were generated">>
            }};
        {error, no_api_key} ->
            {error, #{
                <<"code">> => <<"JINA_API_KEY_MISSING">>,
                <<"message">> => <<"JINA_API_KEY environment variable not set. Get your free key at: https://jina.ai/?sui=apikey">>
            }};
        {error, Reason} ->
            {error, #{
                <<"code">> => <<"JINA_EMBEDDING_ERROR">>,
                <<"message">> => format_error(Reason)
            }}
    end.

jina_rerank(Args) ->
    Query = maps:get(<<"query">>, Args),
    DocumentsStr = maps:get(<<"documents">>, Args),
    Model = maps:get(<<"model">>, Args, <<"jina-reranker-v2-base-multilingual">>),
    TopN = maps:get(<<"top_n">>, Args, 5),
    
    % Split documents by "---"
    Documents = [string:strip(Doc) || Doc <- string:tokens(binary_to_list(DocumentsStr), "---"), Doc =/= ""],
    
    case length(Documents) of
        0 ->
            {error, #{
                <<"code">> => <<"NO_DOCUMENTS">>,
                <<"message">> => <<"No documents provided for reranking">>
            }};
        _ ->
            Opts = #{top_n => TopN},
            case jina_client:rerank(Query, [list_to_binary(D) || D <- Documents], Model, Opts) of
                {ok, #{results := Results}} ->
                    FormattedResults = format_rerank_results(Results, Documents),
                    {ok, #{
                        <<"content">> => [#{
                            <<"type">> => <<"text">>,
                            <<"text">> => FormattedResults
                        }]
                    }};
                {error, no_api_key} ->
                    {error, #{
                        <<"code">> => <<"JINA_API_KEY_MISSING">>,
                        <<"message">> => <<"JINA_API_KEY environment variable not set. Get your free key at: https://jina.ai/?sui=apikey">>
                    }};
                {error, Reason} ->
                    {error, #{
                        <<"code">> => <<"JINA_RERANK_ERROR">>,
                        <<"message">> => format_error(Reason)
                    }}
            end
    end.

jina_classify(Args) ->
    InputsStr = maps:get(<<"inputs">>, Args),
    LabelsStr = maps:get(<<"labels">>, Args),
    IsImage = maps:get(<<"is_image">>, Args, false),
    Model = maps:get(<<"model">>, Args, undefined),
    
    % Process inputs and labels
    Inputs = [string:strip(Input) || Input <- string:tokens(binary_to_list(InputsStr), "---"), Input =/= ""],
    Labels = [string:strip(Label) || Label <- string:tokens(binary_to_list(LabelsStr), ","), Label =/= ""],
    
    case {length(Inputs), length(Labels)} of
        {0, _} ->
            {error, #{
                <<"code">> => <<"NO_INPUTS">>,
                <<"message">> => <<"No input items provided for classification">>
            }};
        {_, N} when N < 2 ->
            {error, #{
                <<"code">> => <<"INSUFFICIENT_LABELS">>,
                <<"message">> => <<"At least two labels must be provided for classification">>
            }};
        _ ->
            % Choose model if not provided
            SelectedModel = case Model of
                undefined -> 
                    case IsImage of
                        true -> <<"jina-clip-v2">>;
                        false -> <<"jina-embeddings-v3">>
                    end;
                _ -> Model
            end,
            
            case jina_client:classify([list_to_binary(I) || I <- Inputs], 
                                    [list_to_binary(L) || L <- Labels], 
                                    SelectedModel, IsImage) of
                {ok, #{classifications := Classifications}} ->
                    FormattedResults = format_classify_results(Classifications, Inputs),
                    {ok, #{
                        <<"content">> => [#{
                            <<"type">> => <<"text">>,
                            <<"text">> => FormattedResults
                        }]
                    }};
                {error, no_api_key} ->
                    {error, #{
                        <<"code">> => <<"JINA_API_KEY_MISSING">>,
                        <<"message">> => <<"JINA_API_KEY environment variable not set. Get your free key at: https://jina.ai/?sui=apikey">>
                    }};
                {error, Reason} ->
                    {error, #{
                        <<"code">> => <<"JINA_CLASSIFY_ERROR">>,
                        <<"message">> => format_error(Reason)
                    }}
            end
    end.

jina_segment(Args) ->
    Content = maps:get(<<"content">>, Args),
    Tokenizer = maps:get(<<"tokenizer">>, Args, <<"cl100k_base">>),
    MaxChunkLength = maps:get(<<"max_chunk_length">>, Args, 1000),
    ReturnTokens = maps:get(<<"return_tokens">>, Args, false),
    
    Opts = #{
        tokenizer => Tokenizer,
        max_chunk_length => MaxChunkLength,
        return_tokens => ReturnTokens
    },
    
    case jina_client:segment(Content, Opts) of
        {ok, Result} ->
            FormattedResult = format_segment_result(Result),
            {ok, #{
                <<"content">> => [#{
                    <<"type">> => <<"text">>,
                    <<"text">> => FormattedResult
                }]
            }};
        {error, no_api_key} ->
            {error, #{
                <<"code">> => <<"JINA_API_KEY_MISSING">>,
                <<"message">> => <<"JINA_API_KEY environment variable not set. Get your free key at: https://jina.ai/?sui=apikey">>
            }};
        {error, Reason} ->
            {error, #{
                <<"code">> => <<"JINA_SEGMENT_ERROR">>,
                <<"message">> => format_error(Reason)
            }}
    end.

jina_deep_search(Args) ->
    Query = maps:get(<<"query">>, Args),
    ReasoningEffort = maps:get(<<"reasoning_effort">>, Args, <<"medium">>),
    MaxReturnedUrls = maps:get(<<"max_returned_urls">>, Args, 5),
    BoostHostnames = maps:get(<<"boost_hostnames">>, Args, undefined),
    BadHostnames = maps:get(<<"bad_hostnames">>, Args, undefined),
    
    % Convert comma-separated hostnames to lists
    BoostList = case BoostHostnames of
        undefined -> undefined;
        _ -> [list_to_binary(string:strip(H)) || H <- string:tokens(binary_to_list(BoostHostnames), ",")]
    end,
    
    BadList = case BadHostnames of
        undefined -> undefined;
        _ -> [list_to_binary(string:strip(H)) || H <- string:tokens(binary_to_list(BadHostnames), ",")]
    end,
    
    Messages = [#{<<"role">> => <<"user">>, <<"content">> => Query}],
    
    Opts = #{
        reasoning_effort => ReasoningEffort,
        max_returned_urls => MaxReturnedUrls,
        boost_hostnames => BoostList,
        bad_hostnames => BadList
    },
    
    case jina_client:deep_search(Messages, Opts) of
        {ok, Result} ->
            FormattedResult = format_deep_search_result(Result),
            {ok, #{
                <<"content">> => [#{
                    <<"type">> => <<"text">>,
                    <<"text">> => FormattedResult
                }]
            }};
        {error, no_api_key} ->
            {error, #{
                <<"code">> => <<"JINA_API_KEY_MISSING">>,
                <<"message">> => <<"JINA_API_KEY environment variable not set. Get your free key at: https://jina.ai/?sui=apikey">>
            }};
        {error, Reason} ->
            {error, #{
                <<"code">> => <<"JINA_DEEP_SEARCH_ERROR">>,
                <<"message">> => format_error(Reason)
            }}
    end.

%%====================================================================
%% Formatting Functions
%%====================================================================

format_search_results(Results) ->
    FormattedList = [format_single_search_result(I, Result) || {I, Result} <- lists:zip(lists:seq(1, length(Results)), Results)],
    iolist_to_binary(string:join(FormattedList, "\n\n")).

format_single_search_result(Index, #{title := Title, url := Url, content := Content}) ->
    TruncatedContent = case byte_size(Content) > 300 of
        true -> <<(binary:part(Content, 0, 300))/binary, "...">>;
        false -> Content
    end,
    io_lib:format("~p. ~s~nURL: ~s~n~s", [Index, Title, Url, TruncatedContent]);
format_single_search_result(Index, Other) ->
    io_lib:format("~p. ~p", [Index, Other]).

format_reader_result(#{title := Title, content := Content, links := Links, images := Images}) ->
    LinksSection = case maps:size(Links) of
        0 -> "";
        _ -> 
            LinksList = [io_lib:format("- ~s: ~s", [Text, Url]) || {Text, Url} <- maps:to_list(Links)],
            io_lib:format("~n~nLinks:~n~s", [string:join(LinksList, "\n")])
    end,
    
    ImagesSection = case maps:size(Images) of
        0 -> "";
        _ ->
            ImagesList = [io_lib:format("- ~s: ~s", [Alt, Url]) || {Alt, Url} <- maps:to_list(Images)],
            io_lib:format("~n~nImages:~n~s", [string:join(ImagesList, "\n")])
    end,
    
    iolist_to_binary(io_lib:format("Title: ~s~n~nContent:~n~s~s~s", [Title, Content, LinksSection, ImagesSection]));
format_reader_result(#{content := Content}) ->
    Content;
format_reader_result(Other) ->
    io_lib:format("~p", [Other]).

format_fact_check_result(#{factuality_score := Score, references := References, analysis := Analysis}) ->
    RefSection = case length(References) of
        0 -> "";
        _ ->
            RefList = [io_lib:format("- ~p", [Ref]) || Ref <- References],
            io_lib:format("~n~nReferences:~n~s", [string:join(RefList, "\n")])
    end,
    
    iolist_to_binary(io_lib:format("Factuality Score: ~.2f~n~nAnalysis:~n~s~s", [Score, Analysis, RefSection])).

format_rerank_results(Results, OriginalDocs) ->
    FormattedList = [format_single_rerank_result(I, Result, OriginalDocs) || {I, Result} <- lists:zip(lists:seq(1, length(Results)), Results)],
    iolist_to_binary(string:join(FormattedList, "\n\n")).

format_single_rerank_result(Rank, #{score := Score, index := Index, document := Doc}, OriginalDocs) ->
    Document = case Doc of
        undefined when Index < length(OriginalDocs) ->
            lists:nth(Index + 1, OriginalDocs);
        undefined ->
            "Document not found";
        _ ->
            binary_to_list(Doc)
    end,
    io_lib:format("Rank ~p (Score: ~.4f):~n~s", [Rank, Score, Document]);
format_single_rerank_result(Rank, Other, _) ->
    io_lib:format("Rank ~p: ~p", [Rank, Other]).

format_classify_results(Classifications, OriginalInputs) ->
    FormattedList = [format_single_classify_result(I, Class, OriginalInputs) || {I, Class} <- lists:zip(lists:seq(1, length(Classifications)), Classifications)],
    iolist_to_binary(string:join(FormattedList, "\n\n")).

format_single_classify_result(Index, #{prediction := Prediction, score := Score, predictions := Predictions}, OriginalInputs) ->
    Input = case Index =< length(OriginalInputs) of
        true ->
            InputText = lists:nth(Index, OriginalInputs),
            case length(InputText) > 100 of
                true -> string:substr(InputText, 1, 100) ++ "...";
                false -> InputText
            end;
        false ->
            "Unknown input"
    end,
    
    PredictionsText = case length(Predictions) of
        0 -> "";
        _ ->
            PredList = [io_lib:format("  - ~s: ~.4f", [maps:get(<<"label">>, P, ""), maps:get(<<"score">>, P, 0.0)]) || P <- Predictions],
            io_lib:format("~nAll classifications:~n~s", [string:join(PredList, "\n")])
    end,
    
    io_lib:format("Input ~p: ~s~nPrediction: ~s (Score: ~.4f)~s", [Index, Input, Prediction, Score, PredictionsText]);
format_single_classify_result(Index, Other, _) ->
    io_lib:format("Input ~p: ~p", [Index, Other]).

format_segment_result(#{num_tokens := NumTokens, chunks := Chunks, tokens := Tokens}) ->
    ChunksSection = case length(Chunks) of
        0 -> "";
        _ ->
            ChunkList = [io_lib:format("Chunk ~p:~n~s", [I, Chunk]) || {I, Chunk} <- lists:zip(lists:seq(1, length(Chunks)), Chunks)],
            io_lib:format("~n~nChunks:~n~s", [string:join(ChunkList, "\n\n")])
    end,
    
    TokensSection = case length(Tokens) of
        0 -> "";
        N when N > 0 ->
            % Show limited token information for brevity
            TokenSample = lists:sublist(Tokens, 3),
            TokenInfo = [io_lib:format("Group ~p: [token info...]", [I]) || I <- lists:seq(1, length(TokenSample))],
            ExtraMsg = case N > 3 of
                true -> io_lib:format("~n... and ~p more token groups", [N - 3]);
                false -> ""
            end,
            io_lib:format("~n~nTokens information:~n~s~s", [string:join(TokenInfo, "\n"), ExtraMsg])
    end,
    
    iolist_to_binary(io_lib:format("Total tokens: ~p~s~s", [NumTokens, ChunksSection, TokensSection])).

format_deep_search_result(#{content := Content, sources := Sources}) ->
    SourcesSection = case length(Sources) of
        0 -> "";
        _ ->
            SourceList = [format_source(I, Source) || {I, Source} <- lists:zip(lists:seq(1, length(Sources)), Sources)],
            io_lib:format("~n~nSources:~n~s", [string:join(SourceList, "\n")])
    end,
    
    iolist_to_binary(io_lib:format("~s~s", [Content, SourcesSection]));
format_deep_search_result(Other) ->
    io_lib:format("~p", [Other]).

format_source(Index, #{<<"url">> := Url, <<"title">> := Title}) ->
    io_lib:format("[~p] ~s~n    URL: ~s", [Index, Title, Url]);
format_source(Index, #{<<"url">> := Url}) ->
    io_lib:format("[~p] ~s", [Index, Url]);
format_source(Index, Other) ->
    io_lib:format("[~p] ~p", [Index, Other]).

format_error(#{status_code := Code, reason := Reason, body := Body}) ->
    io_lib:format("HTTP ~p: ~s - ~s", [Code, Reason, Body]);
format_error(#{reason := Reason}) ->
    io_lib:format("~p", [Reason]);
format_error(Other) ->
    io_lib:format("~p", [Other]).