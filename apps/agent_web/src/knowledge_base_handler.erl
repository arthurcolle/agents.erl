-module(knowledge_base_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path_info(Req0),
    handle_request(Method, Path, Req0, State).

%% GET /api/knowledge - List all knowledge domains
handle_request(<<"GET">>, [], Req0, State) ->
    case list_available_domains() of
        {ok, Domains} ->
            Response = jsx:encode(#{domains => Domains}),
            Req = cowboy_req:reply(200, #{
                <<"content-type">> => <<"application/json">>
            }, Response, Req0);
        {error, Reason} ->
            Req = cowboy_req:reply(500, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{error => Reason}), Req0)
    end,
    {ok, Req, State};

%% GET /api/knowledge/:domain - Get knowledge for a specific domain
handle_request(<<"GET">>, [Domain], Req0, State) ->
    case get_domain_knowledge(Domain, "index") of
        {ok, Knowledge} ->
            Response = jsx:encode(#{
                domain => Domain,
                knowledge => list_to_binary(Knowledge)
            }),
            Req = cowboy_req:reply(200, #{
                <<"content-type">> => <<"application/json">>
            }, Response, Req0);
        {error, topic_not_found} ->
            % Try to list all files in the domain instead
            case list_domain_contents(Domain) of
                {ok, Contents} ->
                    Response = jsx:encode(#{
                        domain => Domain,
                        contents => Contents
                    }),
                    Req = cowboy_req:reply(200, #{
                        <<"content-type">> => <<"application/json">>
                    }, Response, Req0);
                {error, Reason} ->
                    Req = cowboy_req:reply(404, #{
                        <<"content-type">> => <<"application/json">>
                    }, jsx:encode(#{error => Reason}), Req0)
            end;
        {error, Reason} ->
            Req = cowboy_req:reply(500, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{error => Reason}), Req0)
    end,
    {ok, Req, State};

%% GET /api/knowledge/:domain/:topic - Get specific topic knowledge
handle_request(<<"GET">>, [Domain, Topic], Req0, State) ->
    case get_domain_knowledge(Domain, binary_to_list(Topic)) of
        {ok, Knowledge} ->
            Response = jsx:encode(#{
                domain => Domain,
                topic => Topic,
                knowledge => list_to_binary(Knowledge)
            }),
            Req = cowboy_req:reply(200, #{
                <<"content-type">> => <<"application/json">>
            }, Response, Req0);
        {error, Reason} ->
            Req = cowboy_req:reply(404, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{error => Reason}), Req0)
    end,
    {ok, Req, State};

%% POST /api/knowledge/search - Search knowledge base
handle_request(<<"POST">>, [<<"search">>], Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    case jsx:decode(Body, [return_maps]) of
        #{<<"domain">> := Domain, <<"query">> := Query} ->
            % Perform async search
            Ref = make_ref(),
            Callback = fun(Results) ->
                self() ! {search_results, Ref, Results}
            end,
            search_knowledge_base(Domain, binary_to_list(Query), Callback),
            
            % Wait for results with timeout
            receive
                {search_results, Ref, Results} ->
                    Response = jsx:encode(#{
                        domain => Domain,
                        query => Query,
                        results => Results
                    }),
                    Req = cowboy_req:reply(200, #{
                        <<"content-type">> => <<"application/json">>
                    }, Response, Req1)
            after 5000 ->
                Req = cowboy_req:reply(504, #{
                    <<"content-type">> => <<"application/json">>
                }, jsx:encode(#{error => <<"Search timeout">>}), Req1)
            end;
        _ ->
            Req = cowboy_req:reply(400, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{error => <<"Invalid request body">>}), Req1)
    end,
    {ok, Req, State};

%% POST /api/knowledge/:domain - Add new knowledge to domain
handle_request(<<"POST">>, [Domain], Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    case jsx:decode(Body, [return_maps]) of
        #{<<"content">> := Content} ->
            update_knowledge_base(Domain, Content),
            Response = jsx:encode(#{
                status => <<"Knowledge added">>,
                domain => Domain
            }),
            Req = cowboy_req:reply(201, #{
                <<"content-type">> => <<"application/json">>
            }, Response, Req1);
        _ ->
            Req = cowboy_req:reply(400, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{error => <<"Invalid request body">>}), Req1)
    end,
    {ok, Req, State};

%% POST /api/knowledge/ingest - Trigger knowledge ingestion
handle_request(<<"POST">>, [<<"ingest">>], Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    case jsx:decode(Body, [return_maps]) of
        #{<<"sources">> := Sources} ->
            % Convert JSON sources to Erlang records
            ErlangSources = convert_sources_to_records(Sources),
            ingest_from_sources(ErlangSources),
            Response = jsx:encode(#{
                status => <<"Ingestion started">>,
                sources_count => length(ErlangSources)
            }),
            Req = cowboy_req:reply(202, #{
                <<"content-type">> => <<"application/json">>
            }, Response, Req1);
        _ ->
            Req = cowboy_req:reply(400, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{error => <<"Invalid request body">>}), Req1)
    end,
    {ok, Req, State};

%% GET /api/knowledge/status - Get ingestion status
handle_request(<<"GET">>, [<<"status">>], Req0, State) ->
    case get_ingestion_status() of
        {ok, Status} ->
            Response = jsx:encode(Status),
            Req = cowboy_req:reply(200, #{
                <<"content-type">> => <<"application/json">>
            }, Response, Req0);
        {error, Reason} ->
            Req = cowboy_req:reply(500, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{error => Reason}), Req0)
    end,
    {ok, Req, State};

%% POST /api/knowledge/discover - Trigger domain discovery
handle_request(<<"POST">>, [<<"discover">>], Req0, State) ->
    case discover_new_domains() of
        {ok, NewDomains} ->
            Response = jsx:encode(#{
                status => <<"Discovery completed">>,
                new_domains => NewDomains
            }),
            Req = cowboy_req:reply(200, #{
                <<"content-type">> => <<"application/json">>
            }, Response, Req0);
        {error, Reason} ->
            Req = cowboy_req:reply(500, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{error => Reason}), Req0)
    end,
    {ok, Req, State};

%% Catch-all for unsupported methods/paths
handle_request(_, _, Req0, State) ->
    Req = cowboy_req:reply(404, #{}, Req0),
    {ok, Req, State}.

%% Helper functions

list_domain_contents(Domain) ->
    DomainPath = filename:join(["/Users/agent/agents.erl/knowledge_bases", binary_to_list(Domain)]),
    case file:list_dir(DomainPath) of
        {ok, Files} ->
            % Filter and process files
            KnowledgeFiles = [F || F <- Files, 
                             filename:extension(F) =:= ".json" orelse
                             filename:extension(F) =:= ".md" orelse
                             filename:extension(F) =:= ".txt"],
            
            % Read preview of each file
            Contents = lists:map(fun(File) ->
                FilePath = filename:join(DomainPath, File),
                case file:read_file(FilePath) of
                    {ok, FileContent} ->
                        Preview = extract_preview(FileContent, File),
                        #{
                            file => list_to_binary(File),
                            preview => Preview,
                            size => byte_size(FileContent),
                            type => get_file_type(File)
                        };
                    {error, _} ->
                        #{
                            file => list_to_binary(File),
                            error => <<"Could not read file">>
                        }
                end
            end, KnowledgeFiles),
            
            {ok, Contents};
        {error, Reason} ->
            {error, atom_to_binary(Reason, utf8)}
    end.

extract_preview(Content, File) ->
    case filename:extension(File) of
        ".json" ->
            % For JSON files, try to decode and show structure
            try
                Decoded = jsx:decode(Content, [return_maps]),
                case Decoded of
                    #{<<"content">> := ContentText} when is_binary(ContentText) ->
                        % Extract first 200 characters of content
                        Preview = binary:part(ContentText, 0, min(200, byte_size(ContentText))),
                        <<Preview/binary, <<"...">>/binary>>;
                    _ ->
                        % Show keys if it's a map
                        Keys = case Decoded of
                            Map when is_map(Map) -> maps:keys(Map);
                            _ -> [<<"unknown_structure">>]
                        end,
                        iolist_to_binary(io_lib:format("JSON with keys: ~p", [Keys]))
                end
            catch
                _:_ ->
                    <<"Invalid JSON file">>
            end;
        _ ->
            % For text files, show first 200 characters
            Preview = binary:part(Content, 0, min(200, byte_size(Content))),
            <<Preview/binary, <<"...">>/binary>>
    end.

get_file_type(File) ->
    case filename:extension(File) of
        ".json" -> <<"json">>;
        ".md" -> <<"markdown">>;
        ".txt" -> <<"text">>;
        _ -> <<"unknown">>
    end.

convert_sources_to_records(Sources) ->
    lists:map(fun(Source) ->
        Type = binary_to_atom(maps:get(<<"type">>, Source, <<"web">>), utf8),
        Url = binary_to_list(maps:get(<<"url">>, Source, <<"">>)),
        Domain = binary_to_atom(maps:get(<<"domain">>, Source, <<"general">>), utf8),
        Priority = maps:get(<<"priority">>, Source, 5),
        
        % Create a simplified source record (matching the structure expected)
        #{
            type => Type,
            url => Url,
            domain => Domain,
            priority => Priority,
            frequency => daily,
            last_update => undefined,
            quality_score => 0.5
        }
    end, Sources).
%% Simple stub implementations for knowledge base functions
list_available_domains() ->
    case file:list_dir("knowledge_bases") of
        {ok, Files} ->
            Domains = [list_to_binary(filename:basename(F, ".json")) || F <- Files, 
                      filename:extension(F) =:= ".json"],
            {ok, Domains};
        {error, _} ->
            {ok, []}
    end.

get_domain_knowledge(Domain, _Topic) ->
    DomainFile = "knowledge_bases/" ++ binary_to_list(Domain) ++ ".json",
    case file:read_file(DomainFile) of
        {ok, Content} ->
            try
                Parsed = jsx:decode(Content, [return_maps]),
                ContentText = maps:get(<<"content">>, Parsed, Content),
                {ok, binary_to_list(ContentText)}
            catch
                _:_ -> {ok, binary_to_list(Content)}
            end;
        {error, _} ->
            {error, topic_not_found}
    end.

search_knowledge_base(Domain, Query, Callback) ->
    % Simple search implementation
    Results = [#{
        file => <<"sample_result.json">>,
        content_preview => <<"Search results for: ", (list_to_binary(Query))/binary>>,
        relevance_score => 0.8
    }],
    Callback(Results).

update_knowledge_base(_Domain, _Content) ->
    ok.

ingest_from_sources(_Sources) ->
    ok.

get_ingestion_status() ->
    {ok, #{
        queue_length => 0,
        active_jobs => 0,
        domain_coverage => #{},
        last_discovery => undefined
    }}.

discover_new_domains() ->
    {ok, []}.
