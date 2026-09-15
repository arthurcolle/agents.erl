%% knowledge_base_retrieval.erl
%% Async knowledge base retrieval system for agent templates
-module(knowledge_base_retrieval).

-export([
    start_link/0,
    search_knowledge_base/3,
    index_knowledge_base/1,
    get_domain_knowledge/2,
    update_knowledge_base/2,
    list_available_domains/0
]).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(INDEX_TABLE, knowledge_base_index).

-record(state, {
    indexed_domains = #{},
    cache = #{},
    cache_ttl = 300000  % 5 minutes in milliseconds
}).

%% Public API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Search across knowledge base for a specific domain
search_knowledge_base(Domain, Query, _Callback) ->
    gen_server:call(?MODULE, {search, Domain, Query}).

%% Index a specific domain's knowledge base
index_knowledge_base(Domain) ->
    gen_server:cast(?MODULE, {index_domain, Domain}).

%% Get knowledge for a specific domain and topic
get_domain_knowledge(Domain, Topic) ->
    gen_server:call(?MODULE, {get_knowledge, Domain, Topic}).

%% Update knowledge base with new content
update_knowledge_base(Domain, Content) ->
    gen_server:cast(?MODULE, {update_knowledge, Domain, Content}).

%% List all available knowledge domains
list_available_domains() ->
    gen_server:call(?MODULE, list_domains).

%% Gen_server callbacks

init([]) ->
    % Create ETS table for indexing
    ets:new(?INDEX_TABLE, [named_table, public, {keypos, 1}]),
    
    % Queue indexing on the supervised server instead of creating an
    % unowned background process.
    self() ! index_all_domains,
    schedule_cache_cleanup(),
    
    {ok, #state{}}.

handle_call({get_knowledge, Domain, Topic}, _From, State) ->
    case get_cached_knowledge(Domain, Topic, State) of
        {ok, Knowledge} ->
            {reply, {ok, Knowledge}, State};
        cache_miss ->
            case load_domain_knowledge(Domain, Topic) of
                {ok, Knowledge} ->
                    NewState = cache_knowledge(Domain, Topic, Knowledge, State),
                    {reply, {ok, Knowledge}, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({search, Domain, Query}, _From, State) ->
    Result = perform_search(Domain, Query),
    {reply, {ok, Result}, State};

handle_call(list_domains, _From, State) ->
    Domains = list_knowledge_domains(),
    {reply, {ok, Domains}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({index_domain, Domain}, State) ->
    self() ! {index_domain, Domain},
    {noreply, State};

handle_cast({update_knowledge, Domain, Content}, State) ->
    update_domain_knowledge(Domain, Content),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(index_all_domains, State) ->
    index_all_domains(),
    {noreply, State};
handle_info({index_domain, Domain}, State) ->
    index_domain(Domain),
    {noreply, State};
handle_info({cache_cleanup}, State) ->
    NewState = cleanup_cache(State),
    schedule_cache_cleanup(),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

index_all_domains() ->
    BasePath = kb_base_path(),
    case file:list_dir(BasePath) of
        {ok, Entries} ->
            lists:foreach(fun(Entry) ->
                Path = filename:join(BasePath, Entry),
                case filelib:is_dir(Path) of
                    true ->
                        index_domain(Entry);
                    false ->
                        case is_knowledge_file(Entry) of
                            true ->
                                Domain = filename:basename(
                                    Entry,
                                    filename:extension(Entry)
                                ),
                                index_knowledge_file(Domain, Path);
                            false ->
                                ok
                        end
                end
            end, Entries);
        {error, _Reason} ->
            logger:warning("Knowledge base directory not found: ~s", [BasePath])
    end.

index_domain(Domain) ->
    BasePath = kb_base_path(),
    DomainName = normalize_path_component(Domain),
    DomainPath = filename:join(BasePath, DomainName),
    case file:list_dir(DomainPath) of
        {ok, Files} ->
            KnowledgeFiles = [F || F <- Files, filename:extension(F) =:= ".md" 
                                 orelse filename:extension(F) =:= ".txt"
                                 orelse filename:extension(F) =:= ".json"],
            lists:foreach(fun(File) ->
                index_knowledge_file(Domain, filename:join(DomainPath, File))
            end, KnowledgeFiles);
        {error, _Reason} ->
            index_flat_domain(BasePath, DomainName)
    end.

index_knowledge_file(Domain, FilePath) ->
    case file:read_file(FilePath) of
        {ok, Content} ->
            FileName = filename:basename(FilePath, filename:extension(FilePath)),
            Keywords = extract_keywords(Content),
            ets:insert(?INDEX_TABLE, {{Domain, FileName}, Keywords, FilePath}),
            ok;
        {error, _Reason} ->
            error_logger:warning_msg("Cannot read knowledge file: ~s~n", [FilePath])
    end.

extract_keywords(Content) ->
    % Simple keyword extraction - split on whitespace and punctuation
    Text = binary_to_list(Content),
    Words = string:tokens(string:to_lower(Text), " \t\n\r.,;:!?()[]{}\"'"),
    % Filter out common stop words and keep meaningful terms
    FilteredWords = [W || W <- Words, length(W) > 3],
    lists:usort(FilteredWords).

perform_search(Domain, Query) ->
    QueryTerms = string:tokens(string:to_lower(Query), " "),
    MatchingFiles = ets:foldl(fun({{D, FileName}, Keywords, FilePath}, Acc) ->
        case D of
            Domain ->
                Score = calculate_relevance_score(QueryTerms, Keywords),
                if Score > 0 ->
                    [{Score, FileName, FilePath} | Acc];
                true ->
                    Acc
                end;
            _ ->
                Acc
        end
    end, [], ?INDEX_TABLE),
    
    % Sort by relevance score (descending)
    SortedResults = lists:reverse(lists:keysort(1, MatchingFiles)),
    
    % Return top 5 results with content
    TopResults = lists:sublist(SortedResults, 5),
    lists:map(fun({Score, FileName, FilePath}) ->
        case file:read_file(FilePath) of
            {ok, Content} ->
                #{
                    file => FileName,
                    relevance_score => Score,
                    content_preview => extract_preview(Content, QueryTerms),
                    full_path => FilePath
                };
            {error, _} ->
                #{
                    file => FileName,
                    relevance_score => Score,
                    error => file_read_error
                }
        end
    end, TopResults).

calculate_relevance_score(QueryTerms, Keywords) ->
    Matches = [1 || QT <- QueryTerms, lists:member(QT, Keywords)],
    length(Matches).

extract_preview(Content, QueryTerms) ->
    Text = binary_to_list(Content),
    Lines = string:tokens(Text, "\n"),
    
    % Find lines containing query terms
    RelevantLines = lists:filter(fun(Line) ->
        LowerLine = string:to_lower(Line),
        lists:any(fun(Term) -> string:str(LowerLine, Term) > 0 end, QueryTerms)
    end, Lines),
    
    % Return first 3 relevant lines or first 3 lines if none match
    PreviewLines = case RelevantLines of
        [] -> lists:sublist(Lines, 3);
        _ -> lists:sublist(RelevantLines, 3)
    end,
    
    string:join(PreviewLines, "\n").

load_domain_knowledge(Domain, Topic) ->
    BasePath = kb_base_path(),
    DomainName = normalize_path_component(Domain),
    TopicName = normalize_path_component(Topic),
    FilePath = filename:join([BasePath, DomainName, TopicName ++ ".md"]),
    case file:read_file(FilePath) of
        {ok, Content} ->
            {ok, binary_to_list(Content)};
        {error, enoent} ->
            % Try .txt extension
            TxtPath = filename:join([BasePath, DomainName, TopicName ++ ".txt"]),
            case file:read_file(TxtPath) of
                {ok, Content} ->
                    {ok, binary_to_list(Content)};
                {error, _} ->
                    load_flat_domain(BasePath, DomainName)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

update_domain_knowledge(Domain, Content) ->
    % This would typically save new knowledge to the appropriate domain
    % For now, just log the update
    error_logger:info_msg("Knowledge update for domain ~s: ~p~n", [Domain, Content]).

list_knowledge_domains() ->
    BasePath = kb_base_path(),
    case file:list_dir(BasePath) of
        {ok, Entries} ->
            [
                domain_name(BasePath, Entry)
                || Entry <- Entries,
                   is_domain_entry(BasePath, Entry)
            ];
        {error, _} ->
            []
    end.

get_cached_knowledge(Domain, Topic, #state{cache = Cache, cache_ttl = TTL}) ->
    Key = {Domain, Topic},
    case maps:get(Key, Cache, undefined) of
        undefined ->
            cache_miss;
        {Knowledge, Timestamp} ->
            Now = erlang:system_time(millisecond),
            if (Now - Timestamp) < TTL ->
                {ok, Knowledge};
            true ->
                cache_miss
            end
    end.

cache_knowledge(Domain, Topic, Knowledge, #state{cache = Cache} = State) ->
    Key = {Domain, Topic},
    Timestamp = erlang:system_time(millisecond),
    NewCache = maps:put(Key, {Knowledge, Timestamp}, Cache),
    State#state{cache = NewCache}.

cleanup_cache(#state{cache = Cache, cache_ttl = TTL} = State) ->
    Now = erlang:system_time(millisecond),
    NewCache = maps:filter(fun(_, {_, Timestamp}) ->
        (Now - Timestamp) < TTL
    end, Cache),
    State#state{cache = NewCache}.

schedule_cache_cleanup() ->
    erlang:send_after(300000, self(), {cache_cleanup}).  % Every 5 minutes

kb_base_path() ->
    ConfiguredPath = application:get_env(
        agents,
        knowledge_base_dir,
        "knowledge_bases"
    ),
    filename:absname(ConfiguredPath).

is_knowledge_file(FileName) ->
    lists:member(filename:extension(FileName), [".md", ".txt", ".json"]).

is_domain_entry(BasePath, Entry) ->
    filelib:is_dir(filename:join(BasePath, Entry)) orelse
        is_knowledge_file(Entry).

domain_name(BasePath, Entry) ->
    case filelib:is_dir(filename:join(BasePath, Entry)) of
        true -> Entry;
        false -> filename:basename(Entry, filename:extension(Entry))
    end.

index_flat_domain(BasePath, DomainName) ->
    case find_flat_domain_file(BasePath, DomainName) of
        {ok, FilePath} ->
            index_knowledge_file(DomainName, FilePath);
        error ->
            logger:warning("Cannot index knowledge domain: ~s", [DomainName])
    end.

load_flat_domain(BasePath, DomainName) ->
    case find_flat_domain_file(BasePath, DomainName) of
        {ok, FilePath} ->
            case file:read_file(FilePath) of
                {ok, Content} -> {ok, binary_to_list(Content)};
                {error, Reason} -> {error, Reason}
            end;
        error ->
            {error, topic_not_found}
    end.

find_flat_domain_file(BasePath, DomainName) ->
    Candidates = [
        filename:join(BasePath, DomainName ++ Extension)
        || Extension <- [".json", ".md", ".txt"]
    ],
    case lists:dropwhile(fun(Path) -> not filelib:is_regular(Path) end, Candidates) of
        [FilePath | _] -> {ok, FilePath};
        [] -> error
    end.

normalize_path_component(Value) when is_binary(Value) ->
    binary_to_list(Value);
normalize_path_component(Value) when is_atom(Value) ->
    atom_to_list(Value);
normalize_path_component(Value) when is_list(Value) ->
    Value.
