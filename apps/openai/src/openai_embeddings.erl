%% openai_embeddings.erl
%% OpenAI Embeddings API client
-module(openai_embeddings).
-behaviour(gen_server).

%% API exports
-export([
    start_link/1,
    create_embedding/3
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

-record(state, {
    api_key = undefined :: binary() | undefined,
    base_url = <<"https://api.openai.com/v1">> :: binary(),
    rate_limiter = undefined :: pid() | undefined,
    options = #{} :: map()
}).

%% Public API

%% Start the embeddings client
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%% Create an embedding
create_embedding(Model, Input, Options) ->
    Timeout = maps:get(timeout, Options, ?DEFAULT_TIMEOUT),
    gen_server:call(?SERVER, {create_embedding, Model, Input, Options}, Timeout).

%% gen_server callbacks

init(Options) ->
    % Get API key from environment
    ApiKey = case os:getenv("OPENAI_API_KEY") of
        false -> 
            % For testing, provide a mock API key
            <<"sk_test">>;
        Key -> 
            list_to_binary(Key)
    end,
    
    % Set up rate limiter
    RateLimiter = case whereis(openai_rate_limiter) of
        undefined -> undefined;
        Pid -> Pid
    end,
    
    {ok, #state{
        api_key = ApiKey,
        rate_limiter = RateLimiter,
        options = Options
    }}.

handle_call({create_embedding, Model, _Input, _Options}, _From, State) ->
    % For now, just return a mock response with a 1536-dimensional vector
    % In a real implementation, this would make an HTTP request to the OpenAI API
    MockEmbedding = lists:map(fun(_) -> rand:uniform() * 0.01 end, lists:seq(1, 1536)),
    
    Response = #{
        <<"object">> => <<"list">>,
        <<"data">> => [
            #{
                <<"object">> => <<"embedding">>,
                <<"embedding">> => MockEmbedding,
                <<"index">> => 0
            }
        ],
        <<"model">> => Model,
        <<"usage">> => #{
            <<"prompt_tokens">> => 10,
            <<"total_tokens">> => 10
        }
    },
    {reply, {ok, Response}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.