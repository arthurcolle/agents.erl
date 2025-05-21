%% openai_completions.erl
%% OpenAI Completions API client
-module(openai_completions).
-behaviour(gen_server).

%% API exports
-export([
    start_link/1,
    create_completion/3
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

%% Start the completions client
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%% Create a completion
create_completion(Model, Prompt, Options) ->
    Timeout = maps:get(timeout, Options, ?DEFAULT_TIMEOUT),
    gen_server:call(?SERVER, {create_completion, Model, Prompt, Options}, Timeout).

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

handle_call({create_completion, Model, _Prompt, _Options}, _From, State) ->
    % For now, just return a mock response
    % In a real implementation, this would make an HTTP request to the OpenAI API
    Response = #{
        <<"id">> => <<"mock-completion-id">>,
        <<"object">> => <<"text_completion">>,
        <<"created">> => os:system_time(second),
        <<"model">> => Model,
        <<"choices">> => [
            #{
                <<"text">> => <<"This is a mock response from the OpenAI completions API.">>,
                <<"index">> => 0,
                <<"finish_reason">> => <<"stop">>
            }
        ],
        <<"usage">> => #{
            <<"prompt_tokens">> => 10,
            <<"completion_tokens">> => 15,
            <<"total_tokens">> => 25
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