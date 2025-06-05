%%%-------------------------------------------------------------------
%%% @doc OpenAI Self-Scaffolding System
%%% Automatically generates Erlang modules based on OpenAI API structure
%%% @end
%%%-------------------------------------------------------------------
-module(openai_self_scaffold).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    scaffold_from_spec/1,
    generate_module/2,
    generate_all_modules/0,
    get_api_structure/0,
    update_cost_tracking/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    api_spec = #{},
    generated_modules = [],
    cost_config = #{}
}).

%% OpenAI API Structure (based on latest documentation)
-define(OPENAI_API_SPEC, #{
    base_url => "https://api.openai.com/v1",
    endpoints => #{
        %% Chat Completions
        chat_completions => #{
            path => "/chat/completions",
            method => post,
            description => "Creates a completion for the chat message",
            parameters => #{
                model => #{type => string, required => true},
                messages => #{type => array, required => true},
                temperature => #{type => float, default => 1.0},
                max_tokens => #{type => integer},
                stream => #{type => boolean, default => false},
                tools => #{type => array},
                tool_choice => #{type => string}
            }
        },
        
        %% Responses API (new)
        responses => #{
            path => "/responses",
            method => post,
            description => "Creates a response using the Responses API",
            parameters => #{
                model => #{type => string, required => true},
                input => #{type => array, required => true},
                instructions => #{type => string},
                tools => #{type => array},
                parallel_tool_calls => #{type => boolean, default => true},
                temperature => #{type => float, default => 0.7}
            }
        },
        
        %% Embeddings
        embeddings => #{
            path => "/embeddings",
            method => post,
            description => "Creates an embedding vector",
            parameters => #{
                model => #{type => string, required => true},
                input => #{type => string_or_array, required => true},
                encoding_format => #{type => string, default => "float"}
            }
        },
        
        %% Images
        images_generations => #{
            path => "/images/generations",
            method => post,
            description => "Creates an image given a prompt",
            parameters => #{
                prompt => #{type => string, required => true},
                model => #{type => string, default => "dall-e-3"},
                n => #{type => integer, default => 1},
                size => #{type => string, default => "1024x1024"},
                quality => #{type => string, default => "standard"}
            }
        },
        
        %% Audio
        audio_speech => #{
            path => "/audio/speech",
            method => post,
            description => "Generates audio from text",
            parameters => #{
                model => #{type => string, required => true},
                input => #{type => string, required => true},
                voice => #{type => string, required => true},
                response_format => #{type => string, default => "mp3"},
                speed => #{type => float, default => 1.0}
            }
        },
        
        audio_transcriptions => #{
            path => "/audio/transcriptions",
            method => post,
            description => "Transcribes audio into text",
            parameters => #{
                file => #{type => file, required => true},
                model => #{type => string, required => true},
                language => #{type => string},
                prompt => #{type => string},
                response_format => #{type => string, default => "json"}
            }
        },
        
        %% Assistants
        assistants => #{
            path => "/assistants",
            method => post,
            description => "Create an assistant",
            parameters => #{
                model => #{type => string, required => true},
                name => #{type => string},
                description => #{type => string},
                instructions => #{type => string},
                tools => #{type => array},
                file_ids => #{type => array}
            }
        },
        
        %% Fine-tuning
        fine_tuning_jobs => #{
            path => "/fine-tuning/jobs",
            method => post,
            description => "Creates a fine-tuning job",
            parameters => #{
                training_file => #{type => string, required => true},
                model => #{type => string, required => true},
                hyperparameters => #{type => object},
                suffix => #{type => string},
                validation_file => #{type => string}
            }
        },
        
        %% Batch
        batches => #{
            path => "/batches",
            method => post,
            description => "Create large batches of API requests",
            parameters => #{
                input_file_id => #{type => string, required => true},
                endpoint => #{type => string, required => true},
                completion_window => #{type => string, required => true}
            }
        },
        
        %% Moderation
        moderations => #{
            path => "/moderations",
            method => post,
            description => "Classifies if text violates content policy",
            parameters => #{
                input => #{type => string_or_array, required => true},
                model => #{type => string, default => "omni-moderation-latest"}
            }
        }
    },
    
    %% Models with latest pricing
    models => #{
        %% GPT-4.1 Series
        <<"gpt-4.1">> => #{
            name => "GPT-4.1",
            type => chat,
            context_window => 1047576,
            max_output => 32768,
            pricing => #{
                input => 2.00,      % per 1M tokens
                cached_input => 0.50,
                output => 8.00
            },
            capabilities => [chat, functions, vision],
            intelligence => expert,
            speed => medium
        },
        
        <<"gpt-4.1-mini">> => #{
            name => "GPT-4.1 mini",
            type => chat,
            context_window => 128000,
            max_output => 16384,
            pricing => #{
                input => 0.10,
                cached_input => 0.025,
                output => 0.30
            },
            capabilities => [chat, functions],
            intelligence => standard,
            speed => very_fast
        },
        
        <<"gpt-4.1-nano">> => #{
            name => "GPT-4.1 nano",
            type => chat,
            context_window => 128000,
            max_output => 8192,
            pricing => #{
                input => 0.05,
                cached_input => 0.0125,
                output => 0.15
            },
            capabilities => [chat],
            intelligence => basic,
            speed => ultra_fast
        },
        
        %% GPT-4o Series
        <<"gpt-4o">> => #{
            name => "GPT-4o",
            type => chat,
            context_window => 128000,
            max_output => 16384,
            pricing => #{
                input => 2.50,
                cached_input => 1.25,
                output => 10.00
            },
            capabilities => [chat, functions, vision, audio],
            intelligence => advanced,
            speed => fast
        },
        
        <<"gpt-4o-mini">> => #{
            name => "GPT-4o mini",
            type => chat,
            context_window => 128000,
            max_output => 16384,
            pricing => #{
                input => 0.15,
                cached_input => 0.075,
                output => 0.60
            },
            capabilities => [chat, functions, vision],
            intelligence => standard,
            speed => very_fast
        },
        
        %% Reasoning Models
        <<"o4-mini">> => #{
            name => "o4-mini",
            type => reasoning,
            context_window => 128000,
            max_output => 65536,
            pricing => #{
                input => 3.00,
                cached_input => 0.75,
                output => 12.00
            },
            capabilities => [reasoning, chat],
            intelligence => advanced,
            speed => fast
        },
        
        <<"o3-mini">> => #{
            name => "o3-mini",
            type => reasoning,
            context_window => 128000,
            max_output => 65536,
            pricing => #{
                input => 1.10,
                cached_input => 0.275,
                output => 4.40
            },
            capabilities => [reasoning, chat],
            intelligence => advanced,
            speed => medium
        },
        
        %% Embedding Models
        <<"text-embedding-3-small">> => #{
            name => "text-embedding-3-small",
            type => embedding,
            dimensions => 1536,
            pricing => #{
                usage => 0.02  % per 1M tokens
            },
            max_input => 8191
        },
        
        <<"text-embedding-3-large">> => #{
            name => "text-embedding-3-large",
            type => embedding,
            dimensions => 3072,
            pricing => #{
                usage => 0.13  % per 1M tokens
            },
            max_input => 8191
        },
        
        %% Image Models
        <<"dall-e-3">> => #{
            name => "DALL-E 3",
            type => image_generation,
            pricing => #{
                standard_1024x1024 => 0.040,
                standard_1024x1792 => 0.080,
                standard_1792x1024 => 0.080,
                hd_1024x1024 => 0.080,
                hd_1024x1792 => 0.120,
                hd_1792x1024 => 0.120
            }
        },
        
        %% Audio Models
        <<"whisper-1">> => #{
            name => "Whisper",
            type => audio_transcription,
            pricing => #{
                usage => 0.006  % per minute
            }
        },
        
        <<"tts-1">> => #{
            name => "TTS",
            type => text_to_speech,
            pricing => #{
                usage => 15.00  % per 1M characters
            }
        },
        
        <<"tts-1-hd">> => #{
            name => "TTS HD",
            type => text_to_speech,
            pricing => #{
                usage => 30.00  % per 1M characters
            }
        }
    },
    
    %% Rate limits by tier
    rate_limits => #{
        tier_1 => #{
            <<"gpt-4.1">> => #{rpm => 500, tpm => 30000},
            <<"gpt-4o">> => #{rpm => 500, tpm => 30000},
            <<"gpt-4.1-mini">> => #{rpm => 500, tpm => 200000},
            <<"o4-mini">> => #{rpm => 30, tpm => 150000}
        },
        tier_2 => #{
            <<"gpt-4.1">> => #{rpm => 5000, tpm => 450000},
            <<"gpt-4o">> => #{rpm => 5000, tpm => 450000},
            <<"gpt-4.1-mini">> => #{rpm => 5000, tpm => 2000000},
            <<"o4-mini">> => #{rpm => 100, tpm => 450000}
        },
        tier_3 => #{
            <<"gpt-4.1">> => #{rpm => 5000, tpm => 800000},
            <<"gpt-4o">> => #{rpm => 5000, tpm => 800000},
            <<"gpt-4.1-mini">> => #{rpm => 5000, tpm => 4000000}
        }
    }
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

scaffold_from_spec(Spec) ->
    gen_server:call(?MODULE, {scaffold, Spec}).

generate_module(EndpointName, EndpointSpec) ->
    gen_server:call(?MODULE, {generate_module, EndpointName, EndpointSpec}).

generate_all_modules() ->
    gen_server:call(?MODULE, generate_all, 30000).

get_api_structure() ->
    gen_server:call(?MODULE, get_api_structure).

update_cost_tracking(Models) ->
    gen_server:call(?MODULE, {update_cost_tracking, Models}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{api_spec = ?OPENAI_API_SPEC}}.

handle_call({scaffold, NewSpec}, _From, State) ->
    MergedSpec = maps:merge(State#state.api_spec, NewSpec),
    {reply, ok, State#state{api_spec = MergedSpec}};

handle_call({generate_module, Name, Spec}, _From, State) ->
    Result = generate_single_module(Name, Spec),
    case Result of
        {ok, ModuleName} ->
            NewModules = [ModuleName | State#state.generated_modules],
            {reply, Result, State#state{generated_modules = NewModules}};
        Error ->
            {reply, Error, State}
    end;

handle_call(generate_all, _From, State) ->
    Endpoints = maps:get(endpoints, State#state.api_spec, #{}),
    Results = maps:map(fun(Name, Spec) ->
        generate_single_module(Name, Spec)
    end, Endpoints),
    
    %% Also generate cost tracking updates
    Models = maps:get(models, State#state.api_spec, #{}),
    update_cost_tracker_models(Models),
    
    {reply, {ok, Results}, State};

handle_call(get_api_structure, _From, State) ->
    {reply, {ok, State#state.api_spec}, State};

handle_call({update_cost_tracking, Models}, _From, State) ->
    update_cost_tracker_models(Models),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

generate_single_module(Name, Spec) ->
    ModuleName = list_to_atom("openai_" ++ atom_to_list(Name)),
    
    %% Generate module content
    ModuleContent = generate_module_content(ModuleName, Name, Spec),
    
    %% Write to file
    FileName = "apps/openai/src/" ++ atom_to_list(ModuleName) ++ ".erl",
    case file:write_file(FileName, ModuleContent) of
        ok ->
            io:format("Generated module: ~s~n", [FileName]),
            {ok, ModuleName};
        {error, Reason} ->
            {error, {write_failed, Reason}}
    end.

generate_module_content(ModuleName, EndpointName, Spec) ->
    Path = maps:get(path, Spec, ""),
    Method = maps:get(method, Spec, post),
    Description = maps:get(description, Spec, ""),
    Parameters = maps:get(parameters, Spec, #{}),
    
    %% Generate parameter validation
    RequiredParams = [P || {P, #{required := true}} <- maps:to_list(Parameters)],
    OptionalParams = [P || {P, #{required := R}} <- maps:to_list(Parameters), R =/= true],
    
    io_lib:format(
"%%%-------------------------------------------------------------------
%%% @doc ~s
%%% ~s
%%% Auto-generated by openai_self_scaffold
%%% @end
%%%-------------------------------------------------------------------
-module(~w).

-export([
    request/1,
    request/2,
    validate_params/1
]).

%% @doc Make a request to the ~s endpoint
request(Params) ->
    request(Params, #{}).

request(Params, Options) ->
    case validate_params(Params) of
        ok ->
            Path = ~p,
            Method = ~w,
            
            %% Track costs if applicable
            track_costs(Params),
            
            %% Make the API request
            openai_api:request(Method, Path, Params, Options);
        {error, Reason} ->
            {error, {validation_failed, Reason}}
    end.

%% @doc Validate parameters
validate_params(Params) ->
    RequiredParams = ~w,
    OptionalParams = ~w,
    
    %% Check required parameters
    case check_required_params(Params, RequiredParams) of
        ok ->
            %% Validate parameter types
            validate_param_types(Params);
        Error ->
            Error
    end.

%% Internal functions

check_required_params(_Params, []) ->
    ok;
check_required_params(Params, [Required | Rest]) ->
    case maps:is_key(Required, Params) of
        true ->
            check_required_params(Params, Rest);
        false ->
            {error, {missing_required_param, Required}}
    end.

validate_param_types(Params) ->
    %% TODO: Implement type validation based on spec
    ok.

track_costs(Params) ->
    case maps:get(model, Params, undefined) of
        undefined -> ok;
        Model ->
            %% Cost tracking will be handled by the request
            ok
    end.
",
    [ModuleName, Description, ModuleName, EndpointName, Path, Method, 
     RequiredParams, OptionalParams]).

update_cost_tracker_models(Models) ->
    %% Generate updated cost tracking configuration
    CostConfig = maps:fold(fun(ModelId, ModelSpec, Acc) ->
        case maps:get(pricing, ModelSpec, undefined) of
            undefined -> Acc;
            Pricing ->
                maps:put(ModelId, Pricing, Acc)
        end
    end, #{}, Models),
    
    %% Write to cost tracker config file
    ConfigContent = io_lib:format(
"%%% Auto-generated cost configuration
%%% Generated by openai_self_scaffold
-module(cost_tracker_config).
-export([get_model_pricing/0]).

get_model_pricing() ->
    ~p.
", [CostConfig]),
    
    file:write_file("apps/openai/src/cost_tracker_config.erl", ConfigContent),
    io:format("Updated cost tracking configuration~n").