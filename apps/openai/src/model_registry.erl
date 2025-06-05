%%%-------------------------------------------------------------------
%%% @doc Model Registry
%%% Complete registry of all available AI models with their capabilities
%%% @end
%%%-------------------------------------------------------------------
-module(model_registry).

-export([
    get_all_models/0,
    get_model_info/1,
    get_models_by_category/1,
    get_models_by_capability/1,
    validate_model/1,
    get_default_model/0,
    get_model_categories/0,
    get_model_capabilities/0
]).

-type model_category() :: flagship_chat | reasoning | cost_optimized | realtime | 
                         image_generation | text_to_speech | transcription | 
                         tool_specific | embeddings | moderation | legacy | base.

-type model_capability() :: chat | reasoning | audio_input | audio_output | 
                           image_generation | image_input | realtime | web_search |
                           computer_use | code_optimization | embeddings | moderation.

-type model_info() :: #{
    id := binary(),
    name := binary(),
    category := model_category(),
    capabilities := [model_capability()],
    description := binary(),
    provider := openai | anthropic,
    deprecated := boolean(),
    cost_tier := low | medium | high | very_high,
    speed_tier := slow | medium | fast | very_fast,
    intelligence_tier := basic | standard | advanced | expert
}.

%% @doc Get all available models
-spec get_all_models() -> [model_info()].
get_all_models() ->
    [
        %% Flagship Chat Models
        #{id => <<"gpt-4.1">>, 
          name => <<"GPT-4.1">>,
          category => flagship_chat,
          capabilities => [chat],
          description => <<"Flagship GPT model for complex tasks">>,
          provider => openai,
          deprecated => false,
          cost_tier => high,
          speed_tier => medium,
          intelligence_tier => expert},
          
        #{id => <<"gpt-4.1-2025-04-14">>, 
          name => <<"GPT-4.1">>,
          category => flagship_chat,
          capabilities => [chat],
          description => <<"Flagship GPT model for complex tasks (2025-04-14)">>,
          provider => openai,
          deprecated => false,
          cost_tier => high,
          speed_tier => medium,
          intelligence_tier => expert},
          
        #{id => <<"gpt-4.5-preview">>, 
          name => <<"GPT-4.5 Preview">>,
          category => flagship_chat,
          capabilities => [chat],
          description => <<"Next-generation preview model with enhanced capabilities">>,
          provider => openai,
          deprecated => false,
          cost_tier => very_high,
          speed_tier => medium,
          intelligence_tier => expert},
          
        #{id => <<"gpt-4.5-preview-2025-02-27">>, 
          name => <<"GPT-4.5 Preview">>,
          category => flagship_chat,
          capabilities => [chat],
          description => <<"Next-generation preview model (2025-02-27)">>,
          provider => openai,
          deprecated => false,
          cost_tier => very_high,
          speed_tier => medium,
          intelligence_tier => expert},
          
        #{id => <<"gpt-4o">>,
          name => <<"GPT-4o">>,
          category => flagship_chat,
          capabilities => [chat],
          description => <<"Fast, intelligent, flexible GPT model">>,
          provider => openai,
          deprecated => false,
          cost_tier => medium,
          speed_tier => fast,
          intelligence_tier => advanced},
          
        #{id => <<"gpt-4o-audio-preview">>,
          name => <<"GPT-4o Audio">>,
          category => flagship_chat,
          capabilities => [chat, audio_input, audio_output],
          description => <<"GPT-4o models capable of audio inputs and outputs">>,
          provider => openai,
          deprecated => false,
          cost_tier => high,
          speed_tier => medium,
          intelligence_tier => advanced},
          
        #{id => <<"chatgpt-4o-latest">>,
          name => <<"ChatGPT-4o">>,
          category => flagship_chat,
          capabilities => [chat],
          description => <<"GPT-4o model used in ChatGPT">>,
          provider => openai,
          deprecated => false,
          cost_tier => medium,
          speed_tier => fast,
          intelligence_tier => advanced},
          
        %% Reasoning Models
        #{id => <<"o4-mini">>,
          name => <<"o4-mini">>,
          category => reasoning,
          capabilities => [reasoning, chat],
          description => <<"Faster, more affordable reasoning model">>,
          provider => openai,
          deprecated => false,
          cost_tier => medium,
          speed_tier => fast,
          intelligence_tier => advanced},
          
        #{id => <<"o3">>,
          name => <<"o3">>,
          category => reasoning,
          capabilities => [reasoning, chat],
          description => <<"Our most powerful reasoning model">>,
          provider => openai,
          deprecated => false,
          cost_tier => very_high,
          speed_tier => slow,
          intelligence_tier => expert},
          
        #{id => <<"o3-mini">>,
          name => <<"o3-mini">>,
          category => reasoning,
          capabilities => [reasoning, chat],
          description => <<"A small model alternative to o3">>,
          provider => openai,
          deprecated => false,
          cost_tier => medium,
          speed_tier => medium,
          intelligence_tier => advanced},
          
        #{id => <<"o1">>,
          name => <<"o1">>,
          category => reasoning,
          capabilities => [reasoning, chat],
          description => <<"Previous full o-series reasoning model">>,
          provider => openai,
          deprecated => false,
          cost_tier => high,
          speed_tier => slow,
          intelligence_tier => expert},
          
        #{id => <<"o1-mini">>,
          name => <<"o1-mini">>,
          category => reasoning,
          capabilities => [reasoning, chat],
          description => <<"A small model alternative to o1">>,
          provider => openai,
          deprecated => true,
          cost_tier => medium,
          speed_tier => medium,
          intelligence_tier => standard},
          
        #{id => <<"o1-pro">>,
          name => <<"o1-pro">>,
          category => reasoning,
          capabilities => [reasoning, chat],
          description => <<"Version of o1 with more compute for better responses">>,
          provider => openai,
          deprecated => false,
          cost_tier => very_high,
          speed_tier => slow,
          intelligence_tier => expert},
          
        %% Cost-optimized Models
        #{id => <<"gpt-4.1-mini">>,
          name => <<"GPT-4.1 mini">>,
          category => cost_optimized,
          capabilities => [chat],
          description => <<"Balanced for intelligence, speed, and cost">>,
          provider => openai,
          deprecated => false,
          cost_tier => low,
          speed_tier => very_fast,
          intelligence_tier => standard},
          
        #{id => <<"gpt-4.1-nano">>,
          name => <<"GPT-4.1 nano">>,
          category => cost_optimized,
          capabilities => [chat],
          description => <<"Fastest, most cost-effective GPT-4.1 model">>,
          provider => openai,
          deprecated => false,
          cost_tier => low,
          speed_tier => very_fast,
          intelligence_tier => basic},
          
        #{id => <<"gpt-4o-mini">>,
          name => <<"GPT-4o mini">>,
          category => cost_optimized,
          capabilities => [chat],
          description => <<"Fast, affordable small model for focused tasks">>,
          provider => openai,
          deprecated => false,
          cost_tier => low,
          speed_tier => very_fast,
          intelligence_tier => standard},
          
        #{id => <<"gpt-4o-mini-audio-preview">>,
          name => <<"GPT-4o mini Audio">>,
          category => cost_optimized,
          capabilities => [chat, audio_input, audio_output],
          description => <<"Smaller model capable of audio inputs and outputs">>,
          provider => openai,
          deprecated => false,
          cost_tier => medium,
          speed_tier => fast,
          intelligence_tier => standard},
          
        %% Realtime Models
        #{id => <<"gpt-4o-realtime-preview">>,
          name => <<"GPT-4o Realtime">>,
          category => realtime,
          capabilities => [realtime, chat, audio_input, audio_output],
          description => <<"Model capable of realtime text and audio inputs and outputs">>,
          provider => openai,
          deprecated => false,
          cost_tier => high,
          speed_tier => very_fast,
          intelligence_tier => advanced},
          
        #{id => <<"gpt-4o-mini-realtime-preview">>,
          name => <<"GPT-4o mini Realtime">>,
          category => realtime,
          capabilities => [realtime, chat, audio_input, audio_output],
          description => <<"Smaller realtime model for text and audio inputs and outputs">>,
          provider => openai,
          deprecated => false,
          cost_tier => medium,
          speed_tier => very_fast,
          intelligence_tier => standard},
          
        %% Image Generation Models
        #{id => <<"gpt-image-1">>,
          name => <<"GPT Image 1">>,
          category => image_generation,
          capabilities => [image_generation],
          description => <<"State-of-the-art image generation model">>,
          provider => openai,
          deprecated => false,
          cost_tier => high,
          speed_tier => medium,
          intelligence_tier => expert},
          
        #{id => <<"dall-e-3">>,
          name => <<"DALL·E 3">>,
          category => image_generation,
          capabilities => [image_generation],
          description => <<"Previous generation image generation model">>,
          provider => openai,
          deprecated => false,
          cost_tier => medium,
          speed_tier => medium,
          intelligence_tier => advanced},
          
        #{id => <<"dall-e-2">>,
          name => <<"DALL·E 2">>,
          category => image_generation,
          capabilities => [image_generation],
          description => <<"Our first image generation model">>,
          provider => openai,
          deprecated => false,
          cost_tier => low,
          speed_tier => fast,
          intelligence_tier => standard},
          
        %% Text-to-Speech Models
        #{id => <<"gpt-4o-mini-tts">>,
          name => <<"GPT-4o mini TTS">>,
          category => text_to_speech,
          capabilities => [audio_output],
          description => <<"Text-to-speech model powered by GPT-4o mini">>,
          provider => openai,
          deprecated => false,
          cost_tier => low,
          speed_tier => very_fast,
          intelligence_tier => standard},
          
        #{id => <<"tts-1">>,
          name => <<"TTS-1">>,
          category => text_to_speech,
          capabilities => [audio_output],
          description => <<"Text-to-speech model optimized for speed">>,
          provider => openai,
          deprecated => false,
          cost_tier => low,
          speed_tier => very_fast,
          intelligence_tier => standard},
          
        #{id => <<"tts-1-hd">>,
          name => <<"TTS-1 HD">>,
          category => text_to_speech,
          capabilities => [audio_output],
          description => <<"Text-to-speech model optimized for quality">>,
          provider => openai,
          deprecated => false,
          cost_tier => medium,
          speed_tier => medium,
          intelligence_tier => advanced},
          
        %% Transcription Models
        #{id => <<"gpt-4o-transcribe">>,
          name => <<"GPT-4o Transcribe">>,
          category => transcription,
          capabilities => [audio_input],
          description => <<"Speech-to-text model powered by GPT-4o">>,
          provider => openai,
          deprecated => false,
          cost_tier => medium,
          speed_tier => fast,
          intelligence_tier => advanced},
          
        #{id => <<"gpt-4o-mini-transcribe">>,
          name => <<"GPT-4o mini Transcribe">>,
          category => transcription,
          capabilities => [audio_input],
          description => <<"Speech-to-text model powered by GPT-4o mini">>,
          provider => openai,
          deprecated => false,
          cost_tier => low,
          speed_tier => very_fast,
          intelligence_tier => standard},
          
        #{id => <<"whisper-1">>,
          name => <<"Whisper">>,
          category => transcription,
          capabilities => [audio_input],
          description => <<"General-purpose speech recognition model">>,
          provider => openai,
          deprecated => false,
          cost_tier => low,
          speed_tier => fast,
          intelligence_tier => standard},
          
        %% Tool-specific Models
        #{id => <<"gpt-4o-search-preview">>,
          name => <<"GPT-4o Search Preview">>,
          category => tool_specific,
          capabilities => [chat, web_search],
          description => <<"GPT model for web search in Chat Completions">>,
          provider => openai,
          deprecated => false,
          cost_tier => medium,
          speed_tier => medium,
          intelligence_tier => advanced},
          
        #{id => <<"gpt-4o-mini-search-preview">>,
          name => <<"GPT-4o mini Search Preview">>,
          category => tool_specific,
          capabilities => [chat, web_search],
          description => <<"Fast, affordable small model for web search">>,
          provider => openai,
          deprecated => false,
          cost_tier => low,
          speed_tier => fast,
          intelligence_tier => standard},
          
        #{id => <<"computer-use-preview">>,
          name => <<"computer-use-preview">>,
          category => tool_specific,
          capabilities => [computer_use],
          description => <<"Specialized model for computer use tool">>,
          provider => openai,
          deprecated => false,
          cost_tier => high,
          speed_tier => medium,
          intelligence_tier => advanced},
          
        #{id => <<"codex-mini-latest">>,
          name => <<"codex-mini-latest">>,
          category => tool_specific,
          capabilities => [reasoning, code_optimization],
          description => <<"Fast reasoning model optimized for the Codex CLI">>,
          provider => openai,
          deprecated => false,
          cost_tier => medium,
          speed_tier => fast,
          intelligence_tier => advanced},
          
        %% Embeddings Models
        #{id => <<"text-embedding-3-small">>,
          name => <<"text-embedding-3-small">>,
          category => embeddings,
          capabilities => [embeddings],
          description => <<"Small embedding model">>,
          provider => openai,
          deprecated => false,
          cost_tier => low,
          speed_tier => very_fast,
          intelligence_tier => standard},
          
        #{id => <<"text-embedding-3-large">>,
          name => <<"text-embedding-3-large">>,
          category => embeddings,
          capabilities => [embeddings],
          description => <<"Most capable embedding model">>,
          provider => openai,
          deprecated => false,
          cost_tier => medium,
          speed_tier => fast,
          intelligence_tier => advanced},
          
        #{id => <<"text-embedding-ada-002">>,
          name => <<"text-embedding-ada-002">>,
          category => embeddings,
          capabilities => [embeddings],
          description => <<"Older embedding model">>,
          provider => openai,
          deprecated => false,
          cost_tier => low,
          speed_tier => very_fast,
          intelligence_tier => basic},
          
        %% Moderation Models
        #{id => <<"omni-moderation-latest">>,
          name => <<"omni-moderation">>,
          category => moderation,
          capabilities => [moderation, image_input],
          description => <<"Identify potentially harmful content in text and images">>,
          provider => openai,
          deprecated => false,
          cost_tier => low,
          speed_tier => very_fast,
          intelligence_tier => standard},
          
        #{id => <<"text-moderation-latest">>,
          name => <<"text-moderation">>,
          category => moderation,
          capabilities => [moderation],
          description => <<"Previous generation text-only moderation model">>,
          provider => openai,
          deprecated => true,
          cost_tier => low,
          speed_tier => very_fast,
          intelligence_tier => basic},
          
        %% Legacy Models
        #{id => <<"gpt-4-turbo">>,
          name => <<"GPT-4 Turbo">>,
          category => legacy,
          capabilities => [chat],
          description => <<"An older high-intelligence GPT model">>,
          provider => openai,
          deprecated => false,
          cost_tier => high,
          speed_tier => medium,
          intelligence_tier => advanced},
          
        #{id => <<"gpt-4">>,
          name => <<"GPT-4">>,
          category => legacy,
          capabilities => [chat],
          description => <<"An older high-intelligence GPT model">>,
          provider => openai,
          deprecated => false,
          cost_tier => high,
          speed_tier => slow,
          intelligence_tier => advanced},
          
        #{id => <<"gpt-3.5-turbo">>,
          name => <<"GPT-3.5 Turbo">>,
          category => legacy,
          capabilities => [chat],
          description => <<"Legacy GPT model for cheaper chat and non-chat tasks">>,
          provider => openai,
          deprecated => false,
          cost_tier => low,
          speed_tier => very_fast,
          intelligence_tier => standard},
          
        %% Base Models
        #{id => <<"babbage-002">>,
          name => <<"babbage-002">>,
          category => base,
          capabilities => [chat],
          description => <<"Replacement for the GPT-3 ada and babbage base models">>,
          provider => openai,
          deprecated => false,
          cost_tier => low,
          speed_tier => very_fast,
          intelligence_tier => basic},
          
        #{id => <<"davinci-002">>,
          name => <<"davinci-002">>,
          category => base,
          capabilities => [chat],
          description => <<"Replacement for the GPT-3 curie and davinci base models">>,
          provider => openai,
          deprecated => false,
          cost_tier => medium,
          speed_tier => fast,
          intelligence_tier => standard},
          
        %% Anthropic Models (for compatibility)
        #{id => <<"claude-3-opus-20240229">>,
          name => <<"Claude 3 Opus">>,
          category => flagship_chat,
          capabilities => [chat],
          description => <<"Most capable Claude model for complex tasks">>,
          provider => anthropic,
          deprecated => false,
          cost_tier => high,
          speed_tier => medium,
          intelligence_tier => expert},
          
        #{id => <<"claude-3-sonnet-20240229">>,
          name => <<"Claude 3 Sonnet">>,
          category => flagship_chat,
          capabilities => [chat],
          description => <<"Balanced Claude model for most tasks">>,
          provider => anthropic,
          deprecated => false,
          cost_tier => medium,
          speed_tier => fast,
          intelligence_tier => advanced},
          
        #{id => <<"claude-3-haiku-20240307">>,
          name => <<"Claude 3 Haiku">>,
          category => cost_optimized,
          capabilities => [chat],
          description => <<"Fast, cost-effective Claude model">>,
          provider => anthropic,
          deprecated => false,
          cost_tier => low,
          speed_tier => very_fast,
          intelligence_tier => standard}
    ].

%% @doc Get information about a specific model
-spec get_model_info(binary()) -> {ok, model_info()} | {error, model_not_found}.
get_model_info(ModelId) ->
    case lists:filter(fun(Model) -> 
        maps:get(id, Model) =:= ModelId 
    end, get_all_models()) of
        [] -> {error, model_not_found};
        [Model | _] -> {ok, Model}
    end.

%% @doc Get models by category
-spec get_models_by_category(model_category()) -> [model_info()].
get_models_by_category(Category) ->
    [Model || Model <- get_all_models(), 
              maps:get(category, Model) =:= Category].

%% @doc Get models by capability
-spec get_models_by_capability(model_capability()) -> [model_info()].
get_models_by_capability(Capability) ->
    [Model || Model <- get_all_models(),
              lists:member(Capability, maps:get(capabilities, Model))].

%% @doc Validate if a model ID exists
-spec validate_model(binary()) -> boolean().
validate_model(ModelId) ->
    case get_model_info(ModelId) of
        {ok, _} -> true;
        {error, model_not_found} -> false
    end.

%% @doc Get the default model
-spec get_default_model() -> binary().
get_default_model() ->
    <<"gpt-4.1">>.

%% @doc Get all model categories
-spec get_model_categories() -> [model_category()].
get_model_categories() ->
    [flagship_chat, reasoning, cost_optimized, realtime, image_generation,
     text_to_speech, transcription, tool_specific, embeddings, moderation,
     legacy, base].

%% @doc Get all model capabilities
-spec get_model_capabilities() -> [model_capability()].
get_model_capabilities() ->
    [chat, reasoning, audio_input, audio_output, image_generation, image_input,
     realtime, web_search, computer_use, code_optimization, embeddings, moderation].