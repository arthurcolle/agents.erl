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
        #{id => <<"gpt-5.6-sol">>,
          name => <<"GPT-5.6 Sol">>,
          category => flagship_chat,
          capabilities => [chat, reasoning, image_input],
          description => <<"Latest frontier model for the most complex tasks">>,
          provider => openai,
          deprecated => false,
          cost_tier => high,
          speed_tier => medium,
          intelligence_tier => expert},

        #{id => <<"gpt-5.6-terra">>,
          name => <<"GPT-5.6 Terra">>,
          category => flagship_chat,
          capabilities => [chat, reasoning, image_input],
          description => <<"Balanced model for everyday work">>,
          provider => openai,
          deprecated => false,
          cost_tier => medium,
          speed_tier => fast,
          intelligence_tier => advanced},

        #{id => <<"gpt-5.5">>,
          name => <<"GPT-5.5">>,
          category => flagship_chat,
          capabilities => [chat, reasoning, image_input],
          description => <<"Frontier model for complex coding, research, and real-world tasks">>,
          provider => openai,
          deprecated => false,
          cost_tier => high,
          speed_tier => medium,
          intelligence_tier => expert},

        #{id => <<"gpt-5.2">>,
          name => <<"GPT-5.2">>,
          category => flagship_chat,
          capabilities => [chat, reasoning, image_input],
          description => <<"Optimized for professional work and long-running agents">>,
          provider => openai,
          deprecated => false,
          cost_tier => medium,
          speed_tier => medium,
          intelligence_tier => advanced},

        %% Cost-optimized Models
        #{id => <<"gpt-5.6-luna">>,
          name => <<"GPT-5.6 Luna">>,
          category => cost_optimized,
          capabilities => [chat, reasoning, image_input],
          description => <<"Fast and affordable model for high-volume workloads">>,
          provider => openai,
          deprecated => false,
          cost_tier => low,
          speed_tier => very_fast,
          intelligence_tier => standard},

        #{id => <<"gpt-5.4-mini">>,
          name => <<"GPT-5.4 Mini">>,
          category => cost_optimized,
          capabilities => [chat, reasoning],
          description => <<"Small, fast, and cost-efficient model for simpler tasks">>,
          provider => openai,
          deprecated => false,
          cost_tier => low,
          speed_tier => very_fast,
          intelligence_tier => standard},

        %% Realtime Models
        #{id => <<"gpt-realtime">>,
          name => <<"GPT Realtime">>,
          category => realtime,
          capabilities => [realtime, chat, audio_input, audio_output],
          description => <<"Model for realtime text and audio inputs and outputs">>,
          provider => openai,
          deprecated => false,
          cost_tier => high,
          speed_tier => very_fast,
          intelligence_tier => advanced},

        #{id => <<"gpt-realtime-mini">>,
          name => <<"GPT Realtime Mini">>,
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

        #{id => <<"gpt-image-1-mini">>,
          name => <<"GPT Image 1 Mini">>,
          category => image_generation,
          capabilities => [image_generation],
          description => <<"Cost-efficient image generation model">>,
          provider => openai,
          deprecated => false,
          cost_tier => medium,
          speed_tier => fast,
          intelligence_tier => advanced},

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

        %% Legacy Models (superseded generations, still served)
        #{id => <<"gpt-5.1">>,
          name => <<"GPT-5.1">>,
          category => legacy,
          capabilities => [chat, reasoning],
          description => <<"Previous generation flagship model">>,
          provider => openai,
          deprecated => false,
          cost_tier => medium,
          speed_tier => medium,
          intelligence_tier => advanced},

        #{id => <<"gpt-5">>,
          name => <<"GPT-5">>,
          category => legacy,
          capabilities => [chat, reasoning],
          description => <<"Older flagship model">>,
          provider => openai,
          deprecated => false,
          cost_tier => medium,
          speed_tier => medium,
          intelligence_tier => advanced},

        #{id => <<"gpt-4.1">>,
          name => <<"GPT-4.1">>,
          category => legacy,
          capabilities => [chat],
          description => <<"Legacy GPT-4 generation model">>,
          provider => openai,
          deprecated => true,
          cost_tier => high,
          speed_tier => medium,
          intelligence_tier => advanced},

        #{id => <<"gpt-4o">>,
          name => <<"GPT-4o">>,
          category => legacy,
          capabilities => [chat],
          description => <<"Legacy GPT-4 generation model">>,
          provider => openai,
          deprecated => true,
          cost_tier => medium,
          speed_tier => fast,
          intelligence_tier => advanced},

        #{id => <<"o3">>,
          name => <<"o3">>,
          category => legacy,
          capabilities => [reasoning, chat],
          description => <<"Legacy o-series reasoning model">>,
          provider => openai,
          deprecated => true,
          cost_tier => very_high,
          speed_tier => slow,
          intelligence_tier => expert},

        #{id => <<"o4-mini">>,
          name => <<"o4-mini">>,
          category => legacy,
          capabilities => [reasoning, chat],
          description => <<"Legacy small reasoning model">>,
          provider => openai,
          deprecated => true,
          cost_tier => medium,
          speed_tier => fast,
          intelligence_tier => advanced},

        %% Anthropic Models
        #{id => <<"claude-fable-5">>,
          name => <<"Claude Fable 5">>,
          category => flagship_chat,
          capabilities => [chat, reasoning, image_input],
          description => <<"Anthropic's most capable model for demanding reasoning and long-horizon agentic work">>,
          provider => anthropic,
          deprecated => false,
          cost_tier => very_high,
          speed_tier => medium,
          intelligence_tier => expert},

        #{id => <<"claude-opus-4-8">>,
          name => <<"Claude Opus 4.8">>,
          category => flagship_chat,
          capabilities => [chat, reasoning, image_input],
          description => <<"Most capable Opus-tier Claude model for long-horizon agentic work">>,
          provider => anthropic,
          deprecated => false,
          cost_tier => high,
          speed_tier => medium,
          intelligence_tier => expert},

        #{id => <<"claude-sonnet-5">>,
          name => <<"Claude Sonnet 5">>,
          category => flagship_chat,
          capabilities => [chat, reasoning, image_input],
          description => <<"Best combination of speed and intelligence in the Sonnet tier">>,
          provider => anthropic,
          deprecated => false,
          cost_tier => medium,
          speed_tier => fast,
          intelligence_tier => advanced},

        #{id => <<"claude-sonnet-4-6">>,
          name => <<"Claude Sonnet 4.6">>,
          category => flagship_chat,
          capabilities => [chat, reasoning, image_input],
          description => <<"Previous generation Sonnet-tier Claude model">>,
          provider => anthropic,
          deprecated => false,
          cost_tier => medium,
          speed_tier => fast,
          intelligence_tier => advanced},

        #{id => <<"claude-haiku-4-5">>,
          name => <<"Claude Haiku 4.5">>,
          category => cost_optimized,
          capabilities => [chat, image_input],
          description => <<"Fastest and most cost-effective Claude model">>,
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
    <<"gpt-5.6-sol">>.

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
