# AI Model Index

This document provides a comprehensive index of all available AI models in the agents.erl system.

## Quick Start

The system now includes a comprehensive model registry with all the latest OpenAI models and support for Anthropic Claude models.

### Using the Model Registry

```erlang
% Get all available models
AllModels = model_registry:get_all_models().

% Get models by category
ChatModels = model_registry:get_models_by_category(flagship_chat).
ReasoningModels = model_registry:get_models_by_category(reasoning).

% Get models by capability
AudioModels = model_registry:get_models_by_capability(audio_input).
RealtimeModels = model_registry:get_models_by_capability(realtime).

% Validate a model ID
model_registry:validate_model(<<"gpt-4.1">>). % returns true
model_registry:validate_model(<<"invalid-model">>). % returns false

% Get detailed information about a model
{ok, ModelInfo} = model_registry:get_model_info(<<"gpt-4.1">>).
```

### Model Comparison and Selection

```erlang
% Compare two models
Comparison = model_comparison:compare_models(<<"gpt-4.1">>, <<"gpt-4.1-mini">>).

% Get model recommendation for a specific task
{ok, BestModel} = model_comparison:recommend_model(#{
    task_type => coding,
    priority => balanced,  % cost | speed | intelligence | balanced
    max_cost_tier => high
}).

% Get rankings
CostEfficient = model_comparison:get_cost_performance_ranking().
Fastest = model_comparison:get_speed_ranking().
Smartest = model_comparison:get_intelligence_ranking().
```

## Model Categories

### 1. Flagship Chat Models
High-performance models for complex conversational tasks.

| Model | Description | Cost | Speed | Intelligence |
|-------|-------------|------|-------|--------------|
| gpt-4.1 | Flagship GPT model for complex tasks | High | Medium | Expert |
| gpt-4o | Fast, intelligent, flexible GPT model | Medium | Fast | Advanced |
| gpt-4o-audio-preview | GPT-4o with audio capabilities | High | Medium | Advanced |
| chatgpt-4o-latest | GPT-4o model used in ChatGPT | Medium | Fast | Advanced |

### 2. Reasoning Models
Specialized models for complex reasoning and problem-solving.

| Model | Description | Cost | Speed | Intelligence |
|-------|-------------|------|-------|--------------|
| o3 | Most powerful reasoning model | Very High | Slow | Expert |
| o4-mini | Faster, affordable reasoning model | Medium | Fast | Advanced |
| o3-mini | Small alternative to o3 | Medium | Medium | Advanced |
| o1 | Previous full o-series model | High | Slow | Expert |
| o1-pro | o1 with more compute | Very High | Slow | Expert |

### 3. Cost-Optimized Models
Efficient models balancing performance and cost.

| Model | Description | Cost | Speed | Intelligence |
|-------|-------------|------|-------|--------------|
| gpt-4.1-mini | Balanced intelligence, speed, cost | Low | Very Fast | Standard |
| gpt-4.1-nano | Fastest, most cost-effective | Low | Very Fast | Basic |
| gpt-4o-mini | Fast, affordable for focused tasks | Low | Very Fast | Standard |

### 4. Realtime Models
Models for real-time text and audio interactions.

| Model | Description | Capabilities |
|-------|-------------|--------------|
| gpt-4o-realtime-preview | Full realtime capabilities | Text + Audio I/O |
| gpt-4o-mini-realtime-preview | Smaller realtime model | Text + Audio I/O |

### 5. Specialized Models

#### Image Generation
- **gpt-image-1**: State-of-the-art image generation
- **dall-e-3**: Previous generation image model
- **dall-e-2**: First image generation model

#### Text-to-Speech
- **gpt-4o-mini-tts**: TTS powered by GPT-4o mini
- **tts-1**: Optimized for speed
- **tts-1-hd**: Optimized for quality

#### Transcription
- **gpt-4o-transcribe**: Speech-to-text with GPT-4o
- **gpt-4o-mini-transcribe**: Smaller transcription model
- **whisper-1**: General-purpose speech recognition

#### Tool-Specific
- **gpt-4o-search-preview**: Web search optimized
- **computer-use-preview**: Computer use tool
- **codex-mini-latest**: Code optimization

## Creating Agents with Specific Models

```erlang
% Create an agent with a specific model
Config = #{
    name => <<"My Assistant">>,
    model => <<"gpt-4.1-mini">>,  % Specify the model
    system_prompt => <<"You are a helpful assistant">>,
    tools => [web_search, file_operations]
},
{ok, AgentId} = agent:create(Config).

% Update an agent's model
agent:update_model(AgentId, <<"o3">>).
```

## Agent Templates with Updated Models

The agent templates have been updated to use the latest models:

- **researcher**: Uses GPT-4.1 for comprehensive research
- **advanced_researcher**: Uses o3 for complex reasoning
- **reasoning_specialist**: Uses o3 for advanced problem-solving
- **efficient_coder**: Uses GPT-4.1-mini for fast coding
- **lightweight_helper**: Uses GPT-4.1-nano for ultra-fast responses

## Model Selection Guidelines

### By Use Case

1. **General Chat/Assistance**
   - Best: gpt-4.1-mini (balanced)
   - Premium: gpt-4.1 (highest quality)
   - Budget: gpt-4.1-nano (fastest, cheapest)

2. **Complex Reasoning**
   - Best: o3 (most capable)
   - Fast: o4-mini (good balance)
   - Budget: o3-mini (smaller but capable)

3. **Real-time Interaction**
   - Best: gpt-4o-realtime-preview
   - Budget: gpt-4o-mini-realtime-preview

4. **Code Development**
   - Best: gpt-4.1 or gpt-4o
   - Fast: gpt-4.1-mini
   - Specialized: codex-mini-latest

### By Priority

- **Cost Priority**: gpt-4.1-nano, gpt-4o-mini
- **Speed Priority**: gpt-4.1-nano, gpt-4.1-mini
- **Intelligence Priority**: o3, gpt-4.1, o1-pro
- **Balanced**: gpt-4.1-mini, gpt-4o

## Running the Demo

To see the model system in action:

```bash
# Compile and run the demo
erlc -o ebin examples/model_usage_demo.erl
erl -pa ebin -pa apps/*/ebin -s model_usage_demo run -s init stop
```

This will demonstrate:
- Listing available models
- Comparing models
- Getting recommendations
- Creating agents with different models