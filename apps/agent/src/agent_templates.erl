%% agent_templates.erl
%% Pre-configured agent templates for common use cases
-module(agent_templates).

-export([
    get_template/1,
    list_templates/0,
    create_from_template/2,
    register_custom_template/2
]).

-define(TEMPLATES, #{
    <<"researcher">> => #{
        name => <<"Research Assistant">>,
        description => <<"An agent specialized in research and information gathering">>,
        model => <<"gpt-4">>,
        tools => [web_search, file_operations, data_processing],
        system_prompt => <<"You are a meticulous research assistant. Your goal is to find accurate, relevant information and present it in a clear, organized manner. Always cite your sources and verify information when possible.">>
    },
    
    <<"coder">> => #{
        name => <<"Code Assistant">>,
        description => <<"An agent specialized in software development and coding tasks">>,
        model => <<"gpt-4">>,
        tools => [shell, file_operations, code_analysis],
        system_prompt => <<"You are an expert software developer. Write clean, efficient, and well-documented code. Follow best practices and consider security, performance, and maintainability in your solutions.">>
    },
    
    <<"analyst">> => #{
        name => <<"Data Analyst">>,
        description => <<"An agent specialized in data analysis and visualization">>,
        model => <<"gpt-4">>,
        tools => [data_processing, file_operations, visualization],
        system_prompt => <<"You are a data analyst expert. Analyze data thoroughly, identify patterns and insights, and present findings in a clear, visual manner. Always validate your analysis and consider statistical significance.">>
    },
    
    <<"orchestrator">> => #{
        name => <<"Task Orchestrator">>,
        description => <<"An agent that coordinates other agents to complete complex tasks">>,
        model => <<"gpt-4">>,
        tools => [agent_management, task_planning, monitoring],
        system_prompt => <<"You are a task orchestrator. Break down complex tasks into manageable sub-tasks, delegate to appropriate agents, monitor progress, and ensure successful completion. Coordinate effectively and handle failures gracefully.">>
    },
    
    <<"monitor">> => #{
        name => <<"System Monitor">>,
        description => <<"An agent that monitors system health and performance">>,
        model => <<"gpt-3.5-turbo">>,
        tools => [system_metrics, alerting, logging],
        system_prompt => <<"You are a system monitoring agent. Track system health, performance metrics, and agent activities. Alert on anomalies, generate reports, and suggest optimizations.">>
    },
    
    <<"translator">> => #{
        name => <<"Language Translator">>,
        description => <<"An agent specialized in language translation and localization">>,
        model => <<"gpt-4">>,
        tools => [text_processing, file_operations],
        system_prompt => <<"You are a professional translator. Provide accurate, culturally appropriate translations while preserving the original meaning and tone. Handle idioms and context sensitively.">>
    },
    
    <<"teacher">> => #{
        name => <<"Educational Assistant">>,
        description => <<"An agent that helps with learning and education">>,
        model => <<"gpt-4">>,
        tools => [knowledge_base, quiz_generator, progress_tracking],
        system_prompt => <<"You are an educational assistant. Explain concepts clearly, adapt to the learner's level, provide examples, and create engaging learning experiences. Be patient and encouraging.">>
    },
    
    <<"debugger">> => #{
        name => <<"Debug Assistant">>,
        description => <<"An agent specialized in debugging and troubleshooting">>,
        model => <<"gpt-4">>,
        tools => [code_analysis, log_analysis, shell],
        system_prompt => <<"You are a debugging expert. Systematically identify and resolve issues, analyze error messages and logs, suggest solutions, and explain root causes. Be thorough and methodical.">>
    }
}).

%% Custom templates storage (in production, this would be persistent)
-define(CUSTOM_TEMPLATES_TABLE, agent_custom_templates).

%% Get a specific template
get_template(TemplateId) ->
    case maps:get(TemplateId, ?TEMPLATES, undefined) of
        undefined ->
            % Check custom templates
            case ets:lookup(?CUSTOM_TEMPLATES_TABLE, TemplateId) of
                [{_, Template}] -> {ok, Template};
                [] -> {error, template_not_found}
            end;
        Template ->
            {ok, Template}
    end.

%% List all available templates
list_templates() ->
    BuiltIn = maps:fold(fun(Id, Template, Acc) ->
        [#{
            id => Id,
            name => maps:get(name, Template),
            description => maps:get(description, Template),
            type => built_in
        } | Acc]
    end, [], ?TEMPLATES),
    
    Custom = case ets:info(?CUSTOM_TEMPLATES_TABLE) of
        undefined -> [];
        _ ->
            ets:foldl(fun({Id, Template}, Acc) ->
                [#{
                    id => Id,
                    name => maps:get(name, Template),
                    description => maps:get(description, Template),
                    type => custom
                } | Acc]
            end, [], ?CUSTOM_TEMPLATES_TABLE)
    end,
    
    BuiltIn ++ Custom.

%% Create an agent from a template
create_from_template(TemplateId, Overrides) ->
    case get_template(TemplateId) of
        {ok, Template} ->
            % Merge template with overrides
            Config = maps:merge(Template, Overrides),
            
            % Add template metadata
            EnrichedConfig = Config#{
                template_id => TemplateId,
                created_from_template => true
            },
            
            % Create the agent using the supervisor
            agent_supervisor:start_agent(EnrichedConfig);
        {error, Reason} ->
            {error, Reason}
    end.

%% Register a custom template
register_custom_template(TemplateId, Template) ->
    ensure_custom_templates_table(),
    
    % Validate template structure
    case validate_template(Template) of
        ok ->
            ets:insert(?CUSTOM_TEMPLATES_TABLE, {TemplateId, Template}),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% Internal functions

ensure_custom_templates_table() ->
    case ets:info(?CUSTOM_TEMPLATES_TABLE) of
        undefined ->
            ets:new(?CUSTOM_TEMPLATES_TABLE, [
                named_table,
                public,
                {keypos, 1}
            ]);
        _ ->
            ok
    end.

validate_template(Template) ->
    RequiredFields = [name, description, model, tools, system_prompt],
    case lists:all(fun(Field) -> maps:is_key(Field, Template) end, RequiredFields) of
        true -> ok;
        false -> {error, missing_required_fields}
    end.