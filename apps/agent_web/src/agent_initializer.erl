-module(agent_initializer).

%% Colorful logging macros
-define(LOG_INFO(Msg), colored_logger:data(processed, Msg)).
-define(LOG_INFO(Msg, Args), colored_logger:data(processed, io_lib:format(Msg, Args))).
-define(LOG_ERROR(Msg), colored_logger:fire(inferno, Msg)).
-define(LOG_ERROR(Msg, Args), colored_logger:fire(inferno, io_lib:format(Msg, Args))).
-define(LOG_WARNING(Msg), colored_logger:alarm(medium, Msg)).
-define(LOG_WARNING(Msg, Args), colored_logger:alarm(medium, io_lib:format(Msg, Args))).
-define(LOG_SUCCESS(Msg), colored_logger:complete(success, Msg)).
-define(LOG_SUCCESS(Msg, Args), colored_logger:complete(success, io_lib:format(Msg, Args))).
-define(LOG_DEBUG(Msg), colored_logger:development(debugging, Msg)).
-define(LOG_DEBUG(Msg, Args), colored_logger:development(debugging, io_lib:format(Msg, Args))).

-export([init_default_agents/0]).

%% Internal function to generate UUID
generate_uuid() ->
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    ID = io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b", 
                       [A, B, C, D, E]),
    list_to_binary(ID).

init_default_agents() ->
    ?LOG_INFO("Initializing extensive agent fleet with 30+ specialized agents..."),
    
    %% Check if agents already exist
    ExistingAgents = agent_registry:list_agents(),
    ExistingNames = lists:map(fun({_, _, Meta}) ->
        maps:get(name, Meta, undefined)
    end, ExistingAgents),
    
    %% Comprehensive list of 40+ default agents with proper gpt-4.1 models
    DefaultAgents = [
        %% Core Research & Analysis Team (use gpt-4.1 for main models)
        #{name => <<"Research Assistant">>, type => researcher, template => <<"researcher">>, model_override => <<"gpt-4.1">>},
        #{name => <<"Advanced Research Agent">>, type => advanced_researcher, template => <<"advanced_researcher">>, model_override => <<"gpt-4.1">>},
        #{name => <<"Data Analyst">>, type => analyst, template => <<"analyst">>, model_override => <<"gpt-4.1">>},
        #{name => <<"Data Scientist">>, type => data_scientist, template => <<"data_scientist">>, model_override => <<"gpt-4.1">>},
        #{name => <<"Quantum Data Analyst">>, type => quantum_analyst, template => <<"quantum_analyst">>, model_override => <<"gpt-4.1">>},
        
        %% Software Development Team (use gpt-4.1 for complex coding)
        #{name => <<"Code Assistant">>, type => coder, template => <<"coder">>, model_override => <<"gpt-4.1">>},
        #{name => <<"Debug Assistant">>, type => debugger, template => <<"debugger">>, model_override => <<"gpt-4.1">>},
        #{name => <<"API Designer">>, type => api_designer, template => <<"api_designer">>, model_override => <<"gpt-4.1-mini">>},
        #{name => <<"DevOps Engineer">>, type => devops_engineer, template => <<"devops_engineer">>, model_override => <<"gpt-4.1">>},
        #{name => <<"Performance Optimizer">>, type => performance_optimizer, template => <<"performance_optimizer">>, model_override => <<"gpt-4.1-mini">>},
        #{name => <<"ML Engineer">>, type => ml_engineer, template => <<"ml_engineer">>, model_override => <<"gpt-4.1">>},
        #{name => <<"Mobile Developer">>, type => mobile_developer, template => <<"mobile_developer">>, model_override => <<"gpt-4.1-mini">>},
        #{name => <<"Database Architect">>, type => database_architect, template => <<"database_architect">>, model_override => <<"gpt-4.1">>},
        #{name => <<"Game Developer">>, type => game_developer, template => <<"game_developer">>, model_override => <<"gpt-4.1-mini">>},
        #{name => <<"Blockchain Developer">>, type => blockchain_developer, template => <<"blockchain_developer">>, model_override => <<"gpt-4.1">>},
        
        %% Specialized Systems & Architecture (use gpt-4.1 for complex architecture)
        #{name => <<"Systems Architect">>, type => systems_architect, template => <<"systems_architect">>, model_override => <<"gpt-4.1">>},
        #{name => <<"Cloud Architect">>, type => cloud_architect, template => <<"cloud_architect">>, model_override => <<"gpt-4.1">>},
        #{name => <<"Security Analyst">>, type => security_analyst, template => <<"security_analyst">>, model_override => <<"gpt-4.1">>},
        #{name => <<"Ethical Hacker">>, type => ethical_hacker, template => <<"ethical_hacker">>, model_override => <<"gpt-4.1">>},
        #{name => <<"Network Engineer">>, type => network_engineer, template => <<"network_engineer">>, model_override => <<"gpt-4.1-mini">>},
        #{name => <<"Quantum Developer">>, type => quantum_developer, template => <<"quantum_developer">>, model_override => <<"gpt-4.1">>},
        #{name => <<"Robotics Engineer">>, type => robotics_engineer, template => <<"robotics_engineer">>, model_override => <<"gpt-4.1">>},
        #{name => <<"Digital Twin Architect">>, type => digital_twin_architect, template => <<"digital_twin_architect">>, model_override => <<"gpt-4.1">>},
        
        %% Management & Coordination (use o4-mini for some)
        #{name => <<"Task Orchestrator">>, type => orchestrator, template => <<"orchestrator">>, model_override => <<"gpt-4.1">>},
        #{name => <<"Product Manager">>, type => product_manager, template => <<"product_manager">>, model_override => <<"o4-mini">>},
        #{name => <<"System Monitor">>, type => monitor, template => <<"monitor">>, model_override => <<"gpt-4.1-mini">>},
        
        %% Communication & Content (mix of models)
        #{name => <<"Language Translator">>, type => translator, template => <<"translator">>, model_override => <<"gpt-4.1">>},
        #{name => <<"Content Creator">>, type => content_creator, template => <<"content_creator">>, model_override => <<"gpt-4.1">>},
        #{name => <<"Technical Writer">>, type => technical_writer, template => <<"technical_writer">>, model_override => <<"gpt-4.1-mini">>},
        #{name => <<"UX Designer">>, type => ux_designer, template => <<"ux_designer">>, model_override => <<"gpt-4.1-mini">>},
        #{name => <<"Voice UI Designer">>, type => voice_ui_designer, template => <<"voice_ui_designer">>, model_override => <<"gpt-4.1-mini">>},
        
        %% Education & Healthcare (use gpt-4.1 for healthcare)
        #{name => <<"Educational Assistant">>, type => teacher, template => <<"teacher">>, model_override => <<"gpt-4.1">>},
        #{name => <<"Educational Specialist">>, type => educational_specialist, template => <<"educational_specialist">>, model_override => <<"gpt-4.1-mini">>},
        #{name => <<"Clinical Psychologist">>, type => clinical_psychologist, template => <<"clinical_psychologist">>, model_override => <<"gpt-4.1">>},
        #{name => <<"Medical Advisor">>, type => medical_advisor, template => <<"medical_advisor">>, model_override => <<"gpt-4.1">>},
        #{name => <<"Nutritionist">>, type => nutritionist, template => <<"nutritionist">>, model_override => <<"gpt-4.1-mini">>},
        
        %% Science & Research Specialists (use gpt-4.1 for complex research)
        #{name => <<"Bioinformatics Analyst">>, type => bioinformatics_analyst, template => <<"bioinformatics_analyst">>, model_override => <<"gpt-4.1">>},
        #{name => <<"Astrobiology Researcher">>, type => astrobiology_researcher, template => <<"astrobiology_researcher">>, model_override => <<"gpt-4.1">>},
        #{name => <<"Climate Data Analyst">>, type => climate_data_analyst, template => <<"climate_data_analyst">>, model_override => <<"gpt-4.1">>},
        #{name => <<"Conservation Geneticist">>, type => conservation_geneticist, template => <<"conservation_geneticist">>, model_override => <<"gpt-4.1">>},
        #{name => <<"Space Tech Engineer">>, type => space_tech_engineer, template => <<"space_tech_engineer">>, model_override => <<"gpt-4.1">>},
        
        %% Humanities & Social Sciences (use o4-mini for some)
        #{name => <<"Philosopher">>, type => philosopher, template => <<"philosopher">>, model_override => <<"o4-mini">>},
        #{name => <<"Anthropologist">>, type => anthropologist, template => <<"anthropologist">>, model_override => <<"gpt-4.1">>},
        #{name => <<"Historian">>, type => historian, template => <<"historian">>, model_override => <<"gpt-4.1">>},
        #{name => <<"Linguist">>, type => linguist, template => <<"linguist">>, model_override => <<"gpt-4.1">>},
        #{name => <<"Sociologist">>, type => sociologist, template => <<"sociologist">>, model_override => <<"gpt-4.1-mini">>},
        
        %% Quality Assurance & Testing
        #{name => <<"Automation Tester">>, type => automation_tester, template => <<"automation_tester">>, model_override => <<"gpt-4.1-mini">>},
        
        %% Emerging Technologies
        #{name => <<"AR/VR Developer">>, type => ar_vr_developer, template => <<"ar_vr_developer">>, model_override => <<"gpt-4.1-mini">>},
        #{name => <<"IoT Specialist">>, type => iot_specialist, template => <<"iot_specialist">>, model_override => <<"gpt-4.1-mini">>},
        #{name => <<"Edge Computing Specialist">>, type => edge_computing_specialist, template => <<"edge_computing_specialist">>, model_override => <<"gpt-4.1-mini">>},
        
        %% Business & Finance
        #{name => <<"FinTech Developer">>, type => fintech_developer, template => <<"fintech_developer">>, model_override => <<"gpt-4.1-mini">>},
        #{name => <<"Supply Chain Optimizer">>, type => supply_chain_optimizer, template => <<"supply_chain_optimizer">>, model_override => <<"gpt-4.1-mini">>},
        #{name => <<"Legal Tech Advisor">>, type => legal_tech_advisor, template => <<"legal_tech_advisor">>, model_override => <<"gpt-4.1">>},
        
        %% Accessibility & Sustainability (use o4-mini for ethics)
        #{name => <<"Accessibility Expert">>, type => accessibility_expert, template => <<"accessibility_expert">>, model_override => <<"gpt-4.1-mini">>},
        #{name => <<"Sustainability Advisor">>, type => sustainability_advisor, template => <<"sustainability_advisor">>, model_override => <<"gpt-4.1-mini">>},
        #{name => <<"AI Ethics Advisor">>, type => ai_ethics_advisor, template => <<"ai_ethics_advisor">>, model_override => <<"o4-mini">>}
    ],
    
    %% Create each agent
    Results = lists:map(fun(Agent) ->
        #{name := Name, template := Template} = Agent,
        ModelOverride = maps:get(model_override, Agent, undefined),
        case lists:member(Name, ExistingNames) of
            true ->
                ?LOG_WARNING("Agent ~s already exists, skipping", [Name]),
                {skipped, Name, already_exists};
            false ->
                %% Prepare overrides with model if specified
                Overrides = case ModelOverride of
                    undefined -> #{name => Name};
                    Model -> #{name => Name, model => Model}
                end,
                case agent_templates:create_from_template(Template, Overrides) of
                    {ok, Pid} ->
                        AgentId = generate_uuid(),
                        agent_registry:register_agent(AgentId, Pid, #{
                            type => template,
                            name => Name,
                            template_id => Template,
                            model => ModelOverride
                        }),
                        ?LOG_SUCCESS("Created agent ~s (ID: ~s) with model ~s", [Name, AgentId, ModelOverride]),
                        {ok, AgentId, Name};
                    {error, Reason} ->
                        ?LOG_ERROR("Failed to create agent ~s: ~p", [Name, Reason]),
                        {error, Name, Reason}
                end
        end
    end, DefaultAgents),
    
    %% Summary
    Ok = [R || R = {ok, _, _} <- Results],
    Errors = [R || R = {error, _, _} <- Results],
    Skipped = [R || R = {skipped, _, _} <- Results],
    ?LOG_SUCCESS("Agent initialization complete: ~p successful, ~p failed, ~p skipped", 
                 [length(Ok), length(Errors), length(Skipped)]),
    
    Results.