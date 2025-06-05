%% specialized_agent_factory.erl
%% Factory for creating specialized agents with domain-specific toolsets
-module(specialized_agent_factory).
-behaviour(gen_server).

-export([
    start_link/0,
    create_specialized_agent/2,
    get_agent_types/0,
    register_agent_type/3,
    get_tools_for_type/1,
    spawn_agent_fleet/2,
    get_agent_type_metadata/1
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

-record(agent_type, {
    name :: binary(),
    description :: binary(),
    capabilities :: [binary()],
    tools :: [map()],
    specialization :: map(), % domain-specific configuration
    resource_requirements :: map(),
    default_config :: map()
}).

-record(state, {
    agent_types = #{} :: #{binary() => #agent_type{}},
    type_instances = #{} :: #{binary() => [pid()]}, % type -> [agent_pids]
    tools_registry = #{} :: map()
}).

%% API Functions
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Create a specialized agent of a specific type
create_specialized_agent(TypeName, Config) when is_binary(TypeName) ->
    gen_server:call(?SERVER, {create_specialized_agent, TypeName, Config}).

%% Get all available agent types
get_agent_types() ->
    gen_server:call(?SERVER, get_agent_types).

%% Register a new agent type
register_agent_type(TypeName, TypeSpec, Tools) ->
    gen_server:call(?SERVER, {register_agent_type, TypeName, TypeSpec, Tools}).

%% Get tools for a specific agent type
get_tools_for_type(TypeName) ->
    gen_server:call(?SERVER, {get_tools_for_type, TypeName}).

%% Spawn a fleet of agents of the same type
spawn_agent_fleet(TypeName, Count) when is_binary(TypeName), is_integer(Count) ->
    gen_server:call(?SERVER, {spawn_agent_fleet, TypeName, Count}).

%% Get metadata for an agent type
get_agent_type_metadata(TypeName) ->
    gen_server:call(?SERVER, {get_agent_type_metadata, TypeName}).

%% gen_server callbacks
init([]) ->
    % Register predefined specialized agent types
    DefaultTypes = get_predefined_agent_types(),
    
    State = lists:foldl(fun({TypeName, TypeSpec, Tools}, AccState) ->
        register_type_internal(TypeName, TypeSpec, Tools, AccState)
    end, #state{}, DefaultTypes),
    
    {ok, State}.

handle_call({create_specialized_agent, TypeName, Config}, _From, State) ->
    case maps:get(TypeName, State#state.agent_types, undefined) of
        undefined ->
            {reply, {error, {unknown_agent_type, TypeName}}, State};
        #agent_type{} = AgentType ->
            Result = create_agent_internal(AgentType, Config),
            case Result of
                {ok, AgentPid} ->
                    % Track the instance
                    ExistingInstances = maps:get(TypeName, State#state.type_instances, []),
                    NewInstances = maps:put(TypeName, [AgentPid | ExistingInstances], State#state.type_instances),
                    {reply, {ok, AgentPid}, State#state{type_instances = NewInstances}};
                Error ->
                    {reply, Error, State}
            end
    end;

handle_call(get_agent_types, _From, State) ->
    Types = maps:fold(fun(TypeName, #agent_type{description = Desc, capabilities = Caps}, Acc) ->
        [#{
            name => TypeName,
            description => Desc,
            capabilities => Caps,
            instances => length(maps:get(TypeName, State#state.type_instances, []))
        } | Acc]
    end, [], State#state.agent_types),
    {reply, {ok, Types}, State};

handle_call({register_agent_type, TypeName, TypeSpec, Tools}, _From, State) ->
    try
        NewState = register_type_internal(TypeName, TypeSpec, Tools, State),
        {reply, ok, NewState}
    catch
        Error:Reason ->
            {reply, {error, {registration_failed, Error, Reason}}, State}
    end;

handle_call({get_tools_for_type, TypeName}, _From, State) ->
    case maps:get(TypeName, State#state.agent_types, undefined) of
        undefined ->
            {reply, {error, {unknown_agent_type, TypeName}}, State};
        #agent_type{tools = Tools} ->
            {reply, {ok, Tools}, State}
    end;

handle_call({spawn_agent_fleet, TypeName, Count}, _From, State) ->
    case maps:get(TypeName, State#state.agent_types, undefined) of
        undefined ->
            {reply, {error, {unknown_agent_type, TypeName}}, State};
        #agent_type{} = AgentType ->
            Results = spawn_fleet_internal(AgentType, Count),
            SuccessfulPids = [Pid || {ok, Pid} <- Results],
            
            % Track instances
            ExistingInstances = maps:get(TypeName, State#state.type_instances, []),
            NewInstances = maps:put(TypeName, SuccessfulPids ++ ExistingInstances, State#state.type_instances),
            
            {reply, {ok, #{
                spawned => length(SuccessfulPids),
                failed => Count - length(SuccessfulPids),
                pids => SuccessfulPids
            }}, State#state{type_instances = NewInstances}}
    end;

handle_call({get_agent_type_metadata, TypeName}, _From, State) ->
    case maps:get(TypeName, State#state.agent_types, undefined) of
        undefined ->
            {reply, {error, {unknown_agent_type, TypeName}}, State};
        #agent_type{} = AgentType ->
            Metadata = #{
                name => AgentType#agent_type.name,
                description => AgentType#agent_type.description,
                capabilities => AgentType#agent_type.capabilities,
                tool_count => length(AgentType#agent_type.tools),
                specialization => AgentType#agent_type.specialization,
                resource_requirements => AgentType#agent_type.resource_requirements,
                active_instances => length(maps:get(TypeName, State#state.type_instances, []))
            },
            {reply, {ok, Metadata}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal Functions

register_type_internal(TypeName, TypeSpec, Tools, State) ->
    AgentType = #agent_type{
        name = TypeName,
        description = maps:get(description, TypeSpec, <<"Specialized agent type">>),
        capabilities = maps:get(capabilities, TypeSpec, []),
        tools = Tools,
        specialization = maps:get(specialization, TypeSpec, #{}),
        resource_requirements = maps:get(resource_requirements, TypeSpec, #{}),
        default_config = maps:get(default_config, TypeSpec, #{})
    },
    
    NewTypes = maps:put(TypeName, AgentType, State#state.agent_types),
    State#state{agent_types = NewTypes}.

create_agent_internal(#agent_type{name = TypeName, tools = Tools, default_config = DefaultConfig}, UserConfig) ->
    % Merge configurations
    FinalConfig = maps:merge(DefaultConfig, UserConfig),
    
    % Create agent with specialized tools
    AgentConfig = #{
        agent_type => TypeName,
        tools => Tools,
        config => FinalConfig
    },
    
    try
        % Start the agent instance
        {ok, AgentPid} = agent_instance:start_link(AgentConfig),
        
        % Register specialized tools for this agent
        register_agent_tools(AgentPid, Tools),
        
        % Register with dynamic router
        Capabilities = get_capabilities_from_tools(Tools),
        dynamic_agent_router:register_agent(
            generate_agent_id(TypeName), 
            AgentPid, 
            Capabilities
        ),
        
        {ok, AgentPid}
    catch
        Error:Reason ->
            {error, {agent_creation_failed, Error, Reason}}
    end.

spawn_fleet_internal(AgentType, Count) ->
    lists:map(fun(_) ->
        create_agent_internal(AgentType, #{})
    end, lists:seq(1, Count)).

register_agent_tools(AgentPid, Tools) ->
    lists:foreach(fun(Tool) ->
        ToolName = maps:get(<<"name">>, Tool),
        try
            enhanced_agent_tools:register_agent_tool(
                generate_agent_id_for_pid(AgentPid),
                ToolName,
                Tool
            )
        catch
            _:_ -> ok % Continue if registration fails
        end
    end, Tools).

get_capabilities_from_tools(Tools) ->
    lists:map(fun(Tool) ->
        ToolName = maps:get(<<"name">>, Tool),
        <<"tool:", ToolName/binary>>
    end, Tools).

generate_agent_id(TypeName) ->
    Timestamp = integer_to_binary(erlang:system_time(microsecond)),
    Random = integer_to_binary(rand:uniform(999999)),
    <<TypeName/binary, "_", Timestamp/binary, "_", Random/binary>>.

generate_agent_id_for_pid(Pid) ->
    PidStr = list_to_binary(pid_to_list(Pid)),
    <<"agent_", PidStr/binary>>.

%% Predefined Agent Types with Specialized Toolsets

get_predefined_agent_types() ->
    [
        % Reasoning Specialist - Uses most powerful reasoning models
        {<<"reasoning_specialist">>, #{
            description => <<"Advanced reasoning and complex problem-solving specialist">>,
            capabilities => [<<"complex_reasoning">>, <<"mathematical_analysis">>, <<"logical_deduction">>, <<"strategic_planning">>],
            specialization => #{
                domain => <<"reasoning">>,
                preferred_models => [<<"o3">>, <<"o4-mini">>, <<"gpt-4.1">>],
                task_complexity => <<"high">>,
                reasoning_depth => <<"comprehensive">>
            },
            resource_requirements => #{
                memory_mb => 1024,
                cpu_cores => 4,
                priority => <<"high">>
            },
            default_config => #{
                model => <<"o3">>,
                max_tokens => 8000,
                temperature => 0.1
            }
        }, get_reasoning_specialist_tools()},
        
        % Fast Response Agent - Uses fastest models for quick interactions
        {<<"fast_responder">>, #{
            description => <<"Quick response and real-time interaction specialist">>,
            capabilities => [<<"real_time_chat">>, <<"quick_queries">>, <<"instant_help">>, <<"basic_tasks">>],
            specialization => #{
                domain => <<"real_time">>,
                preferred_models => [<<"gpt-4.1-nano">>, <<"gpt-4.1-mini">>, <<"o4-mini">>],
                response_time => <<"minimal">>,
                optimization_target => <<"speed">>
            },
            resource_requirements => #{
                memory_mb => 256,
                cpu_cores => 1,
                priority => <<"normal">>
            },
            default_config => #{
                model => <<"gpt-4.1-nano">>,
                max_tokens => 2000,
                temperature => 0.3
            }
        }, get_fast_responder_tools()},
        
        % Data Analysis Specialist - Balanced model for analytical work
        {<<"data_analyst">>, #{
            description => <<"Data analysis and visualization specialist">>,
            capabilities => [<<"data_analysis">>, <<"visualization">>, <<"statistics">>, <<"reporting">>],
            specialization => #{
                domain => <<"data_science">>,
                preferred_models => [<<"gpt-4.1">>, <<"o4-mini">>, <<"gpt-4.1-mini">>],
                analysis_depth => <<"detailed">>,
                memory_type => <<"analytical">>
            },
            resource_requirements => #{
                memory_mb => 512,
                cpu_cores => 2,
                priority => <<"normal">>
            },
            default_config => #{
                model => <<"gpt-4.1">>,
                max_tokens => 4000,
                temperature => 0.2
            }
        }, get_data_analyst_tools()},
        
        % Code Development Specialist - Uses reasoning models for complex coding
        {<<"code_developer">>, #{
            description => <<"Software development and code analysis specialist">>,
            capabilities => [<<"code_analysis">>, <<"debugging">>, <<"refactoring">>, <<"testing">>, <<"documentation">>],
            specialization => #{
                domain => <<"software_engineering">>,
                preferred_models => [<<"o4-mini">>, <<"gpt-4.1">>, <<"o3">>],
                languages => [<<"erlang">>, <<"python">>, <<"javascript">>, <<"rust">>, <<"go">>],
                frameworks => [<<"otp">>, <<"react">>, <<"fastapi">>]
            },
            resource_requirements => #{
                memory_mb => 768,
                cpu_cores => 3,
                priority => <<"high">>
            },
            default_config => #{
                model => <<"o4-mini">>,
                max_tokens => 6000,
                temperature => 0.15
            }
        }, get_code_developer_tools()},
        
        % Research Assistant - Uses reasoning models for deep research
        {<<"researcher">>, #{
            description => <<"Research and knowledge discovery specialist">>,
            capabilities => [<<"web_research">>, <<"fact_checking">>, <<"knowledge_synthesis">>, <<"citation_management">>],
            specialization => #{
                domain => <<"research">>,
                preferred_models => [<<"o3">>, <<"gpt-4.1">>, <<"o4-mini">>],
                search_depth => <<"comprehensive">>,
                fact_check_rigor => <<"high">>
            },
            resource_requirements => #{
                memory_mb => 512,
                cpu_cores => 2,
                priority => <<"normal">>
            },
            default_config => #{
                model => <<"gpt-4.1">>,
                max_tokens => 5000,
                temperature => 0.25
            }
        }, get_researcher_tools()},
        
        % Communication Specialist - Cost-optimized for basic communication
        {<<"communicator">>, #{
            description => <<"Communication and collaboration specialist">>,
            capabilities => [<<"email_management">>, <<"document_generation">>, <<"translation">>, <<"summarization">>],
            specialization => #{
                domain => <<"communication">>,
                preferred_models => [<<"gpt-4.1-mini">>, <<"gpt-4.1-nano">>, <<"o4-mini">>],
                languages => [<<"english">>, <<"spanish">>, <<"french">>, <<"german">>, <<"chinese">>],
                optimization_target => <<"cost">>
            },
            resource_requirements => #{
                memory_mb => 256,
                cpu_cores => 1,
                priority => <<"low">>
            },
            default_config => #{
                model => <<"gpt-4.1-mini">>,
                max_tokens => 3000,
                temperature => 0.4
            }
        }, get_communicator_tools()},
        
        % System Administrator - Fast response for operational tasks
        {<<"sysadmin">>, #{
            description => <<"System administration and infrastructure specialist">>,
            capabilities => [<<"system_monitoring">>, <<"log_analysis">>, <<"automation">>, <<"security">>],
            specialization => #{
                domain => <<"infrastructure">>,
                preferred_models => [<<"gpt-4.1-mini">>, <<"o4-mini">>, <<"gpt-4.1">>],
                platforms => [<<"linux">>, <<"docker">>, <<"kubernetes">>, <<"aws">>],
                response_priority => <<"high">>
            },
            resource_requirements => #{
                memory_mb => 384,
                cpu_cores => 2,
                priority => <<"high">>
            },
            default_config => #{
                model => <<"gpt-4.1-mini">>,
                max_tokens => 3000,
                temperature => 0.2
            }
        }, get_sysadmin_tools()},
        
        % Financial Analyst - Reasoning models for complex analysis
        {<<"financial_analyst">>, #{
            description => <<"Financial analysis and market research specialist">>,
            capabilities => [<<"financial_modeling">>, <<"market_analysis">>, <<"risk_assessment">>, <<"portfolio_management">>],
            specialization => #{
                domain => <<"finance">>,
                preferred_models => [<<"o3">>, <<"gpt-4.1">>, <<"o4-mini">>],
                markets => [<<"stocks">>, <<"crypto">>, <<"bonds">>, <<"forex">>],
                analysis_complexity => <<"high">>
            },
            resource_requirements => #{
                memory_mb => 768,
                cpu_cores => 3,
                priority => <<"high">>
            },
            default_config => #{
                model => <<"o3">>,
                max_tokens => 6000,
                temperature => 0.1
            }
        }, get_financial_analyst_tools()},
        
        % Creative Assistant - Balanced model for creative tasks
        {<<"creative">>, #{
            description => <<"Creative content generation and design specialist">>,
            capabilities => [<<"content_creation">>, <<"image_generation">>, <<"design">>, <<"copywriting">>],
            specialization => #{
                domain => <<"creative">>,
                preferred_models => [<<"gpt-4.1">>, <<"o4-mini">>, <<"gpt-4.1-mini">>],
                content_types => [<<"blog">>, <<"social_media">>, <<"marketing">>, <<"technical_writing">>],
                creativity_level => <<"high">>
            },
            resource_requirements => #{
                memory_mb => 512,
                cpu_cores => 2,
                priority => <<"normal">>
            },
            default_config => #{
                model => <<"gpt-4.1">>,
                max_tokens => 4000,
                temperature => 0.7
            }
        }, get_creative_tools()},
        
        % Security Specialist - Reasoning models for threat analysis
        {<<"security_analyst">>, #{
            description => <<"Cybersecurity and threat analysis specialist">>,
            capabilities => [<<"threat_detection">>, <<"vulnerability_scanning">>, <<"incident_response">>, <<"compliance_checking">>],
            specialization => #{
                domain => <<"cybersecurity">>,
                preferred_models => [<<"o4-mini">>, <<"gpt-4.1">>, <<"o3">>],
                frameworks => [<<"nist">>, <<"iso27001">>, <<"owasp">>],
                analysis_depth => <<"comprehensive">>
            },
            resource_requirements => #{
                memory_mb => 640,
                cpu_cores => 3,
                priority => <<"high">>
            },
            default_config => #{
                model => <<"o4-mini">>,
                max_tokens => 5000,
                temperature => 0.1
            }
        }, get_security_analyst_tools()}
    ].

%% Tool Definitions for Each Specialist Type

get_data_analyst_tools() ->
    [
        #{
            <<"name">> => <<"analyze_dataset">>,
            <<"description">> => <<"Analyze datasets and generate statistical insights">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"data_source">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Path or URL to dataset">>},
                    <<"analysis_type">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"descriptive">>, <<"inferential">>, <<"predictive">>]},
                    <<"output_format">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"json">>, <<"csv">>, <<"html">>]}
                },
                <<"required">> => [<<"data_source">>, <<"analysis_type">>],
                <<"additionalProperties">> => false
            }
        },
        #{
            <<"name">> => <<"create_visualization">>,
            <<"description">> => <<"Create data visualizations and charts">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"data">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Data to visualize">>},
                    <<"chart_type">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"bar">>, <<"line">>, <<"scatter">>, <<"heatmap">>, <<"histogram">>]},
                    <<"title">> => #{<<"type">> => <<"string">>},
                    <<"save_path">> => #{<<"type">> => <<"string">>}
                },
                <<"required">> => [<<"data">>, <<"chart_type">>],
                <<"additionalProperties">> => false
            }
        },
        #{
            <<"name">> => <<"statistical_test">>,
            <<"description">> => <<"Perform statistical tests and hypothesis testing">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"test_type">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"t_test">>, <<"chi_square">>, <<"anova">>, <<"correlation">>]},
                    <<"data1">> => #{<<"type">> => <<"array">>, <<"items">> => #{<<"type">> => <<"number">>}},
                    <<"data2">> => #{<<"type">> => <<"array">>, <<"items">> => #{<<"type">> => <<"number">>}},
                    <<"alpha">> => #{<<"type">> => <<"number">>, <<"default">> => 0.05}
                },
                <<"required">> => [<<"test_type">>, <<"data1">>],
                <<"additionalProperties">> => false
            }
        }
    ].

get_code_developer_tools() ->
    [
        #{
            <<"name">> => <<"analyze_code">>,
            <<"description">> => <<"Analyze code for bugs, performance issues, and best practices">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"file_path">> => #{<<"type">> => <<"string">>},
                    <<"language">> => #{<<"type">> => <<"string">>},
                    <<"analysis_type">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"syntax">>, <<"security">>, <<"performance">>, <<"style">>]}
                },
                <<"required">> => [<<"file_path">>],
                <<"additionalProperties">> => false
            }
        },
        #{
            <<"name">> => <<"generate_tests">>,
            <<"description">> => <<"Generate unit tests for code">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"source_file">> => #{<<"type">> => <<"string">>},
                    <<"test_framework">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"eunit">>, <<"pytest">>, <<"jest">>, <<"junit">>]},
                    <<"coverage_target">> => #{<<"type">> => <<"number">>, <<"default">> => 80}
                },
                <<"required">> => [<<"source_file">>],
                <<"additionalProperties">> => false
            }
        },
        #{
            <<"name">> => <<"refactor_code">>,
            <<"description">> => <<"Refactor code for better maintainability">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"file_path">> => #{<<"type">> => <<"string">>},
                    <<"refactor_type">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"extract_function">>, <<"rename">>, <<"optimize">>, <<"modernize">>]},
                    <<"target_function">> => #{<<"type">> => <<"string">>}
                },
                <<"required">> => [<<"file_path">>, <<"refactor_type">>],
                <<"additionalProperties">> => false
            }
        },
        #{
            <<"name">> => <<"debug_issue">>,
            <<"description">> => <<"Debug code issues and suggest fixes">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"error_message">> => #{<<"type">> => <<"string">>},
                    <<"stack_trace">> => #{<<"type">> => <<"string">>},
                    <<"code_context">> => #{<<"type">> => <<"string">>},
                    <<"language">> => #{<<"type">> => <<"string">>}
                },
                <<"required">> => [<<"error_message">>],
                <<"additionalProperties">> => false
            }
        }
    ].

get_researcher_tools() ->
    [
        #{
            <<"name">> => <<"web_search_comprehensive">>,
            <<"description">> => <<"Perform comprehensive web research with multiple sources">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"query">> => #{<<"type">> => <<"string">>},
                    <<"search_depth">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"shallow">>, <<"medium">>, <<"deep">>]},
                    <<"source_types">> => #{<<"type">> => <<"array">>, <<"items">> => #{<<"type">> => <<"string">>}},
                    <<"max_results">> => #{<<"type">> => <<"number">>, <<"default">> => 20}
                },
                <<"required">> => [<<"query">>],
                <<"additionalProperties">> => false
            }
        },
        #{
            <<"name">> => <<"fact_check_claim">>,
            <<"description">> => <<"Fact-check claims against multiple authoritative sources">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"claim">> => #{<<"type">> => <<"string">>},
                    <<"rigor_level">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"basic">>, <<"thorough">>, <<"academic">>]},
                    <<"required_sources">> => #{<<"type">> => <<"number">>, <<"default">> => 3}
                },
                <<"required">> => [<<"claim">>],
                <<"additionalProperties">> => false
            }
        },
        #{
            <<"name">> => <<"synthesize_research">>,
            <<"description">> => <<"Synthesize research from multiple sources into coherent findings">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"sources">> => #{<<"type">> => <<"array">>, <<"items">> => #{<<"type">> => <<"string">>}},
                    <<"research_question">> => #{<<"type">> => <<"string">>},
                    <<"output_format">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"summary">>, <<"report">>, <<"bibliography">>]}
                },
                <<"required">> => [<<"sources">>, <<"research_question">>],
                <<"additionalProperties">> => false
            }
        }
    ].

get_communicator_tools() ->
    [
        #{
            <<"name">> => <<"compose_email">>,
            <<"description">> => <<"Compose professional emails with proper formatting">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"recipient">> => #{<<"type">> => <<"string">>},
                    <<"subject">> => #{<<"type">> => <<"string">>},
                    <<"content">> => #{<<"type">> => <<"string">>},
                    <<"tone">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"formal">>, <<"casual">>, <<"friendly">>, <<"urgent">>]},
                    <<"template">> => #{<<"type">> => <<"string">>}
                },
                <<"required">> => [<<"recipient">>, <<"subject">>, <<"content">>],
                <<"additionalProperties">> => false
            }
        },
        #{
            <<"name">> => <<"translate_text">>,
            <<"description">> => <<"Translate text between languages with context awareness">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"text">> => #{<<"type">> => <<"string">>},
                    <<"source_language">> => #{<<"type">> => <<"string">>},
                    <<"target_language">> => #{<<"type">> => <<"string">>},
                    <<"context">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Context for better translation">>}
                },
                <<"required">> => [<<"text">>, <<"target_language">>],
                <<"additionalProperties">> => false
            }
        },
        #{
            <<"name">> => <<"generate_document">>,
            <<"description">> => <<"Generate structured documents from templates">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"document_type">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"report">>, <<"proposal">>, <<"memo">>, <<"presentation">>]},
                    <<"content_outline">> => #{<<"type">> => <<"string">>},
                    <<"format">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"markdown">>, <<"html">>, <<"pdf">>, <<"docx">>]},
                    <<"template">> => #{<<"type">> => <<"string">>}
                },
                <<"required">> => [<<"document_type">>, <<"content_outline">>],
                <<"additionalProperties">> => false
            }
        }
    ].

get_sysadmin_tools() ->
    [
        #{
            <<"name">> => <<"monitor_system">>,
            <<"description">> => <<"Monitor system health and performance metrics">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"metrics">> => #{<<"type">> => <<"array">>, <<"items">> => #{<<"type">> => <<"string">>}},
                    <<"duration">> => #{<<"type">> => <<"string">>, <<"default">> => <<"1h">>},
                    <<"alert_thresholds">> => #{<<"type">> => <<"object">>}
                },
                <<"required">> => [<<"metrics">>],
                <<"additionalProperties">> => false
            }
        },
        #{
            <<"name">> => <<"analyze_logs">>,
            <<"description">> => <<"Analyze system logs for issues and patterns">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"log_files">> => #{<<"type">> => <<"array">>, <<"items">> => #{<<"type">> => <<"string">>}},
                    <<"time_range">> => #{<<"type">> => <<"string">>},
                    <<"search_patterns">> => #{<<"type">> => <<"array">>, <<"items">> => #{<<"type">> => <<"string">>}},
                    <<"severity_filter">> => #{<<"type">> => <<"string">>}
                },
                <<"required">> => [<<"log_files">>],
                <<"additionalProperties">> => false
            }
        },
        #{
            <<"name">> => <<"deploy_automation">>,
            <<"description">> => <<"Deploy automation scripts and configurations">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"script_path">> => #{<<"type">> => <<"string">>},
                    <<"target_hosts">> => #{<<"type">> => <<"array">>, <<"items">> => #{<<"type">> => <<"string">>}},
                    <<"execution_mode">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"immediate">>, <<"scheduled">>, <<"rollback">>]},
                    <<"dry_run">> => #{<<"type">> => <<"boolean">>, <<"default">> => true}
                },
                <<"required">> => [<<"script_path">>, <<"target_hosts">>],
                <<"additionalProperties">> => false
            }
        }
    ].

get_financial_analyst_tools() ->
    [
        #{
            <<"name">> => <<"analyze_portfolio">>,
            <<"description">> => <<"Analyze investment portfolio performance and risk">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"portfolio_data">> => #{<<"type">> => <<"string">>},
                    <<"analysis_period">> => #{<<"type">> => <<"string">>, <<"default">> => <<"1Y">>},
                    <<"benchmark">> => #{<<"type">> => <<"string">>, <<"default">> => <<"SP500">>},
                    <<"risk_metrics">> => #{<<"type">> => <<"array">>, <<"items">> => #{<<"type">> => <<"string">>}}
                },
                <<"required">> => [<<"portfolio_data">>],
                <<"additionalProperties">> => false
            }
        },
        #{
            <<"name">> => <<"market_research">>,
            <<"description">> => <<"Research market trends and economic indicators">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"symbols">> => #{<<"type">> => <<"array">>, <<"items">> => #{<<"type">> => <<"string">>}},
                    <<"research_depth">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"basic">>, <<"fundamental">>, <<"technical">>]},
                    <<"time_horizon">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"short">>, <<"medium">>, <<"long">>]}
                },
                <<"required">> => [<<"symbols">>],
                <<"additionalProperties">> => false
            }
        },
        #{
            <<"name">> => <<"risk_assessment">>,
            <<"description">> => <<"Assess financial risks and generate risk reports">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"asset_data">> => #{<<"type">> => <<"string">>},
                    <<"risk_model">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"var">>, <<"cvar">>, <<"monte_carlo">>]},
                    <<"confidence_level">> => #{<<"type">> => <<"number">>, <<"default">> => 0.95}
                },
                <<"required">> => [<<"asset_data">>],
                <<"additionalProperties">> => false
            }
        }
    ].

get_creative_tools() ->
    [
        #{
            <<"name">> => <<"generate_content">>,
            <<"description">> => <<"Generate creative content for various purposes">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"content_type">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"blog_post">>, <<"social_media">>, <<"ad_copy">>, <<"product_description">>]},
                    <<"topic">> => #{<<"type">> => <<"string">>},
                    <<"tone">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"professional">>, <<"casual">>, <<"humorous">>, <<"inspirational">>]},
                    <<"length">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"short">>, <<"medium">>, <<"long">>]},
                    <<"target_audience">> => #{<<"type">> => <<"string">>}
                },
                <<"required">> => [<<"content_type">>, <<"topic">>],
                <<"additionalProperties">> => false
            }
        },
        #{
            <<"name">> => <<"design_layout">>,
            <<"description">> => <<"Design layouts and visual concepts">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"design_type">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"web_page">>, <<"poster">>, <<"infographic">>, <<"presentation">>]},
                    <<"theme">> => #{<<"type">> => <<"string">>},
                    <<"color_scheme">> => #{<<"type">> => <<"string">>},
                    <<"content_elements">> => #{<<"type">> => <<"array">>, <<"items">> => #{<<"type">> => <<"string">>}}
                },
                <<"required">> => [<<"design_type">>],
                <<"additionalProperties">> => false
            }
        },
        #{
            <<"name">> => <<"brainstorm_ideas">>,
            <<"description">> => <<"Brainstorm creative ideas and concepts">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"domain">> => #{<<"type">> => <<"string">>},
                    <<"constraints">> => #{<<"type">> => <<"array">>, <<"items">> => #{<<"type">> => <<"string">>}},
                    <<"creativity_level">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"conservative">>, <<"moderate">>, <<"innovative">>, <<"radical">>]},
                    <<"num_ideas">> => #{<<"type">> => <<"number">>, <<"default">> => 10}
                },
                <<"required">> => [<<"domain">>],
                <<"additionalProperties">> => false
            }
        }
    ].

get_security_analyst_tools() ->
    [
        #{
            <<"name">> => <<"scan_vulnerabilities">>,
            <<"description">> => <<"Scan systems and applications for security vulnerabilities">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"target">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Target system or application">>},
                    <<"scan_type">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"network">>, <<"web_app">>, <<"code">>, <<"infrastructure">>]},
                    <<"depth">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"surface">>, <<"deep">>, <<"comprehensive">>]},
                    <<"compliance_framework">> => #{<<"type">> => <<"string">>}
                },
                <<"required">> => [<<"target">>, <<"scan_type">>],
                <<"additionalProperties">> => false
            }
        },
        #{
            <<"name">> => <<"analyze_threats">>,
            <<"description">> => <<"Analyze security threats and generate threat intelligence">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"threat_data">> => #{<<"type">> => <<"string">>},
                    <<"analysis_type">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"ioc">>, <<"attribution">>, <<"campaign">>, <<"ttps">>]},
                    <<"severity_threshold">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"low">>, <<"medium">>, <<"high">>, <<"critical">>]}
                },
                <<"required">> => [<<"threat_data">>],
                <<"additionalProperties">> => false
            }
        },
        #{
            <<"name">> => <<"compliance_check">>,
            <<"description">> => <<"Check compliance with security frameworks and standards">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"framework">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"nist">>, <<"iso27001">>, <<"pci_dss">>, <<"gdpr">>, <<"sox">>]},
                    <<"scope">> => #{<<"type">> => <<"string">>},
                    <<"evidence_required">> => #{<<"type">> => <<"boolean">>, <<"default">> => true}
                },
                <<"required">> => [<<"framework">>, <<"scope">>],
                <<"additionalProperties">> => false
            }
        }
    ].

%% Tool definitions for new agent types

get_reasoning_specialist_tools() ->
    [
        #{
            <<"name">> => <<"solve_complex_problem">>,
            <<"description">> => <<"Solve complex multi-step problems using advanced reasoning">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"problem_statement">> => #{<<"type">> => <<"string">>, <<"description">> => <<"The complex problem to solve">>},
                    <<"approach">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"analytical">>, <<"systematic">>, <<"creative">>, <<"logical">>]},
                    <<"constraints">> => #{<<"type">> => <<"array">>, <<"items">> => #{<<"type">> => <<"string">>}},
                    <<"required_depth">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"basic">>, <<"detailed">>, <<"comprehensive">>]}
                },
                <<"required">> => [<<"problem_statement">>],
                <<"additionalProperties">> => false
            }
        },
        #{
            <<"name">> => <<"mathematical_analysis">>,
            <<"description">> => <<"Perform advanced mathematical analysis and computations">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"expression">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Mathematical expression or problem">>},
                    <<"analysis_type">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"calculus">>, <<"algebra">>, <<"statistics">>, <<"optimization">>]},
                    <<"show_steps">> => #{<<"type">> => <<"boolean">>, <<"default">> => true}
                },
                <<"required">> => [<<"expression">>],
                <<"additionalProperties">> => false
            }
        },
        #{
            <<"name">> => <<"strategic_planning">>,
            <<"description">> => <<"Create comprehensive strategic plans with multiple scenarios">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"objective">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Main objective or goal">>},
                    <<"timeframe">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Timeline for the plan">>},
                    <<"resources">> => #{<<"type">> => <<"array">>, <<"items">> => #{<<"type">> => <<"string">>}},
                    <<"risk_tolerance">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"low">>, <<"medium">>, <<"high">>]}
                },
                <<"required">> => [<<"objective">>],
                <<"additionalProperties">> => false
            }
        }
    ].

get_fast_responder_tools() ->
    [
        #{
            <<"name">> => <<"quick_answer">>,
            <<"description">> => <<"Provide quick answers to simple questions">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"question">> => #{<<"type">> => <<"string">>, <<"description">> => <<"The question to answer quickly">>},
                    <<"context">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Optional context for the question">>}
                },
                <<"required">> => [<<"question">>],
                <<"additionalProperties">> => false
            }
        },
        #{
            <<"name">> => <<"instant_help">>,
            <<"description">> => <<"Provide immediate assistance with basic tasks">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"task_type">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"explanation">>, <<"direction">>, <<"definition">>, <<"example">>]},
                    <<"topic">> => #{<<"type">> => <<"string">>, <<"description">> => <<"The topic requiring help">>}
                },
                <<"required">> => [<<"task_type">>, <<"topic">>],
                <<"additionalProperties">> => false
            }
        }
    ].