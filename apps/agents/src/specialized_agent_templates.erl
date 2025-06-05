%%%-------------------------------------------------------------------
%%% @doc Specialized AI Agent Templates
%%% Templates for creating diverse AI agents with specialized functions
%%% @end
%%%-------------------------------------------------------------------
-module(specialized_agent_templates).

-export([
    get_agent_templates/0,
    get_template/1,
    create_agents_from_templates/1,
    get_specialized_tools/1,
    %% Tool functions
    get_data_science_tools/0,
    get_business_analysis_tools/0,
    get_financial_tools/0,
    get_development_tools/0,
    get_devops_tools/0,
    get_security_tools/0,
    get_content_tools/0,
    get_design_tools/0,
    get_video_tools/0,
    get_support_tools/0,
    get_sales_tools/0,
    get_marketing_tools/0,
    get_research_tools/0,
    get_language_tools/0,
    get_coding_mentor_tools/0,
    get_legal_tools/0,
    get_medical_tools/0,
    get_project_management_tools/0,
    get_ai_research_tools/0,
    get_computer_vision_tools/0,
    get_nlp_tools/0
]).

%% Agent template with specialized tools and capabilities
-type agent_template() :: #{
    name := binary(),
    type := binary(),
    description := binary(),
    model := binary(),
    temperature := float(),
    max_tokens := integer(),
    tools := [map()],
    system_prompt := binary(),
    capabilities := [atom()],
    specialization := atom(),
    complexity_level := low | medium | high | expert
}.

%% @doc Get all available agent templates
-spec get_agent_templates() -> [agent_template()].
get_agent_templates() ->
    [
        %% Data & Analytics Agents
        #{
            name => <<"Data Scientist">>,
            type => <<"ai">>,
            description => <<"Advanced data analysis and machine learning specialist">>,
            model => <<"gpt-4o">>,
            temperature => 0.3,
            max_tokens => 4000,
            tools => get_data_science_tools(),
            system_prompt => <<"You are a data scientist with expertise in statistical analysis, machine learning, and data visualization. You can analyze datasets, build predictive models, and provide insights.">>,
            capabilities => [data_analysis, machine_learning, statistics, visualization],
            specialization => data_science,
            complexity_level => expert
        },
        
        #{
            name => <<"Business Analyst">>,
            type => <<"ai">>,
            description => <<"Business intelligence and analytics specialist">>,
            model => <<"gpt-4o-mini">>,
            temperature => 0.4,
            max_tokens => 3000,
            tools => get_business_analysis_tools(),
            system_prompt => <<"You are a business analyst specializing in KPI analysis, market research, and business intelligence. You help organizations make data-driven decisions.">>,
            capabilities => [business_intelligence, market_research, kpi_analysis],
            specialization => business_analysis,
            complexity_level => high
        },
        
        #{
            name => <<"Financial Analyst">>,
            type => <<"ai">>,
            description => <<"Financial modeling and investment analysis expert">>,
            model => <<"gpt-4o">>,
            temperature => 0.2,
            max_tokens => 3500,
            tools => get_financial_tools(),
            system_prompt => <<"You are a financial analyst with expertise in investment analysis, financial modeling, risk assessment, and portfolio management.">>,
            capabilities => [financial_modeling, risk_analysis, investment_analysis],
            specialization => finance,
            complexity_level => expert
        },
        
        %% Development & Engineering Agents
        #{
            name => <<"Full Stack Developer">>,
            type => <<"ai">>,
            description => <<"End-to-end web application development specialist">>,
            model => <<"gpt-4o">>,
            temperature => 0.3,
            max_tokens => 4000,
            tools => get_development_tools(),
            system_prompt => <<"You are a full stack developer proficient in frontend, backend, and database technologies. You can build complete web applications and APIs.">>,
            capabilities => [web_development, api_development, database_design],
            specialization => software_development,
            complexity_level => expert
        },
        
        #{
            name => <<"DevOps Engineer">>,
            type => <<"ai">>,
            description => <<"Infrastructure automation and deployment specialist">>,
            model => <<"gpt-4o">>,
            temperature => 0.3,
            max_tokens => 3500,
            tools => get_devops_tools(),
            system_prompt => <<"You are a DevOps engineer specializing in CI/CD, infrastructure as code, containerization, and cloud deployment.">>,
            capabilities => [infrastructure, automation, deployment, monitoring],
            specialization => devops,
            complexity_level => expert
        },
        
        #{
            name => <<"Security Analyst">>,
            type => <<"ai">>,
            description => <<"Cybersecurity and threat analysis specialist">>,
            model => <<"gpt-4o">>,
            temperature => 0.2,
            max_tokens => 3500,
            tools => get_security_tools(),
            system_prompt => <<"You are a cybersecurity analyst specializing in threat detection, vulnerability assessment, and security best practices.">>,
            capabilities => [security_analysis, threat_detection, vulnerability_assessment],
            specialization => cybersecurity,
            complexity_level => expert
        },
        
        %% Content & Creative Agents
        #{
            name => <<"Content Writer">>,
            type => <<"ai">>,
            description => <<"Professional content creation and copywriting">>,
            model => <<"gpt-4o-mini">>,
            temperature => 0.7,
            max_tokens => 3000,
            tools => get_content_tools(),
            system_prompt => <<"You are a professional content writer specializing in blogs, articles, marketing copy, and technical documentation.">>,
            capabilities => [content_creation, copywriting, seo_optimization],
            specialization => content_writing,
            complexity_level => high
        },
        
        #{
            name => <<"Creative Designer">>,
            type => <<"ai">>,
            description => <<"UI/UX design and creative visual solutions">>,
            model => <<"gpt-4o">>,
            temperature => 0.6,
            max_tokens => 3000,
            tools => get_design_tools(),
            system_prompt => <<"You are a creative designer specializing in UI/UX design, visual branding, and user experience optimization.">>,
            capabilities => [ui_design, ux_design, visual_design],
            specialization => design,
            complexity_level => high
        },
        
        #{
            name => <<"Video Producer">>,
            type => <<"ai">>,
            description => <<"Video editing and production specialist">>,
            model => <<"gpt-4o-mini">>,
            temperature => 0.5,
            max_tokens => 2500,
            tools => get_video_tools(),
            system_prompt => <<"You are a video producer specializing in video editing, motion graphics, and multimedia content creation.">>,
            capabilities => [video_editing, motion_graphics, multimedia],
            specialization => video_production,
            complexity_level => medium
        },
        
        %% Customer & Sales Agents
        #{
            name => <<"Customer Support">>,
            type => <<"ai">>,
            description => <<"24/7 customer service and support specialist">>,
            model => <<"gpt-4o-mini">>,
            temperature => 0.4,
            max_tokens => 2000,
            tools => get_support_tools(),
            system_prompt => <<"You are a customer support specialist providing helpful, empathetic, and solution-oriented assistance to customers.">>,
            capabilities => [customer_service, problem_solving, communication],
            specialization => customer_support,
            complexity_level => medium
        },
        
        #{
            name => <<"Sales Assistant">>,
            type => <<"ai">>,
            description => <<"Sales automation and lead qualification">>,
            model => <<"gpt-4o-mini">>,
            temperature => 0.5,
            max_tokens => 2500,
            tools => get_sales_tools(),
            system_prompt => <<"You are a sales assistant specializing in lead qualification, customer relationship management, and sales process optimization.">>,
            capabilities => [lead_qualification, crm, sales_analytics],
            specialization => sales,
            complexity_level => medium
        },
        
        #{
            name => <<"Marketing Specialist">>,
            type => <<"ai">>,
            description => <<"Digital marketing and campaign optimization">>,
            model => <<"gpt-4o">>,
            temperature => 0.6,
            max_tokens => 3000,
            tools => get_marketing_tools(),
            system_prompt => <<"You are a marketing specialist focusing on digital campaigns, social media strategy, and marketing analytics.">>,
            capabilities => [digital_marketing, social_media, campaign_optimization],
            specialization => marketing,
            complexity_level => high
        },
        
        %% Research & Education Agents
        #{
            name => <<"Research Assistant">>,
            type => <<"ai">>,
            description => <<"Academic research and literature analysis">>,
            model => <<"gpt-4o">>,
            temperature => 0.3,
            max_tokens => 4000,
            tools => get_research_tools(),
            system_prompt => <<"You are a research assistant specializing in academic research, literature reviews, and data synthesis.">>,
            capabilities => [academic_research, literature_analysis, data_synthesis],
            specialization => research,
            complexity_level => expert
        },
        
        #{
            name => <<"Language Tutor">>,
            type => <<"ai">>,
            description => <<"Multilingual education and language learning">>,
            model => <<"gpt-4o-mini">>,
            temperature => 0.5,
            max_tokens => 2500,
            tools => get_language_tools(),
            system_prompt => <<"You are a language tutor specializing in multiple languages, grammar, pronunciation, and cultural context.">>,
            capabilities => [language_teaching, translation, cultural_education],
            specialization => education,
            complexity_level => medium
        },
        
        #{
            name => <<"Code Mentor">>,
            type => <<"ai">>,
            description => <<"Programming education and code review">>,
            model => <<"gpt-4o">>,
            temperature => 0.3,
            max_tokens => 3500,
            tools => get_coding_mentor_tools(),
            system_prompt => <<"You are a coding mentor specializing in programming education, code reviews, and software engineering best practices.">>,
            capabilities => [code_review, programming_education, mentoring],
            specialization => programming_education,
            complexity_level => expert
        },
        
        %% Specialized Domain Agents
        #{
            name => <<"Legal Assistant">>,
            type => <<"ai">>,
            description => <<"Legal research and document analysis">>,
            model => <<"gpt-4o">>,
            temperature => 0.2,
            max_tokens => 4000,
            tools => get_legal_tools(),
            system_prompt => <<"You are a legal assistant specializing in legal research, document analysis, and regulatory compliance.">>,
            capabilities => [legal_research, document_analysis, compliance],
            specialization => legal,
            complexity_level => expert
        },
        
        #{
            name => <<"Medical Assistant">>,
            type => <<"ai">>,
            description => <<"Healthcare information and medical research">>,
            model => <<"gpt-4o">>,
            temperature => 0.2,
            max_tokens => 3500,
            tools => get_medical_tools(),
            system_prompt => <<"You are a medical assistant providing general health information and medical research support. Always recommend consulting healthcare professionals for medical advice.">>,
            capabilities => [medical_research, health_information, symptom_analysis],
            specialization => healthcare,
            complexity_level => expert
        },
        
        #{
            name => <<"Project Manager">>,
            type => <<"ai">>,
            description => <<"Project planning and resource management">>,
            model => <<"gpt-4o-mini">>,
            temperature => 0.4,
            max_tokens => 3000,
            tools => get_project_management_tools(),
            system_prompt => <<"You are a project manager specializing in agile methodologies, resource allocation, and timeline management.">>,
            capabilities => [project_planning, resource_management, agile_methodologies],
            specialization => project_management,
            complexity_level => high
        },
        
        %% AI/ML Specialist Agents
        #{
            name => <<"AI Researcher">>,
            type => <<"ai">>,
            description => <<"Advanced AI/ML research and model development">>,
            model => <<"gpt-4o">>,
            temperature => 0.3,
            max_tokens => 4000,
            tools => get_ai_research_tools(),
            system_prompt => <<"You are an AI researcher specializing in machine learning algorithms, neural networks, and AI system architecture.">>,
            capabilities => [ai_research, model_development, neural_networks],
            specialization => ai_research,
            complexity_level => expert
        },
        
        #{
            name => <<"Computer Vision Engineer">>,
            type => <<"ai">>,
            description => <<"Image processing and computer vision specialist">>,
            model => <<"gpt-4o">>,
            temperature => 0.3,
            max_tokens => 3500,
            tools => get_computer_vision_tools(),
            system_prompt => <<"You are a computer vision engineer specializing in image processing, object detection, and visual recognition systems.">>,
            capabilities => [image_processing, object_detection, visual_recognition],
            specialization => computer_vision,
            complexity_level => expert
        },
        
        #{
            name => <<"NLP Engineer">>,
            type => <<"ai">>,
            description => <<"Natural language processing and text analysis">>,
            model => <<"gpt-4o">>,
            temperature => 0.3,
            max_tokens => 3500,
            tools => get_nlp_tools(),
            system_prompt => <<"You are an NLP engineer specializing in text analysis, language models, and conversational AI systems.">>,
            capabilities => [text_analysis, language_modeling, sentiment_analysis],
            specialization => nlp,
            complexity_level => expert
        }
    ].

%% Tool definitions for different specializations

get_data_science_tools() ->
    [
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"analyze_dataset">>,
            <<"description">> => <<"Analyze a dataset and provide statistical insights">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"data">> => #{<<"type">> => <<"string">>, <<"description">> => <<"CSV data or data description">>},
                    <<"analysis_type">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"descriptive">>, <<"predictive">>, <<"correlation">>]}
                },
                <<"required">> => [<<"data">>, <<"analysis_type">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        },
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"build_ml_model">>,
            <<"description">> => <<"Build and train a machine learning model">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"algorithm">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"linear_regression">>, <<"random_forest">>, <<"neural_network">>]},
                    <<"features">> => #{<<"type">> => <<"array">>, <<"items">> => #{<<"type">> => <<"string">>}},
                    <<"target">> => #{<<"type">> => <<"string">>}
                },
                <<"required">> => [<<"algorithm">>, <<"features">>, <<"target">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        },
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"create_visualization">>,
            <<"description">> => <<"Create data visualizations and charts">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"chart_type">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"bar">>, <<"line">>, <<"scatter">>, <<"heatmap">>]},
                    <<"data_columns">> => #{<<"type">> => <<"array">>, <<"items">> => #{<<"type">> => <<"string">>}}
                },
                <<"required">> => [<<"chart_type">>, <<"data_columns">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        }
    ].

get_business_analysis_tools() ->
    [
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"calculate_kpis">>,
            <<"description">> => <<"Calculate key performance indicators">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"metrics">> => #{<<"type">> => <<"array">>, <<"items">> => #{<<"type">> => <<"string">>}},
                    <<"period">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"daily">>, <<"weekly">>, <<"monthly">>, <<"quarterly">>]}
                },
                <<"required">> => [<<"metrics">>, <<"period">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        },
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"market_research">>,
            <<"description">> => <<"Conduct market research and competitive analysis">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"industry">> => #{<<"type">> => <<"string">>},
                    <<"competitors">> => #{<<"type">> => <<"array">>, <<"items">> => #{<<"type">> => <<"string">>}},
                    <<"research_type">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"competitive">>, <<"market_size">>, <<"trends">>]}
                },
                <<"required">> => [<<"industry">>, <<"research_type">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        }
    ].

get_financial_tools() ->
    [
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"financial_analysis">>,
            <<"description">> => <<"Perform financial statement analysis">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"company">> => #{<<"type">> => <<"string">>},
                    <<"analysis_type">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"ratio">>, <<"trend">>, <<"comparative">>]},
                    <<"financial_data">> => #{<<"type">> => <<"string">>}
                },
                <<"required">> => [<<"company">>, <<"analysis_type">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        },
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"risk_assessment">>,
            <<"description">> => <<"Assess investment risk and portfolio optimization">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"portfolio">> => #{<<"type">> => <<"array">>, <<"items">> => #{<<"type">> => <<"string">>}},
                    <<"risk_tolerance">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"low">>, <<"medium">>, <<"high">>]}
                },
                <<"required">> => [<<"portfolio">>, <<"risk_tolerance">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        }
    ].

get_development_tools() ->
    [
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"code_review">>,
            <<"description">> => <<"Review code for best practices and issues">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"code">> => #{<<"type">> => <<"string">>},
                    <<"language">> => #{<<"type">> => <<"string">>},
                    <<"review_type">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"security">>, <<"performance">>, <<"maintainability">>]}
                },
                <<"required">> => [<<"code">>, <<"language">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        },
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"generate_api">>,
            <<"description">> => <<"Generate REST API endpoints and documentation">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"resource">> => #{<<"type">> => <<"string">>},
                    <<"operations">> => #{<<"type">> => <<"array">>, <<"items">> => #{<<"type">> => <<"string">>}},
                    <<"framework">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"express">>, <<"fastapi">>, <<"spring">>, <<"django">>]}
                },
                <<"required">> => [<<"resource">>, <<"operations">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        },
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"database_design">>,
            <<"description">> => <<"Design database schema and relationships">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"entities">> => #{<<"type">> => <<"array">>, <<"items">> => #{<<"type">> => <<"string">>}},
                    <<"database_type">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"sql">>, <<"nosql">>, <<"graph">>]}
                },
                <<"required">> => [<<"entities">>, <<"database_type">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        }
    ].

get_devops_tools() ->
    [
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"deploy_application">>,
            <<"description">> => <<"Deploy application to cloud infrastructure">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"app_name">> => #{<<"type">> => <<"string">>},
                    <<"environment">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"dev">>, <<"staging">>, <<"prod">>]},
                    <<"platform">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"aws">>, <<"gcp">>, <<"azure">>, <<"kubernetes">>]}
                },
                <<"required">> => [<<"app_name">>, <<"environment">>, <<"platform">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        },
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"setup_monitoring">>,
            <<"description">> => <<"Configure monitoring and alerting">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"service">> => #{<<"type">> => <<"string">>},
                    <<"metrics">> => #{<<"type">> => <<"array">>, <<"items">> => #{<<"type">> => <<"string">>}},
                    <<"alert_thresholds">> => #{<<"type">> => <<"object">>}
                },
                <<"required">> => [<<"service">>, <<"metrics">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        }
    ].

get_security_tools() ->
    [
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"security_scan">>,
            <<"description">> => <<"Perform security vulnerability scanning">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"target">> => #{<<"type">> => <<"string">>},
                    <<"scan_type">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"network">>, <<"web_app">>, <<"code">>]},
                    <<"severity_filter">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"low">>, <<"medium">>, <<"high">>, <<"critical">>]}
                },
                <<"required">> => [<<"target">>, <<"scan_type">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        },
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"threat_analysis">>,
            <<"description">> => <<"Analyze potential security threats">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"threat_data">> => #{<<"type">> => <<"string">>},
                    <<"analysis_depth">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"basic">>, <<"detailed">>, <<"comprehensive">>]}
                },
                <<"required">> => [<<"threat_data">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        }
    ].

get_content_tools() ->
    [
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"create_content">>,
            <<"description">> => <<"Create various types of content">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"content_type">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"blog">>, <<"article">>, <<"social_media">>, <<"documentation">>]},
                    <<"topic">> => #{<<"type">> => <<"string">>},
                    <<"target_audience">> => #{<<"type">> => <<"string">>},
                    <<"tone">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"professional">>, <<"casual">>, <<"technical">>, <<"creative">>]}
                },
                <<"required">> => [<<"content_type">>, <<"topic">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        },
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"seo_optimize">>,
            <<"description">> => <<"Optimize content for search engines">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"content">> => #{<<"type">> => <<"string">>},
                    <<"keywords">> => #{<<"type">> => <<"array">>, <<"items">> => #{<<"type">> => <<"string">>}},
                    <<"optimization_level">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"basic">>, <<"advanced">>]}
                },
                <<"required">> => [<<"content">>, <<"keywords">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        }
    ].

get_design_tools() ->
    [
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"create_wireframe">>,
            <<"description">> => <<"Create UI wireframes and mockups">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"page_type">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"landing">>, <<"dashboard">>, <<"form">>, <<"profile">>]},
                    <<"device">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"desktop">>, <<"tablet">>, <<"mobile">>]},
                    <<"style">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"minimal">>, <<"modern">>, <<"classic">>]}
                },
                <<"required">> => [<<"page_type">>, <<"device">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        },
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"design_system">>,
            <<"description">> => <<"Create design system components">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"brand_colors">> => #{<<"type">> => <<"array">>, <<"items">> => #{<<"type">> => <<"string">>}},
                    <<"typography">> => #{<<"type">> => <<"string">>},
                    <<"component_type">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"buttons">>, <<"forms">>, <<"cards">>, <<"navigation">>]}
                },
                <<"required">> => [<<"component_type">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        }
    ].

get_video_tools() ->
    [
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"video_editing">>,
            <<"description">> => <<"Edit and process video content">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"video_url">> => #{<<"type">> => <<"string">>},
                    <<"edit_type">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"trim">>, <<"merge">>, <<"effects">>, <<"color_grade">>]},
                    <<"output_format">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"mp4">>, <<"avi">>, <<"mov">>]}
                },
                <<"required">> => [<<"video_url">>, <<"edit_type">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        }
    ].

get_support_tools() ->
    [
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"ticket_analysis">>,
            <<"description">> => <<"Analyze and categorize support tickets">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"ticket_content">> => #{<<"type">> => <<"string">>},
                    <<"priority">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"low">>, <<"medium">>, <<"high">>, <<"urgent">>]},
                    <<"category">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"technical">>, <<"billing">>, <<"general">>]}
                },
                <<"required">> => [<<"ticket_content">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        },
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"knowledge_search">>,
            <<"description">> => <<"Search knowledge base for solutions">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"query">> => #{<<"type">> => <<"string">>},
                    <<"search_scope">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"faq">>, <<"documentation">>, <<"troubleshooting">>]}
                },
                <<"required">> => [<<"query">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        }
    ].

get_sales_tools() ->
    [
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"lead_scoring">>,
            <<"description">> => <<"Score and qualify leads">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"lead_data">> => #{<<"type">> => <<"object">>},
                    <<"scoring_criteria">> => #{<<"type">> => <<"array">>, <<"items">> => #{<<"type">> => <<"string">>}}
                },
                <<"required">> => [<<"lead_data">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        },
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"sales_forecast">>,
            <<"description">> => <<"Generate sales forecasts and pipeline analysis">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"pipeline_data">> => #{<<"type">> => <<"string">>},
                    <<"forecast_period">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"monthly">>, <<"quarterly">>, <<"yearly">>]}
                },
                <<"required">> => [<<"pipeline_data">>, <<"forecast_period">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        }
    ].

get_marketing_tools() ->
    [
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"campaign_analysis">>,
            <<"description">> => <<"Analyze marketing campaign performance">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"campaign_data">> => #{<<"type">> => <<"string">>},
                    <<"metrics">> => #{<<"type">> => <<"array">>, <<"items">> => #{<<"type">> => <<"string">>}},
                    <<"platform">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"facebook">>, <<"google">>, <<"linkedin">>, <<"twitter">>]}
                },
                <<"required">> => [<<"campaign_data">>, <<"metrics">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        },
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"audience_segmentation">>,
            <<"description">> => <<"Segment audience for targeted marketing">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"customer_data">> => #{<<"type">> => <<"string">>},
                    <<"segmentation_criteria">> => #{<<"type">> => <<"array">>, <<"items">> => #{<<"type">> => <<"string">>}}
                },
                <<"required">> => [<<"customer_data">>, <<"segmentation_criteria">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        }
    ].

get_research_tools() ->
    [
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"literature_review">>,
            <<"description">> => <<"Conduct systematic literature review">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"research_topic">> => #{<<"type">> => <<"string">>},
                    <<"databases">> => #{<<"type">> => <<"array">>, <<"items">> => #{<<"type">> => <<"string">>}},
                    <<"time_range">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"1_year">>, <<"5_years">>, <<"10_years">>, <<"all">>]}
                },
                <<"required">> => [<<"research_topic">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        },
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"data_synthesis">>,
            <<"description">> => <<"Synthesize research data and findings">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"research_papers">> => #{<<"type">> => <<"array">>, <<"items">> => #{<<"type">> => <<"string">>}},
                    <<"synthesis_method">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"meta_analysis">>, <<"narrative">>, <<"systematic">>]}
                },
                <<"required">> => [<<"research_papers">>, <<"synthesis_method">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        }
    ].

get_language_tools() ->
    [
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"language_lesson">>,
            <<"description">> => <<"Create interactive language lessons">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"language">> => #{<<"type">> => <<"string">>},
                    <<"skill_level">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"beginner">>, <<"intermediate">>, <<"advanced">>]},
                    <<"lesson_type">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"vocabulary">>, <<"grammar">>, <<"conversation">>, <<"pronunciation">>]}
                },
                <<"required">> => [<<"language">>, <<"skill_level">>, <<"lesson_type">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        },
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"translation_service">>,
            <<"description">> => <<"Translate text with cultural context">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"text">> => #{<<"type">> => <<"string">>},
                    <<"source_language">> => #{<<"type">> => <<"string">>},
                    <<"target_language">> => #{<<"type">> => <<"string">>},
                    <<"include_context">> => #{<<"type">> => <<"boolean">>}
                },
                <<"required">> => [<<"text">>, <<"source_language">>, <<"target_language">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        }
    ].

get_coding_mentor_tools() ->
    [
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"code_explanation">>,
            <<"description">> => <<"Explain code concepts and implementations">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"code">> => #{<<"type">> => <<"string">>},
                    <<"language">> => #{<<"type">> => <<"string">>},
                    <<"explanation_level">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"beginner">>, <<"intermediate">>, <<"advanced">>]}
                },
                <<"required">> => [<<"code">>, <<"language">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        },
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"coding_exercise">>,
            <<"description">> => <<"Create coding exercises and challenges">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"topic">> => #{<<"type">> => <<"string">>},
                    <<"difficulty">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"easy">>, <<"medium">>, <<"hard">>]},
                    <<"language">> => #{<<"type">> => <<"string">>}
                },
                <<"required">> => [<<"topic">>, <<"difficulty">>, <<"language">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        }
    ].

get_legal_tools() ->
    [
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"legal_research">>,
            <<"description">> => <<"Research legal precedents and statutes">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"legal_issue">> => #{<<"type">> => <<"string">>},
                    <<"jurisdiction">> => #{<<"type">> => <<"string">>},
                    <<"case_type">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"civil">>, <<"criminal">>, <<"corporate">>, <<"intellectual_property">>]}
                },
                <<"required">> => [<<"legal_issue">>, <<"jurisdiction">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        },
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"document_analysis">>,
            <<"description">> => <<"Analyze legal documents and contracts">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"document_type">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"contract">>, <<"agreement">>, <<"policy">>, <<"compliance">>]},
                    <<"analysis_focus">> => #{<<"type">> => <<"array">>, <<"items">> => #{<<"type">> => <<"string">>}}
                },
                <<"required">> => [<<"document_type">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        }
    ].

get_medical_tools() ->
    [
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"medical_research">>,
            <<"description">> => <<"Research medical literature and studies">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"condition">> => #{<<"type">> => <<"string">>},
                    <<"research_type">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"treatment">>, <<"diagnosis">>, <<"prevention">>, <<"clinical_trials">>]},
                    <<"evidence_level">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"systematic_review">>, <<"randomized_trial">>, <<"cohort_study">>]}
                },
                <<"required">> => [<<"condition">>, <<"research_type">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        },
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"symptom_analysis">>,
            <<"description">> => <<"Analyze symptoms and provide general health information">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"symptoms">> => #{<<"type">> => <<"array">>, <<"items">> => #{<<"type">> => <<"string">>}},
                    <<"patient_demographics">> => #{<<"type">> => <<"object">>},
                    <<"analysis_depth">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"basic">>, <<"detailed">>]}
                },
                <<"required">> => [<<"symptoms">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        }
    ].

get_project_management_tools() ->
    [
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"project_planning">>,
            <<"description">> => <<"Create project plans and timelines">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"project_name">> => #{<<"type">> => <<"string">>},
                    <<"deliverables">> => #{<<"type">> => <<"array">>, <<"items">> => #{<<"type">> => <<"string">>}},
                    <<"methodology">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"agile">>, <<"waterfall">>, <<"hybrid">>]},
                    <<"timeline">> => #{<<"type">> => <<"string">>}
                },
                <<"required">> => [<<"project_name">>, <<"deliverables">>, <<"methodology">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        },
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"resource_allocation">>,
            <<"description">> => <<"Allocate resources and manage capacity">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"team_members">> => #{<<"type">> => <<"array">>, <<"items">> => #{<<"type">> => <<"string">>}},
                    <<"tasks">> => #{<<"type">> => <<"array">>, <<"items">> => #{<<"type">> => <<"string">>}},
                    <<"constraints">> => #{<<"type">> => <<"object">>}
                },
                <<"required">> => [<<"team_members">>, <<"tasks">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        }
    ].

get_ai_research_tools() ->
    [
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"model_architecture">>,
            <<"description">> => <<"Design neural network architectures">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"problem_type">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"classification">>, <<"regression">>, <<"generation">>, <<"reinforcement">>]},
                    <<"data_type">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"tabular">>, <<"image">>, <<"text">>, <<"audio">>]},
                    <<"model_complexity">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"simple">>, <<"moderate">>, <<"complex">>]}
                },
                <<"required">> => [<<"problem_type">>, <<"data_type">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        },
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"hyperparameter_tuning">>,
            <<"description">> => <<"Optimize model hyperparameters">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"model_type">> => #{<<"type">> => <<"string">>},
                    <<"optimization_method">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"grid_search">>, <<"random_search">>, <<"bayesian">>]},
                    <<"metric">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"accuracy">>, <<"f1">>, <<"mse">>, <<"auc">>]}
                },
                <<"required">> => [<<"model_type">>, <<"optimization_method">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        }
    ].

get_computer_vision_tools() ->
    [
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"image_processing">>,
            <<"description">> => <<"Process and analyze images">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"image_url">> => #{<<"type">> => <<"string">>},
                    <<"processing_type">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"object_detection">>, <<"image_classification">>, <<"segmentation">>, <<"feature_extraction">>]},
                    <<"model_type">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"cnn">>, <<"yolo">>, <<"rcnn">>, <<"transformer">>]}
                },
                <<"required">> => [<<"image_url">>, <<"processing_type">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        },
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"video_analysis">>,
            <<"description">> => <<"Analyze video content and motion">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"video_url">> => #{<<"type">> => <<"string">>},
                    <<"analysis_type">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"motion_detection">>, <<"object_tracking">>, <<"action_recognition">>]}
                },
                <<"required">> => [<<"video_url">>, <<"analysis_type">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        }
    ].

get_nlp_tools() ->
    [
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"text_analysis">>,
            <<"description">> => <<"Analyze text for various linguistic features">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"text">> => #{<<"type">> => <<"string">>},
                    <<"analysis_type">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"sentiment">>, <<"entities">>, <<"topics">>, <<"summarization">>]},
                    <<"language">> => #{<<"type">> => <<"string">>}
                },
                <<"required">> => [<<"text">>, <<"analysis_type">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        },
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<"language_modeling">>,
            <<"description">> => <<"Build and fine-tune language models">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"model_type">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"bert">>, <<"gpt">>, <<"t5">>, <<"transformer">>]},
                    <<"task">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"classification">>, <<"generation">>, <<"translation">>, <<"summarization">>]},
                    <<"domain">> => #{<<"type">> => <<"string">>}
                },
                <<"required">> => [<<"model_type">>, <<"task">>],
                <<"additionalProperties">> => false
            },
            <<"strict">> => true
        }
    ].

%% @doc Get a specific agent template by name
-spec get_template(binary()) -> {ok, agent_template()} | {error, not_found}.
get_template(Name) ->
    Templates = get_agent_templates(),
    case lists:filter(fun(Template) -> 
        maps:get(name, Template) =:= Name 
    end, Templates) of
        [] -> {error, not_found};
        [Template | _] -> {ok, Template}
    end.

%% @doc Create multiple agents from templates
-spec create_agents_from_templates([binary()]) -> {ok, [map()]} | {error, term()}.
create_agents_from_templates(TemplateNames) ->
    try
        Agents = lists:map(fun(Name) ->
            case get_template(Name) of
                {ok, Template} ->
                    create_agent_from_template(Template);
                {error, not_found} ->
                    throw({template_not_found, Name})
            end
        end, TemplateNames),
        {ok, Agents}
    catch
        throw:Error -> {error, Error};
        _:Error -> {error, Error}
    end.

%% @doc Get specialized tools for a particular domain
-spec get_specialized_tools(atom()) -> [map()].
get_specialized_tools(data_science) -> get_data_science_tools();
get_specialized_tools(business_analysis) -> get_business_analysis_tools();
get_specialized_tools(finance) -> get_financial_tools();
get_specialized_tools(software_development) -> get_development_tools();
get_specialized_tools(devops) -> get_devops_tools();
get_specialized_tools(cybersecurity) -> get_security_tools();
get_specialized_tools(content_writing) -> get_content_tools();
get_specialized_tools(design) -> get_design_tools();
get_specialized_tools(video_production) -> get_video_tools();
get_specialized_tools(customer_support) -> get_support_tools();
get_specialized_tools(sales) -> get_sales_tools();
get_specialized_tools(marketing) -> get_marketing_tools();
get_specialized_tools(research) -> get_research_tools();
get_specialized_tools(education) -> get_language_tools();
get_specialized_tools(programming_education) -> get_coding_mentor_tools();
get_specialized_tools(legal) -> get_legal_tools();
get_specialized_tools(healthcare) -> get_medical_tools();
get_specialized_tools(project_management) -> get_project_management_tools();
get_specialized_tools(ai_research) -> get_ai_research_tools();
get_specialized_tools(computer_vision) -> get_computer_vision_tools();
get_specialized_tools(nlp) -> get_nlp_tools();
get_specialized_tools(_) -> [].

%% Internal helper functions
create_agent_from_template(Template) ->
    Id = generate_agent_id(),
    #{
        id => Id,
        name => maps:get(name, Template),
        type => maps:get(type, Template),
        description => maps:get(description, Template),
        model => maps:get(model, Template),
        temperature => maps:get(temperature, Template, 0.5),
        max_tokens => maps:get(max_tokens, Template, 2000),
        tools => maps:get(tools, Template, []),
        system_prompt => maps:get(system_prompt, Template),
        capabilities => maps:get(capabilities, Template, []),
        specialization => maps:get(specialization, Template),
        complexity_level => maps:get(complexity_level, Template, medium),
        status => <<"idle">>,
        created_at => erlang:system_time(second),
        last_activity => erlang:system_time(second)
    }.

generate_agent_id() ->
    Timestamp = integer_to_binary(erlang:system_time(microsecond)),
    Random = integer_to_binary(rand:uniform(999999)),
    <<Timestamp/binary, Random/binary>>.