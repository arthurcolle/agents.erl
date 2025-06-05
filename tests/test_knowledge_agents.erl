#!/usr/bin/env escript
%% Test script for knowledge base agent templates

main(_) ->
    %% Add the ebin directories to the code path
    code:add_paths([
        "_build/default/lib/agent/ebin",
        "_build/default/lib/openai/ebin",
        "_build/default/lib/jsx/ebin",
        "_build/default/lib/uuid/ebin",
        "_build/default/lib/quickrand/ebin"
    ]),
    
    io:format("Testing Knowledge Base Agent Templates~n~n"),
    
    %% Test retrieving new templates
    NewTemplates = [
        <<"interdisciplinary_researcher">>,
        <<"computational_scientist">>,
        <<"cognitive_science_researcher">>,
        <<"environmental_systems_analyst">>,
        <<"biomedical_researcher">>,
        <<"quantum_information_theorist">>,
        <<"cultural_intelligence_analyst">>,
        <<"educational_innovation_designer">>,
        <<"systems_biology_modeler">>,
        <<"philosophical_ethicist">>,
        <<"archaeo_historian">>,
        <<"complexity_scientist">>,
        <<"neuroeconomist">>,
        <<"digital_humanities_scholar">>,
        <<"astrobiology_researcher">>,
        <<"social_network_theorist">>,
        <<"conservation_geneticist">>,
        <<"psycholinguist">>,
        <<"urban_systems_planner">>,
        <<"ethnomusicologist">>
    ],
    
    io:format("Checking ~p new knowledge-base agent templates...~n~n", [length(NewTemplates)]),
    
    lists:foreach(fun(TemplateId) ->
        case agent_templates:get_template(TemplateId) of
            {ok, Template} ->
                Name = maps:get(name, Template),
                Desc = maps:get(description, Template),
                Tools = maps:get(tools, Template),
                HasKB = lists:member(knowledge_base_retrieval, Tools),
                io:format("✓ ~s~n", [Name]),
                io:format("  ID: ~s~n", [TemplateId]),
                io:format("  Description: ~s~n", [Desc]),
                io:format("  Has KB access: ~p~n~n", [HasKB]);
            {error, _} ->
                io:format("✗ Template ~s not found~n", [TemplateId])
        end
    end, NewTemplates),
    
    %% Count total templates
    AllTemplates = agent_templates:list_templates(),
    io:format("Total templates available: ~p~n", [length(AllTemplates)]),
    
    %% Count templates with knowledge base access
    KBTemplates = lists:filter(fun(#{id := Id}) ->
        case agent_templates:get_template(Id) of
            {ok, Template} ->
                Tools = maps:get(tools, Template, []),
                lists:member(knowledge_base_retrieval, Tools);
            _ -> false
        end
    end, AllTemplates),
    
    io:format("Templates with knowledge base access: ~p~n", [length(KBTemplates)]).