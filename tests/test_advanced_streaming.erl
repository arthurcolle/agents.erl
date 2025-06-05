#!/usr/bin/env escript
%% test_advanced_streaming.erl
%% Comprehensive test for advanced OpenAI streaming features

-define(LOG(Msg), io:format("[TEST] ~s~n", [Msg])).
-define(LOG(Msg, Args), io:format("[TEST] " ++ Msg ++ "~n", Args)).

main(_) ->
    ?LOG("🚀 Advanced OpenAI Streaming Test Starting..."),
    ?LOG("Testing: Streaming, Background Mode, File Inputs"),
    
    % Start the applications
    ?LOG(""),
    ?LOG("=== STARTUP ==="),
    start_applications(),
    
    % Test 1: Basic Chat Streaming
    ?LOG(""),
    ?LOG("=== TEST 1: Basic Chat Streaming ==="),
    test_basic_streaming(),
    
    % Test 2: Responses API Streaming
    ?LOG(""),
    ?LOG("=== TEST 2: Responses API Streaming ==="),
    test_responses_streaming(),
    
    % Test 3: Background Mode
    ?LOG(""),
    ?LOG("=== TEST 3: Background Mode ==="),
    test_background_mode(),
    
    % Test 4: File Inputs
    ?LOG(""),
    ?LOG("=== TEST 4: File Inputs ==="),
    test_file_inputs(),
    
    % Test 5: Combined Features
    ?LOG(""),
    ?LOG("=== TEST 5: Combined Features ==="),
    test_combined_features(),
    
    ?LOG(""),
    ?LOG("✅ All tests completed!"),
    ok.

start_applications() ->
    ?LOG("Starting required applications..."),
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),
    application:ensure_all_started(jsx),
    
    % Start OpenAI services
    Services = [
        {openai_chat, "Chat Completions"},
        {openai_responses, "Responses API"},
        {openai_background, "Background Manager"},
        {openai_files, "Files Manager"}
    ],
    
    lists:foreach(fun({Module, Name}) ->
        case Module:start_link(#{}) of
            {ok, _} -> ?LOG("✅ ~s service started", [Name]);
            {error, {already_started, _}} -> ?LOG("✅ ~s service already running", [Name]);
            Error -> ?LOG("❌ Failed to start ~s: ~p", [Name, Error])
        end
    end, Services).

test_basic_streaming() ->
    ?LOG("Testing basic chat streaming with semantic events..."),
    
    case has_api_key() of
        false ->
            ?LOG("⚠️  OPENAI_API_KEY not set - running mock test"),
            mock_basic_streaming();
        true ->
            ?LOG("🔑 API key found - running real test"),
            real_basic_streaming()
    end.

test_responses_streaming() ->
    ?LOG("Testing Responses API streaming..."),
    
    case has_api_key() of
        false ->
            mock_responses_streaming();
        true ->
            real_responses_streaming()
    end.

test_background_mode() ->
    ?LOG("Testing background mode for long-running tasks..."),
    
    case has_api_key() of
        false ->
            mock_background_mode();
        true ->
            real_background_mode()
    end.

test_file_inputs() ->
    ?LOG("Testing file input support..."),
    
    % Create a test image file
    create_test_image(),
    
    case has_api_key() of
        false ->
            mock_file_inputs();
        true ->
            real_file_inputs()
    end.

test_combined_features() ->
    ?LOG("Testing combined features: Background + Streaming + Files..."),
    
    case has_api_key() of
        false ->
            mock_combined_features();
        true ->
            real_combined_features()
    end.

%% Mock implementations

mock_basic_streaming() ->
    ?LOG("🎭 Running mock basic streaming..."),
    
    % Test the streaming utilities
    EventHandler = fun(Event) ->
        FormattedEvent = openai_streaming:format_stream_event(Event),
        ?LOG("📥 Event: ~p", [maps:get(type, FormattedEvent, unknown)]),
        continue
    end,
    
    % Simulate some events
    Events = [
        #{type => content, data => <<"Hello">>},
        #{type => content, data => <<" world">>},
        #{type => finish, reason => stop}
    ],
    
    lists:foreach(fun(Event) ->
        EventHandler(Event),
        timer:sleep(50)
    end, Events),
    
    ?LOG("✅ Mock basic streaming completed").

mock_responses_streaming() ->
    ?LOG("🎭 Running mock Responses API streaming..."),
    
    % Test semantic events
    Events = [
        #{<<"type">> => <<"response.created">>, <<"id">> => <<"resp_mock_123">>},
        #{<<"type">> => <<"response.in_progress">>},
        #{<<"type">> => <<"response.output_text.delta">>, <<"delta">> => <<"Hello from mock!">>},
        #{<<"type">> => <<"response.completed">>}
    ],
    
    lists:foreach(fun(Event) ->
        FormattedEvent = openai_streaming:format_stream_event(Event),
        ?LOG("📥 Semantic Event: ~p", [Event]),
        timer:sleep(100)
    end, Events),
    
    ?LOG("✅ Mock Responses API streaming completed").

mock_background_mode() ->
    ?LOG("🎭 Running mock background mode..."),
    
    % Test background response tracking
    ResponseId = <<"resp_mock_bg_123">>,
    
    ?LOG("Creating mock background response: ~s", [ResponseId]),
    ?LOG("Status: queued -> in_progress -> completed"),
    
    % Simulate status progression
    Statuses = [<<"queued">>, <<"in_progress">>, <<"completed">>],
    lists:foreach(fun(Status) ->
        ?LOG("📊 Status update: ~s", [Status]),
        timer:sleep(500)
    end, Statuses),
    
    ?LOG("✅ Mock background mode completed").

mock_file_inputs() ->
    ?LOG("🎭 Running mock file inputs..."),
    
    % Test file input validation
    ValidImageInput = #{
        <<"type">> => <<"image_file">>,
        <<"image_file">> => #{<<"file_id">> => <<"file-mock123">>}
    },
    
    case openai_files:validate_file_input(ValidImageInput) of
        {ok, image_file} ->
            ?LOG("✅ Image file input validation passed");
        Error ->
            ?LOG("❌ Image file input validation failed: ~p", [Error])
    end,
    
    % Test creating image input structure
    MockImageInput = #{
        <<"type">> => <<"image_file">>,
        <<"image_file">> => #{
            <<"file_id">> => <<"file-test123">>
        }
    },
    
    ?LOG("📁 Mock image input created: ~p", [MockImageInput]),
    ?LOG("✅ Mock file inputs completed").

mock_combined_features() ->
    ?LOG("🎭 Running mock combined features..."),
    
    % Simulate a complex workflow
    ?LOG("1. Creating background response with file input..."),
    timer:sleep(200),
    
    ?LOG("2. Starting streaming from background response..."),
    timer:sleep(200),
    
    ?LOG("3. Processing semantic events..."),
    timer:sleep(200),
    
    ?LOG("4. Background task completed successfully"),
    ?LOG("✅ Mock combined features completed").

%% Real implementations (when API key is available)

real_basic_streaming() ->
    ?LOG("🚀 Running real basic streaming test..."),
    
    Model = <<"gpt-4o-mini">>,
    Messages = [
        #{<<"role">> => <<"user">>, <<"content">> => <<"Count from 1 to 5, one number per response chunk.">>}
    ],
    Options = #{stream => true, max_tokens => 50},
    
    % Simple event handler that prints content
    EventHandler = fun(Event) ->
        case maps:get(type, Event, undefined) of
            content ->
                Content = maps:get(data, Event, <<>>),
                io:format("~s", [Content]),
                continue;
            finish ->
                io:format("~n"),
                ?LOG("🏁 Streaming finished"),
                stop;
            _ ->
                continue
        end
    end,
    
    ?LOG("📤 Starting real streaming request..."),
    case openai_streaming:create_streaming_chat(Model, Messages, Options) of
        {ok, _StreamPid} ->
            io:format("Response: "),
            case openai_streaming:handle_stream_events(EventHandler, 30000) of
                {ok, Events} ->
                    ?LOG("✅ Real streaming completed with ~p events", [length(Events)]);
                {error, Reason} ->
                    ?LOG("❌ Real streaming failed: ~p", [Reason])
            end;
        {error, Reason} ->
            ?LOG("❌ Failed to start real streaming: ~p", [Reason])
    end.

real_responses_streaming() ->
    ?LOG("🚀 Running real Responses API streaming test..."),
    
    Input = [
        #{<<"role">> => <<"user">>, <<"content">> => <<"Tell me a short joke about programming.">>}
    ],
    Model = <<"gpt-4o-mini">>,
    Options = #{stream => true, max_output_tokens => 100},
    
    % Event handler for semantic events
    EventHandler = fun(Event) ->
        case maps:get(<<"type">>, Event, undefined) of
            <<"response.output_text.delta">> ->
                Delta = maps:get(<<"delta">>, Event, <<>>),
                io:format("~s", [Delta]),
                continue;
            <<"response.completed">> ->
                io:format("~n"),
                ?LOG("🏁 Response completed"),
                stop;
            Type when Type =/= undefined ->
                ?LOG("📥 Event: ~s", [Type]),
                continue;
            _ ->
                continue
        end
    end,
    
    ?LOG("📤 Starting real Responses API streaming..."),
    case openai_streaming:create_streaming_response(Input, Model, Options) of
        {ok, _StreamPid} ->
            io:format("Response: "),
            case openai_streaming:handle_stream_events(EventHandler, 60000) of
                {ok, Events} ->
                    ?LOG("✅ Real Responses API streaming completed with ~p events", [length(Events)]);
                {error, Reason} ->
                    ?LOG("❌ Real Responses API streaming failed: ~p", [Reason])
            end;
        {error, Reason} ->
            ?LOG("❌ Failed to start real Responses API streaming: ~p", [Reason])
    end.

real_background_mode() ->
    ?LOG("🚀 Running real background mode test..."),
    
    % Create a background task
    Input = [
        #{<<"role">> => <<"user">>, <<"content">> => <<"Write a detailed explanation of how neural networks work. Take your time and be thorough.">>}
    ],
    Model = <<"gpt-4o-mini">>,
    Options = #{
        background => true,
        max_output_tokens => 500,
        store => true  % Required for background mode
    },
    
    ?LOG("📤 Creating background response..."),
    case openai_background:create_background_response(Input, Model, Options, self()) of
        {ok, #{<<"id">> := ResponseId, <<"status">> := InitialStatus}} ->
            ?LOG("✅ Background response created: ~s (status: ~s)", [ResponseId, InitialStatus]),
            
            % Monitor the response
            monitor_background_response(ResponseId, 10),  % Check up to 10 times
            
            ?LOG("✅ Real background mode test completed");
        {error, Reason} ->
            ?LOG("❌ Failed to create background response: ~p", [Reason])
    end.

real_file_inputs() ->
    ?LOG("🚀 Running real file inputs test..."),
    
    TestImagePath = "test_image.png",
    
    case filelib:is_file(TestImagePath) of
        true ->
            ?LOG("📁 Test image found, uploading..."),
            case openai_files:create_image_input(TestImagePath) of
                {ok, ImageInput} ->
                    ?LOG("✅ Image input created: ~p", [ImageInput]),
                    
                    % Test using the image in a response
                    Input = [
                        #{<<"role">> => <<"user">>, <<"content">> => [
                            #{<<"type">> => <<"text">>, <<"text">> => <<"What do you see in this image?">>},
                            ImageInput
                        ]}
                    ],
                    
                    ?LOG("📤 Creating response with image input..."),
                    case openai_responses:create_response(Input, <<"gpt-4o-mini">>, #{max_output_tokens => 100}) of
                        {ok, Response} ->
                            OutputText = maps:get(<<"output_text">>, Response, <<"No output">>),
                            ?LOG("✅ Response with image: ~s", [OutputText]);
                        {error, Reason} ->
                            ?LOG("❌ Failed to get response with image: ~p", [Reason])
                    end;
                {error, Reason} ->
                    ?LOG("❌ Failed to create image input: ~p", [Reason])
            end;
        false ->
            ?LOG("⚠️  Test image not found, skipping file upload test")
    end.

real_combined_features() ->
    ?LOG("🚀 Running real combined features test..."),
    
    TestImagePath = "test_image.png",
    
    case filelib:is_file(TestImagePath) of
        true ->
            ?LOG("📁 Creating image input for background streaming..."),
            case openai_files:create_image_input(TestImagePath) of
                {ok, ImageInput} ->
                    % Create a background streaming response with image
                    Input = [
                        #{<<"role">> => <<"user">>, <<"content">> => [
                            #{<<"type">> => <<"text">>, <<"text">> => <<"Describe this image in detail and explain what you see.">>},
                            ImageInput
                        ]}
                    ],
                    
                    Options = #{
                        background => true,
                        stream => true,
                        max_output_tokens => 200,
                        store => true
                    },
                    
                    ?LOG("📤 Creating background streaming response with image..."),
                    case openai_background:create_background_response(Input, <<"gpt-4o-mini">>, Options, self()) of
                        {ok, #{<<"id">> := ResponseId}} ->
                            ?LOG("✅ Background streaming response created: ~s", [ResponseId]),
                            
                            % Try to stream from the background response
                            case openai_background:stream_background_response(ResponseId) of
                                {ok, _StreamPid} ->
                                    ?LOG("🌊 Background streaming started"),
                                    collect_background_events(5000);  % Wait up to 5 seconds
                                {error, Reason} ->
                                    ?LOG("❌ Failed to start background streaming: ~p", [Reason])
                            end;
                        {error, Reason} ->
                            ?LOG("❌ Failed to create background streaming response: ~p", [Reason])
                    end;
                {error, Reason} ->
                    ?LOG("❌ Failed to create image input for combined test: ~p", [Reason])
            end;
        false ->
            ?LOG("⚠️  Test image not found, running text-only combined test"),
            text_only_combined_test()
    end.

%% Helper functions

has_api_key() ->
    case os:getenv("OPENAI_API_KEY") of
        false -> false;
        Key when length(Key) > 10 -> true;
        _ -> false
    end.

create_test_image() ->
    % Create a simple test PNG file (1x1 pixel)
    PngData = <<137,80,78,71,13,10,26,10,0,0,0,13,73,72,68,82,0,0,0,1,0,0,0,1,8,2,0,0,0,144,119,83,222,0,0,0,12,73,68,65,84,8,215,99,248,15,0,1,1,1,0,24,221,142,175,0,0,0,0,73,69,78,68,174,66,96,130>>,
    file:write_file("test_image.png", PngData),
    ?LOG("📸 Created test image file").

monitor_background_response(_ResponseId, 0) ->
    ?LOG("⏰ Stopped monitoring background response (max attempts reached)");
monitor_background_response(ResponseId, AttemptsLeft) ->
    timer:sleep(2000),  % Wait 2 seconds
    
    case openai_background:poll_response(ResponseId) of
        {ok, #{<<"status">> := Status}} ->
            ?LOG("📊 Background response status: ~s", [Status]),
            case Status of
                <<"completed">> ->
                    ?LOG("✅ Background response completed!");
                <<"failed">> ->
                    ?LOG("❌ Background response failed");
                <<"cancelled">> ->
                    ?LOG("🛑 Background response cancelled");
                _ ->
                    monitor_background_response(ResponseId, AttemptsLeft - 1)
            end;
        {error, Reason} ->
            ?LOG("❌ Failed to poll background response: ~p", [Reason])
    end.

collect_background_events(Timeout) ->
    receive
        {stream_event, Event} ->
            ?LOG("📥 Background stream event: ~p", [Event]),
            collect_background_events(Timeout);
        stream_complete ->
            ?LOG("✅ Background streaming completed");
        {stream_error, Error} ->
            ?LOG("❌ Background streaming error: ~p", [Error]);
        {background_response_update, ResponseId, Status} ->
            ?LOG("📊 Background response ~s updated: ~s", [ResponseId, Status]),
            collect_background_events(Timeout)
    after Timeout ->
        ?LOG("⏰ Background event collection timeout")
    end.

text_only_combined_test() ->
    ?LOG("📝 Running text-only combined features test..."),
    
    Input = [
        #{<<"role">> => <<"user">>, <<"content">> => <<"Explain the concept of streaming APIs and why they're useful in modern applications.">>}
    ],
    
    Options = #{
        background => true,
        stream => true,
        max_output_tokens => 150,
        store => true
    },
    
    case openai_background:create_background_response(Input, <<"gpt-4o-mini">>, Options, self()) of
        {ok, #{<<"id">> := ResponseId}} ->
            ?LOG("✅ Text-only background streaming response created: ~s", [ResponseId]),
            collect_background_events(5000);
        {error, Reason} ->
            ?LOG("❌ Failed to create text-only combined test: ~p", [Reason])
    end.