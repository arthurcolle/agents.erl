-module(image_upload_handler).
-export([init/2, allowed_methods/2, content_types_accepted/2, content_types_provided/2, 
         accept_image_upload/2, to_json/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"multipart/form-data">>, accept_image_upload}], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

to_json(Req, State) ->
    Body = jsx:encode(#{error => <<"Method not allowed">>}),
    {Body, Req, State}.

accept_image_upload(Req, State) ->
    case cowboy_req:read_part(Req) of
        {ok, Headers, Req2} ->
            case cow_multipart:form_data(Headers) of
                {data, <<"image">>} ->
                    handle_image_data(Req2, State);
                _ ->
                    {false, Req2, State}
            end;
        {done, Req2} ->
            {false, Req2, State}
    end.

handle_image_data(Req, State) ->
    case cowboy_req:read_part_body(Req) of
        {ok, Data, Req2} ->
            % Generate unique filename
            Timestamp = integer_to_binary(erlang:system_time(millisecond)),
            Extension = <<".jpg">>, % Could be improved to detect actual type
            Filename = <<Timestamp/binary, Extension/binary>>,
            
            % Create uploads directory if it doesn't exist
            BaseDir = code:priv_dir(agent_web),
            UploadsDir = filename:join([BaseDir, "static", "uploads"]),
            filelib:ensure_dir(filename:join(UploadsDir, "dummy")),
            
            % Write file
            FilePath = filename:join(UploadsDir, binary_to_list(Filename)),
            case file:write_file(FilePath, Data) of
                ok ->
                    Url = <<"/uploads/", Filename/binary>>,
                    Response = #{
                        <<"url">> => Url,
                        <<"filename">> => Filename,
                        <<"size">> => byte_size(Data)
                    },
                    Body = jsx:encode(Response),
                    Req3 = cowboy_req:set_resp_body(Body, Req2),
                    Req4 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req3),
                    {true, Req4, State};
                {error, Reason} ->
                    logger:error("Failed to save uploaded image: ~p", [Reason]),
                    {false, Req2, State}
            end;
        {more, _Data, Req2} ->
            % Handle large files by reading in chunks
            {false, Req2, State}
    end.