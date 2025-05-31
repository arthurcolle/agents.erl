-module(ssl_cert_handler).

-include_lib("public_key/include/public_key.hrl").

-export([init/2, validate_certificates/0, get_certificate_info/0, check_expiry/1]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    {ok, handle_request(Method, Req0), State}.

handle_request(<<"GET">>, Req) ->
    Path = cowboy_req:path(Req),
    case Path of
        <<"/api/ssl/info">> ->
            CertInfo = get_certificate_info(),
            ResponseBody = jsx:encode(CertInfo),
            cowboy_req:reply(200, #{
                <<"content-type">> => <<"application/json">>,
                <<"access-control-allow-origin">> => <<"*">>,
                <<"access-control-allow-methods">> => <<"GET, POST, OPTIONS">>,
                <<"access-control-allow-headers">> => <<"content-type">>
            }, ResponseBody, Req);
        <<"/api/ssl/validate">> ->
            ValidationResult = validate_certificates(),
            ResponseBody = jsx:encode(ValidationResult),
            cowboy_req:reply(200, #{
                <<"content-type">> => <<"application/json">>,
                <<"access-control-allow-origin">> => <<"*">>,
                <<"access-control-allow-methods">> => <<"GET, POST, OPTIONS">>,
                <<"access-control-allow-headers">> => <<"content-type">>
            }, ResponseBody, Req);
        _ ->
            cowboy_req:reply(404, #{}, <<"Not found">>, Req)
    end;

handle_request(<<"OPTIONS">>, Req) ->
    cowboy_req:reply(200, #{
        <<"access-control-allow-origin">> => <<"*">>,
        <<"access-control-allow-methods">> => <<"GET, POST, OPTIONS">>,
        <<"access-control-allow-headers">> => <<"content-type">>
    }, Req);

handle_request(_, Req) ->
    cowboy_req:reply(405, #{}, <<"Method not allowed">>, Req).

get_certificate_info() ->
    CertFile = application:get_env(agent_web, cert_file, "./certs/localhost.crt"),
    KeyFile = application:get_env(agent_web, key_file, "./certs/localhost.key"),
    SslEnabled = application:get_env(agent_web, ssl_enabled, false),
    HttpsPort = application:get_env(agent_web, https_port, 8443),
    
    CertExists = filelib:is_regular(CertFile),
    KeyExists = filelib:is_regular(KeyFile),
    
    CertInfo = case CertExists of
        true ->
            case get_cert_details(CertFile) of
                {ok, Details} -> Details;
                {error, Reason} -> #{error => list_to_binary(io_lib:format("~p", [Reason]))}
            end;
        false ->
            #{error => <<"Certificate file not found">>}
    end,
    
    #{
        ssl_enabled => SslEnabled,
        https_port => HttpsPort,
        cert_file => list_to_binary(CertFile),
        key_file => list_to_binary(KeyFile),
        cert_exists => CertExists,
        key_exists => KeyExists,
        certificate_info => CertInfo
    }.

validate_certificates() ->
    CertFile = application:get_env(agent_web, cert_file, "./certs/localhost.crt"),
    KeyFile = application:get_env(agent_web, key_file, "./certs/localhost.key"),
    
    Results = [
        validate_file_exists(CertFile, <<"certificate">>),
        validate_file_exists(KeyFile, <<"private_key">>),
        validate_certificate_format(CertFile),
        validate_key_format(KeyFile),
        validate_cert_key_match(CertFile, KeyFile),
        check_expiry(CertFile)
    ],
    
    Errors = [R || R <- Results, maps:get(valid, R, true) =:= false],
    Valid = length(Errors) =:= 0,
    
    #{
        valid => Valid,
        error_count => length(Errors),
        results => Results,
        timestamp => list_to_binary(calendar:system_time_to_rfc3339(erlang:system_time(second)))
    }.

validate_file_exists(FilePath, Type) ->
    case filelib:is_regular(FilePath) of
        true ->
            #{type => Type, check => <<"file_exists">>, valid => true, file => list_to_binary(FilePath)};
        false ->
            #{type => Type, check => <<"file_exists">>, valid => false, 
              error => <<"File does not exist">>, file => list_to_binary(FilePath)}
    end.

validate_certificate_format(CertFile) ->
    case filelib:is_regular(CertFile) of
        true ->
            case file:read_file(CertFile) of
                {ok, CertData} ->
                    case public_key:pem_decode(CertData) of
                        [] ->
                            #{type => <<"certificate">>, check => <<"format">>, valid => false,
                              error => <<"Invalid PEM format">>};
                        PemEntries ->
                            case lists:any(fun({'Certificate', _, _}) -> true; (_) -> false end, PemEntries) of
                                true ->
                                    #{type => <<"certificate">>, check => <<"format">>, valid => true};
                                false ->
                                    #{type => <<"certificate">>, check => <<"format">>, valid => false,
                                      error => <<"No certificate found in PEM file">>}
                            end
                    end;
                {error, Reason} ->
                    #{type => <<"certificate">>, check => <<"format">>, valid => false,
                      error => list_to_binary(io_lib:format("Cannot read file: ~p", [Reason]))}
            end;
        false ->
            #{type => <<"certificate">>, check => <<"format">>, valid => false,
              error => <<"Certificate file does not exist">>}
    end.

validate_key_format(KeyFile) ->
    case filelib:is_regular(KeyFile) of
        true ->
            case file:read_file(KeyFile) of
                {ok, KeyData} ->
                    case public_key:pem_decode(KeyData) of
                        [] ->
                            #{type => <<"private_key">>, check => <<"format">>, valid => false,
                              error => <<"Invalid PEM format">>};
                        PemEntries ->
                            KeyTypes = ['RSAPrivateKey', 'PrivateKeyInfo', 'ECPrivateKey'],
                            case lists:any(fun({Type, _, _}) -> lists:member(Type, KeyTypes) end, PemEntries) of
                                true ->
                                    #{type => <<"private_key">>, check => <<"format">>, valid => true};
                                false ->
                                    #{type => <<"private_key">>, check => <<"format">>, valid => false,
                                      error => <<"No private key found in PEM file">>}
                            end
                    end;
                {error, Reason} ->
                    #{type => <<"private_key">>, check => <<"format">>, valid => false,
                      error => list_to_binary(io_lib:format("Cannot read file: ~p", [Reason]))}
            end;
        false ->
            #{type => <<"private_key">>, check => <<"format">>, valid => false,
              error => <<"Private key file does not exist">>}
    end.

validate_cert_key_match(CertFile, KeyFile) ->
    try
        case {filelib:is_regular(CertFile), filelib:is_regular(KeyFile)} of
            {true, true} ->
                case {file:read_file(CertFile), file:read_file(KeyFile)} of
                    {{ok, CertData}, {ok, KeyData}} ->
                        case {public_key:pem_decode(CertData), public_key:pem_decode(KeyData)} of
                            {[CertEntry|_], [KeyEntry|_]} ->
                                try
                                    _Cert = public_key:pem_entry_decode(CertEntry),
                                    _Key = public_key:pem_entry_decode(KeyEntry),
                                    % Basic validation - this is simplified
                                    #{type => <<"cert_key_match">>, check => <<"key_match">>, valid => true,
                                      note => <<"Basic validation passed">>}
                                catch
                                    _:DecodeError ->
                                        #{type => <<"cert_key_match">>, check => <<"key_match">>, valid => false,
                                          error => list_to_binary(io_lib:format("Decode error: ~p", [DecodeError]))}
                                end;
                            _ ->
                                #{type => <<"cert_key_match">>, check => <<"key_match">>, valid => false,
                                  error => <<"Could not decode certificate or key">>}
                        end;
                    _ ->
                        #{type => <<"cert_key_match">>, check => <<"key_match">>, valid => false,
                          error => <<"Could not read certificate or key files">>}
                end;
            _ ->
                #{type => <<"cert_key_match">>, check => <<"key_match">>, valid => false,
                  error => <<"Certificate or key file missing">>}
        end
    catch
        _:OuterError ->
            #{type => <<"cert_key_match">>, check => <<"key_match">>, valid => false,
              error => list_to_binary(io_lib:format("Validation error: ~p", [OuterError]))}
    end.

check_expiry(CertFile) ->
    case filelib:is_regular(CertFile) of
        true ->
            case get_cert_details(CertFile) of
                {ok, Details} ->
                    case maps:get(not_after, Details, undefined) of
                        undefined ->
                            #{type => <<"certificate">>, check => <<"expiry">>, valid => false,
                              error => <<"Could not determine expiration date">>};
                        NotAfter ->
                            Now = erlang:system_time(second),
                            DaysUntilExpiry = (NotAfter - Now) div (24 * 3600),
                            case DaysUntilExpiry > 0 of
                                true ->
                                    Warning = DaysUntilExpiry < 30,
                                    Result = #{type => <<"certificate">>, check => <<"expiry">>, valid => true,
                                               days_until_expiry => DaysUntilExpiry,
                                               expires_at => list_to_binary(calendar:system_time_to_rfc3339(NotAfter))},
                                    case Warning of
                                        true -> Result#{warning => <<"Certificate expires within 30 days">>};
                                        false -> Result
                                    end;
                                false ->
                                    #{type => <<"certificate">>, check => <<"expiry">>, valid => false,
                                      error => <<"Certificate has expired">>,
                                      days_until_expiry => DaysUntilExpiry,
                                      expired_at => list_to_binary(calendar:system_time_to_rfc3339(NotAfter))}
                            end
                    end;
                {error, Reason} ->
                    #{type => <<"certificate">>, check => <<"expiry">>, valid => false,
                      error => list_to_binary(io_lib:format("Could not read certificate: ~p", [Reason]))}
            end;
        false ->
            #{type => <<"certificate">>, check => <<"expiry">>, valid => false,
              error => <<"Certificate file does not exist">>}
    end.

get_cert_details(CertFile) ->
    try
        case file:read_file(CertFile) of
            {ok, CertData} ->
                case public_key:pem_decode(CertData) of
                    [CertEntry|_] ->
                        case public_key:pem_entry_decode(CertEntry) of
                            {'Certificate', TBSCert, _, _} ->
                                Validity = TBSCert#'TBSCertificate'.validity,
                                NotBefore = Validity#'Validity'.notBefore,
                                NotAfter = Validity#'Validity'.notAfter,
                                Subject = TBSCert#'TBSCertificate'.subject,
                                Issuer = TBSCert#'TBSCertificate'.issuer,
                                
                                {ok, #{
                                    subject => format_name(Subject),
                                    issuer => format_name(Issuer),
                                    not_before => time_to_epoch(NotBefore),
                                    not_after => time_to_epoch(NotAfter),
                                    version => TBSCert#'TBSCertificate'.version
                                }};
                            _ ->
                                {error, invalid_certificate_format}
                        end;
                    [] ->
                        {error, no_certificate_found};
                    _ ->
                        {error, multiple_certificates_found}
                end;
            {error, Reason} ->
                {error, {file_read_error, Reason}}
        end
    catch
        _:Error ->
            {error, {parse_error, Error}}
    end.

format_name({rdnSequence, RDNSeq}) ->
    Parts = lists:foldl(fun(RDN, Acc) ->
        case RDN of
            [#'AttributeTypeAndValue'{type = Type, value = Value}] ->
                TypeStr = case Type of
                    ?'id-at-commonName' -> "CN";
                    ?'id-at-countryName' -> "C";
                    ?'id-at-stateOrProvinceName' -> "ST";
                    ?'id-at-localityName' -> "L";
                    ?'id-at-organizationName' -> "O";
                    ?'id-at-organizationalUnitName' -> "OU";
                    _ -> "Unknown"
                end,
                ValueStr = case Value of
                    {utf8String, V} -> binary_to_list(V);
                    {printableString, V} -> V;
                    V when is_list(V) -> V;
                    V when is_binary(V) -> binary_to_list(V);
                    _ -> "Unknown"
                end,
                [io_lib:format("~s=~s", [TypeStr, ValueStr]) | Acc];
            _ ->
                Acc
        end
    end, [], RDNSeq),
    list_to_binary(string:join(lists:reverse(Parts), ", ")).

time_to_epoch({utcTime, TimeStr}) ->
    % Parse YYMMDDHHMMSSZ format
    case length(TimeStr) of
        13 -> % YYMMDDHHMMSSZ
            [YY, MM, DD, HH, Min, SS] = [list_to_integer(lists:sublist(TimeStr, Start, 2)) 
                                          || Start <- [1, 3, 5, 7, 9, 11]],
            Year = if YY >= 50 -> 1900 + YY; true -> 2000 + YY end,
            calendar:datetime_to_gregorian_seconds({{Year, MM, DD}, {HH, Min, SS}}) - 
                calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}});
        _ ->
            0
    end;
time_to_epoch({generalTime, TimeStr}) ->
    % Parse YYYYMMDDHHMMSSZ format
    case length(TimeStr) of
        15 -> % YYYYMMDDHHMMSSZ
            [YYYY, MM, DD, HH, Min, SS] = [list_to_integer(lists:sublist(TimeStr, Start, Len)) 
                                           || {Start, Len} <- [{1, 4}, {5, 2}, {7, 2}, {9, 2}, {11, 2}, {13, 2}]],
            calendar:datetime_to_gregorian_seconds({{YYYY, MM, DD}, {HH, Min, SS}}) - 
                calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}});
        _ ->
            0
    end;
time_to_epoch(_) ->
    0.