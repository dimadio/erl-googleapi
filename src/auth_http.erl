-module(auth_http).

-behaviour(gen_server).


-define (GOOGLE_AUTH_URI,   <<"https://accounts.google.com/o/oauth2/auth">>).
-define (GOOGLE_REVOKE_URI, <<"https://accounts.google.com/o/oauth2/revoke">>).
-define (GOOGLE_TOKEN_URI,  <<"https://accounts.google.com/o/oauth2/token">>).

-define (MAX_TOKEN_LIFETIME_SECS, 3600). %% 1 hour in seconds

-define(REFRESH_STATUS_CODES, [401]).


-include_lib("public_key/include/public_key.hrl"). 



-export([start_link/3,
	 start_link/0,
         init/1,
         code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
	 stop/0]).

-export([generate_refresh_request_body/1]).

-export([get/1, get/2, post/3, delete/2]).

-export([get_access_token/0, refresh_access_token/0]).

get(Uri) ->
    get(Uri, _Headers = []).
get( Uri, Headers)->
    handle_request(get, Uri, Headers).

post(Uri, Headers, Postdata)->
    handle_request(post, Uri, Headers, Postdata).


delete(Uri, Headers)->
    handle_request( delete, Uri, Headers).

stop() ->
    gen_server:cast(?MODULE, stop).


get_access_token()->
    gen_server:call( ?MODULE, get_access_token).

refresh_access_token()->
    gen_server:call( ?MODULE, refresh_access_token).


init(Settings)->
    PoolName = googleapi_pool,
    Options = [{timeout, 150000}, {max_connections, 100}],
    ok  = case  hackney_pool:start_pool(PoolName, Options) of
	      {ok, _PoolPid} -> ok;
	      {error, {already_started, _PoolPid}} -> ok;
	      Other -> Other
	  end,
    {ok, Settings}.


start_link( )->
    gen_server:start_link( {local, ?MODULE}, ?MODULE, [
						       {auth_mode, appscope},
						       {service_account_name, get_app_service_account()},
						       {token_uri, ?GOOGLE_TOKEN_URI},
						       {revoke_uri, ?GOOGLE_REVOKE_URI},
						       {scope, get_app_service_scope()}
						      ],[]).

start_link( Service_account_name, Private_key, Scope)->
    {ok, Binary} = file:read_file(Private_key),
    gen_server:start_link( {local, ?MODULE}, ?MODULE, [
						       {auth_mode, keyfile},
						       {service_account_name, Service_account_name},
						       {private_key, base64:encode(Binary)},
						       {private_key_password, 'notasecret'},
						       {token_uri, ?GOOGLE_TOKEN_URI},
						       {revoke_uri, ?GOOGLE_REVOKE_URI},
						       {scope, scopes_to_string(Scope)}
						      ],[]).



handle_call(get_access_token, _From ,Config)->
    AccessToken = proplists:get_value(access_token, Config),

    NewConfig = 
	case AccessToken of 
	    undefined ->
		refresh_token(Config);
	    _ ->
		Config
	end,

    %% Refresh key to expire beforehand
    Expired_in = proplists:get_value(token_expiry, NewConfig),
    Now =  now_sec(os:timestamp()),
    case Expired_in - Now of
	Soon when Soon < 60 ->
	    error_logger:info_msg("Token to expire - renew!\n"),
	    spawn( fun() -> refresh_access_token() end );
	_ ->
	    ok
    end,
    {reply,  [{access_token, proplists:get_value(access_token, NewConfig) }],  NewConfig};

handle_call(refresh_access_token, _From ,Config)->
    NewConfig = refresh_token(Config),
    {reply,  [{access_token, proplists:get_value(access_token, NewConfig) }],  NewConfig};
handle_call(Command, _From, State) ->
    error_logger:info_msg("Unsupported command: ~p~n", [Command]),
    {noreply, State}.



handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Command, State) ->
    error_logger:info_msg("Unsupported command: ~p~n", [Command]),
    {noreply, State}.

handle_info(Command,State) ->
    error_logger:info_msg("Unsupported command: ~p~n", [Command]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(normal, _State) ->
    hackney_pool:stop_pool(googleapi_pool),
    ok.



%% -----------------------

handle_request(Method, Uri, Headers) ->
    handle_request(Method, Uri, Headers, <<>>).
handle_request(Method, Uri, Headers, PostData) ->


    NewConfig = get_access_token(),

    UpdatedHeaders = apply_headers(Headers, NewConfig, PostData),

    %% error_logger:info_msg("Send request: ~p~n", [{Method, Uri, UpdatedHeaders, PostData}]),

    case hackney:request(Method, Uri,
			 UpdatedHeaders, PostData,
			 [{pool, googleapi_pool}]) of 
	{ok, StatusCode, RespHeaders, ClientRef} ->
	    case lists:member(StatusCode, ?REFRESH_STATUS_CODES) of 
		true ->

		    NewCfg = refresh_access_token(),
		    io:format("-- NewCfg = ~p~n", [NewCfg]),

		    UpdatedHeaders2 = apply_headers(Headers, NewCfg, PostData),

		    {ok, StatusCode2, RespHeaders2, ClientRef2} = hackney:request(Method, Uri,
										  UpdatedHeaders2, PostData,
										  [{pool, googleapi_pool}]),
		    {ok, RespBody2} = get_body(RespHeaders2, ClientRef2),

		    {StatusCode2, RespHeaders2, RespBody2};
		false ->
		    {ok, RespBody} = get_body(RespHeaders, ClientRef),
		    {StatusCode, RespHeaders, RespBody}
	    end;

	{error, Reason}->
	    {error, Reason};

	UnsupportedResult ->
	    io:format("[auth_http] get unsupported result: ~p~n", [UnsupportedResult]),
	    UnsupportedResult
    end.


get_body(Headers, Client)->	    
    ContentLength = proplists:get_value(<<"Content-Length">>, Headers, <<"0">>),
    IntContentLength = list_to_integer(binary:bin_to_list(ContentLength)),

    case IntContentLength of
	N when is_integer(N) andalso N > 0 ->
	    {ok, Body,_ } = hackney:body(Client),
	    {ok, Body};
	N when is_integer(N) andalso N =:= 0 ->
	    {ok, <<>>}
    end.

refresh_token(Config) ->
    case proplists:get_value(auth_mode, Config, undefined) of 
	appscope ->
	    refresh_token_service(Config);
	keyfile ->
	    refresh_token_file(Config)
    end.
    
refresh_token_service(Config) ->
    TokenData = get_app_service_property(<<"token">>),
    {RespJson} = jiffy:decode(TokenData),
    
    Config_1 = lists:keystore(access_token,   1, Config, {access_token, proplists:get_value(<<"access_token">>, RespJson)}),

    Expires_in = proplists:get_value(<<"expires_in">>, RespJson, undefined),
    case Expires_in of 
	undefined ->
	    lists:keystore(token_expiry,  1, Config_1, {token_expiry, undefined});
	Number when is_integer(Number)->
	    lists:keystore(token_expiry,  1, Config_1, {token_expiry, now_sec(os:timestamp())+ Number})
    end.


refresh_token_file(Config) ->
    Body = utils:build_body(generate_refresh_request_body(Config)),
    Headers = generate_refresh_request_headers(Config),


    {ok, StatusCode, _RespHeaders, ClientRef} = hackney:request(post, ?GOOGLE_TOKEN_URI,
								Headers, Body,
								[{pool, googleapi_pool}]),
    {ok, RespBody,_ } = hackney:body(ClientRef),


    case StatusCode of
	200 ->

	    {RespJson} = jiffy:decode(RespBody),

	    Config_1 = lists:keystore(token_response, 1, Config,   {token_response, {RespJson}}),
	    Config_2 = lists:keystore(access_token,   1, Config_1, {access_token, proplists:get_value(<<"access_token">>, RespJson)}),
	    Config_3 = lists:keystore(refresh_token,  1, Config_2, {refresh_token, proplists:get_value(<<"refresh_token">>, RespJson, undefined)}),

	    Expires_in = proplists:get_value(<<"expires_in">>, RespJson, undefined),
	    Config_4 = 
		case Expires_in of 
		    undefined ->
			lists:keystore(token_expiry,  1, Config_3, {token_expiry, undefined});
		    Number when is_integer(Number)->
			lists:keystore(token_expiry,  1, Config_3, {token_expiry, now_sec(os:timestamp())+ Number})
		end,

	    Config_4;
	_ ->
	    throw({error, {auth_failed, StatusCode}})
    end.
		

generate_refresh_request_headers(_Config) ->
    Headers = [
	       {<<"content-type">>, <<"application/x-www-form-urlencoded">>}
	      ],

    %% if self.user_agent is not None:
    %%    headers['user-agent'] = self.user_agent
    Headers.   

generate_refresh_request_body(Config)->
	    Assertion = generate_assertion(Config),

	    [{assertion, http_uri:encode(binary:bin_to_list(Assertion))},
	     {grant_type, http_uri:encode("urn:ietf:params:oauth:grant-type:jwt-bearer")}
	    ].


scopes_to_string([H|_Rest] = Scopes) when is_list(H)->
    string:join(Scopes, " ");
scopes_to_string([H|_Rest] = Scopes) when is_integer(H)->
    Scopes.


generate_assertion(Config)->
    %%5 Generate the assertion that will be used in the request.
    Now = now_sec(os:timestamp() ),
    Payload = {[
	       {<<"aud">>, proplists:get_value(token_uri, Config)},
	       {<<"scope">>, safe_to_bin(proplists:get_value(scope, Config))},
	       {<<"iat">>, Now},
	       {<<"exp">>, Now + ?MAX_TOKEN_LIFETIME_SECS},
	       {<<"iss">>, safe_to_bin(proplists:get_value(service_account_name, Config))}
	      ]},

    Private_key = base64:decode(proplists:get_value(private_key, Config)),
    Key = key_from_string(Private_key, proplists:get_value(private_key_password, Config)), 
    make_signed_jwt(Key,  Payload).

key_from_string(Private_key, _Private_key_password)->
    [DSAEntry] = public_key:pem_decode(Private_key),
    public_key:pem_entry_decode(DSAEntry).

	
make_signed_jwt(Key, Payload)->

    Header = {[{<<"typ">>,<<"JWT">>}, {<<"alg">>,<<"RS256">>}]},

    Segments = [
		'_urlsafe_b64encode'('_json_encode'(Header)),
		'_urlsafe_b64encode'('_json_encode'(Payload))
	       ],

    Signing_input = binary:list_to_bin(string:join(lists:map(fun binary:bin_to_list/1, Segments), ".")),


    #'PrivateKeyInfo'{privateKeyAlgorithm =
			  #'PrivateKeyInfo_privateKeyAlgorithm'{algorithm = Algo},
		      privateKey = DataKey} = Key, 

    SignKey = case Algo of 
		  ?'id-dsa' ->
		      public_key:der_decode('DSAPrivateKey', iolist_to_binary(DataKey));
		  ?'rsaEncryption' ->
		      public_key:der_decode('RSAPrivateKey', iolist_to_binary(DataKey));
		  _ ->
		      Key
	      end,
    Signature = public_key:sign(Signing_input, sha256, SignKey),
    Segments2 = Segments ++ [ '_urlsafe_b64encode'(Signature)],


    ListSegs = lists:map(fun (Elm) ->
				 if
				     is_binary(Elm) ->
					 binary:bin_to_list(Elm);
				     true ->
					 Elm
				 end
			 end, Segments2),
    StrRes = string:join(ListSegs , "."),
    binary:list_to_bin(StrRes).


apply_headers(Headers, Config, PostData)  ->
    Access_token = proplists:get_value(access_token, Config),
    HeadersWithAuth = [{<<"accept">>, <<"application/json">>} | 
		       lists:keystore(<<"Authorization">>, 1, Headers, 
				      {<<"Authorization">>, << <<"Bearer ">>/binary, Access_token/binary >>})],
    case PostData of 
	<<>> ->
	    HeadersWithAuth;
	_ ->
	    [{<<"Content-Length">>, byte_size(PostData)}| HeadersWithAuth]
    end.

now_sec({MegaSecs,Secs,_MicroSecs})->
    (MegaSecs*1000000 + Secs).


'_json_encode'(Object)->
    
    case (catch jiffy:encode(Object) ) of 
	{'EXIT', _Reason} = Err ->
	    throw(Err);
	Res ->
	    Res
    end.


'_urlsafe_b64encode'(String)->
    Encoded = base64:encode(String),
    EncodedStr = binary:bin_to_list(Encoded),
    binary:list_to_bin(string:strip(EncodedStr, right, $=)). %% remove trailing '=' and return binary
    


%% @doc returns binary of instance default service account
get_app_service_account()->
    get_app_service_property(<<"email">>).


get_app_service_scope()->
    get_app_service_property(<<"scopes">>).

get_app_service_property(Property) when is_binary(Property)->
    BaseUrl = <<"http://169.254.169.254/computeMetadata/v1/instance/service-accounts/default/">>,
    Url = << BaseUrl/binary, Property/binary >>,
    {ok, 200, _RespHeaders, ClientRef} = hackney:request(get, Url,
							       [{<<"Metadata-Flavor">>,<<"Google">>}], <<>>,
							       []),
    
    {ok, RespBody,_ } = hackney:body(ClientRef),
    io:format("RespBody is ~p~n", [RespBody]),
    RespBody.
		
	    


safe_to_bin(List) when is_list(List)->
    binary:list_to_bin(List);
safe_to_bin(Bin)  when is_binary(Bin)->
    Bin.
