-module(googleapi_client).

-behaviour(gen_server).

-define(API_URL, <<"https://www.googleapis.com">>).
-define(SLASH, <<"/">>).



-export([start_link/3,
         init/1,
         code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
	 stop/1]).

-export([call/4]).

-spec call(Service :: googleapi:name(), Object:: googleapi:name(), Command :: googleapi:name(), Params :: [googleapi:name()]) ->
		  {_StatusCode :: integer(), RespHeaders :: [{binary(), binary()}], binary()} | {error, _}.
call(Service, Object, Command, Params)->
    handle_call_prepare( Object, Command, Params, Service).

-spec stop(Service::googleapi:name()) -> ok.
stop(Service) ->
    gen_server:cast(get_service_name(Service), stop).

-spec init([any()])-> {ok, [any()] }.
init(Settings)->
    {ok, Settings}.

-spec get_object_json(Service:: googleapi:name(), Object :: googleapi:name())-> {[any()]} .
get_object_json(Service, Object)->
    gen_server:call( get_service_name(Service), {get_object_json, Object}).

-spec get_service(Service::googleapi:name())-> binary() .
get_service(Service)->
    gen_server:call( get_service_name(Service), get_service).

-spec get_version(Service::googleapi:name())-> binary() .
get_version(Service)->
    gen_server:call( get_service_name(Service), get_version).


-spec start_link(Service::googleapi:name(), Version::googleapi:name(), Json::binary())->{error,_} | {ok,pid()} | {ok,pid(),_} .
start_link(Service, Version, Json)->

    gen_server:start_link( {local, get_service_name(Service)},  ?MODULE, [{service, Service},
				     {version, Version},
				     {json, Json}],[]).

-spec handle_call( any(), _From :: {pid(),_} , any()) -> {reply, any(), any()} .
handle_call( get_service, _From, Config) ->
    {reply, proplists:get_value(service, Config), Config} ;
handle_call( get_version, _From, Config) ->
    {reply, proplists:get_value(version, Config), Config} ;
handle_call( {get_object_json, Object}, _From, Config) ->
    ObjectJson = service_builder:get_object_json(proplists:get_value(json, Config), Object),
    {reply, ObjectJson, Config};
handle_call(_CallData, _From, Config)->
    {reply, ok, Config}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Command, Config) ->
    {noreply, Config}.

handle_info(_Command, Config) ->
    {noreply, Config}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(normal, _State) ->
    ok;
terminate({error, Reason}, _State) ->
    {error, Reason}.



%%% --------------------


handle_call_prepare(Object, Command, Params, Service) when is_list(Object) ->
    handle_call_prepare(binary:list_to_bin(Object), Command, Params, Service);

handle_call_prepare(Object, Command, Params, Service) when is_list(Command) ->
    handle_call_prepare(Object, binary:list_to_bin(Command), Params, Service);

handle_call_prepare(Object, Command, Params, Service) when is_binary(Object) andalso is_binary(Command) ->

    Media = <<"_media">>,
    Media_suffix_length = byte_size(<<"_media">>),
    
    Match = binary:longest_common_suffix([Command,Media]),
    {UpdatedCommand, UpdatedParams} = 
	case Match of 
	    Media_suffix_length ->
		{binary:part(Command, 0, byte_size(Command) - Media_suffix_length), [{alt, <<"media">>} |Params ]};
	    _ ->
		{Command, %% [{alt, <<"json">>} |
			   Params %% ]
		    }
	end,

    (catch do_handle_call(Object, UpdatedCommand, UpdatedParams, Service)).


do_handle_call(Object, Command, Params, Service)->
    
    ObjectJson = get_object_json(Service, Object), %% service_builder:get_object_json(proplists:get_value(json, Config), Object),

    {MethodJson} = service_builder:get_method_json(ObjectJson, Command),

    case build_request(Service, MethodJson, Params, Command) of
	{Method, Uri, Headers, Payload} ->

	    case Method of 
		<<"GET">> ->
		    auth_http:get(Uri, Headers);
		<<"POST">> ->
		    auth_http:post(Uri, Headers, Payload);
		<<"DELETE">> ->
		    auth_http:delete(Uri, Headers)
	    end
    end.
    

build_request(Service, MethodJson, Params, _Command ) ->
    %% Methodpath = proplists:get_value(<<"path">>, MethodJson),

    BaseUrl = << ?API_URL/binary%%  , ?SLASH/binary, 
		 %% (proplists:get_value(service, Config))/binary, ?SLASH/binary,
		 %% (proplists:get_value(version, Config))/binary, ?SLASH/binary
		 %% ,
		 %% Methodpath/binary 
	      >>,

    Method = proplists:get_value(<<"httpMethod">>, MethodJson),

    ok = check_required_params(Params, MethodJson),
    
    {Url, QS, Headers, Post} = build_params_pre(Service, BaseUrl, Params, MethodJson),

    BinQS = convert_qs_to_bin(QS),
    case BinQS of
	<<>> ->
	    {Method, Url, Headers, Post};
	_ ->
	    {Method, << Url/binary, <<"?">>/binary, BinQS/binary >> , Headers, Post}
    end.
		 

check_required_params(Params, MethodJson) ->
    {Parameters} = proplists:get_value(<<"parameters">>, MethodJson),
    check_required_params_in(Params, Parameters).

check_required_params_in(_Params, _Parameters = [])->
    ok;
check_required_params_in(Params, [{Pname, {PData} }|RestParameters]) ->
    Required = proplists:get_value(<<"required">>, PData),
    case Required of
	undefined ->
	    check_required_params_in(Params, RestParameters);
	true ->
	    case proplists:get_value(Pname, Params) of
		undefined ->
		    exit({error, "Required parameter '" ++ binary:bin_to_list(Pname) ++ "' not found"});
		_ ->
		    check_required_params_in(Params, RestParameters)
			end
    end.


get_command_uri(Service, MethodJson)->
    case  proplists:get_value(<<"mediaUpload">>, MethodJson) of 
	undefined ->
	    Path = proplists:get_value(<<"path">>, MethodJson),
	    { << ?SLASH/binary, 
	       (get_service(Service))/binary, ?SLASH/binary,
	       (get_version(Service))/binary, ?SLASH/binary,
	       Path/binary >>, []};
	{UploadConfig} ->
	    {Protocols} = proplists:get_value(<<"protocols">>, UploadConfig),
	    {Simple} = proplists:get_value(<<"simple">>, Protocols),
	    { proplists:get_value(<<"path">>, Simple), [ { <<"uploadType">>, <<"media">>}] }
    end.
    
  
build_params_pre(Service, BaseUrl, Params , MethodJson) ->
    {Command, QS} = get_command_uri(Service, MethodJson),
    build_params(<< BaseUrl/binary, Command/binary >>, Params , Params,  MethodJson,  {_QS = QS, _Headers = [], _Postbody = <<>>}).

build_params(BaseUrl, _Params = [] , _AllParams, _MethodJson,  {QS, Headers, Postbody}) ->
    {BaseUrl, QS, Headers, Postbody};
build_params(BaseUrl, [{ParamName, ParamValue} = Param |RestParams], AllParams, MethodJson, {QS, Headers, Postbody}) ->
    {Parameters} = proplists:get_value(<<"parameters">>, MethodJson),


    case proplists:get_value(ParamName, Parameters) of 
	undefined ->
	    case ParamName of
		body ->
		    %% check if we have alt = media in params. if not - put json as content-type
		    case proplists:get_value(alt, AllParams) of
			<<"media">> ->
			    build_params(BaseUrl, RestParams, AllParams, MethodJson,  {QS, Headers, << Postbody/binary, ParamValue/binary >>} );
			_ ->
			    build_params(BaseUrl, RestParams, AllParams, MethodJson,  {QS, 
										       [{<<"Content-Type">>, <<"application/json">>} | Headers], 
										       << Postbody/binary, ParamValue/binary >>} )
		    end;
		alt ->
		    build_params(BaseUrl, RestParams, AllParams, MethodJson, {lists:keystore( ParamName, 1, QS, Param), 
									   Headers, << Postbody/binary, ParamValue/binary >>} );
		'content-type' ->
		    build_params(BaseUrl, RestParams, AllParams, MethodJson,  {QS, [{<<"Content-Type">>, ParamValue} | Headers], Postbody} );
		_ ->
		    build_params(BaseUrl, RestParams, AllParams, MethodJson,  {QS, Headers, Postbody} )
	    end;
	{ParamInfo} ->

	    Location = proplists:get_value(<<"location">>, ParamInfo),

	    {NewUrl, NewQS, NewHeaders, NewPostbody} =  
		case Location of 
		    <<"query">> ->
			{BaseUrl, lists:keystore( ParamName, 1, QS, Param), Headers, Postbody};
		    <<"path">> ->
			Url2 = binary:replace(BaseUrl, << <<"{">>/binary, ParamName/binary, <<"}">>/binary >>, ParamValue),
			{Url2, QS, Headers, Postbody}		   
		end,

	    build_params(NewUrl, RestParams, AllParams, MethodJson, {NewQS, NewHeaders, NewPostbody} )
    end.

    
convert_qs_to_bin(QS)->
    binary:list_to_bin(utils:url_encode(QS)).





get_service_name(Service) when is_binary(Service) ->
    list_to_atom(binary:bin_to_list(Service));

get_service_name(Service) when is_list(Service) ->
    list_to_atom(Service);

get_service_name(Service) when is_atom(Service) ->
    Service.

