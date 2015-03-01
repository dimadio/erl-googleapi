-module(googleapi_client).

-behaviour(gen_server).

-define(API_URL, <<"https://www.googleapis.com">>).
-define(SLASH, <<"/">>).



-export([start_link/4,
         init/1,
         code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
	 stop/1]).

-export([call/4]).

call(Pid, Object, Command, Params)->
    gen_server:call(Pid, {call, {Object, Command, Params}}).

stop(Pid) ->
    gen_server:cast(Pid, stop).

init(Settings)->
    {ok, Settings}.

start_link(Service, Version, Json, Auth_http)->

    gen_server:start_link( ?MODULE, [{service, Service},
				     {version, Version},
				     {http, Auth_http}, 
				     {json, Json}],[]).


handle_call({call, {Object, Command, Params}}, _From, Config) when is_list(Object) ->
    handle_call({call, {binary:list_to_bin(Object), Command, Params}}, _From, Config);

handle_call({call, {Object, Command, Params}}, _From, Config) when is_list(Command) ->
    handle_call({call, {Object, binary:list_to_bin(Command), Params}}, _From, Config);

handle_call({call, {Object, Command, Params}}, _From, Config) when is_binary(Command) andalso is_binary(Command) ->

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

    Result = (catch do_handle_call(Object, UpdatedCommand, UpdatedParams, Config)) ,
    {reply, Result, Config};
handle_call(CallData, _From, Config)->
    {reply, ok, Config}.

handle_cast(stop, State) ->

    {stop, normal, State};

handle_cast(_Command, _Config) ->
    ok.

handle_info(_Command, _Config) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(normal, _State) ->
    ok;
terminate({error, Reason}, _State) ->
    {error, Reason}.



%%% --------------------

do_handle_call(Object, Command, Params, Config)->
    Auth_http = proplists:get_value(http, Config),
    ObjectJson = service_builder:get_object_json(proplists:get_value(json, Config), Object),

    {MethodJson} = service_builder:get_method_json(ObjectJson, Command),

    case build_request(MethodJson, Params, Command, Config) of
	{Method, Uri, Headers, Payload} ->

	    case Method of 
		<<"GET">> ->
		    auth_http:get(Auth_http, Uri, Headers);
		<<"POST">> ->
		    auth_http:post(Auth_http, Uri, Headers, Payload);
		<<"DELETE">> ->
		    auth_http:delete(Auth_http, Uri, Headers)
	    end
    end.
    

build_request(MethodJson, Params, _Command, Config ) ->
    %% Methodpath = proplists:get_value(<<"path">>, MethodJson),

    BaseUrl = << ?API_URL/binary%%  , ?SLASH/binary, 
		 %% (proplists:get_value(service, Config))/binary, ?SLASH/binary,
		 %% (proplists:get_value(version, Config))/binary, ?SLASH/binary
		 %% ,
		 %% Methodpath/binary 
	      >>,

    Method = proplists:get_value(<<"httpMethod">>, MethodJson),

    ok = check_required_params(Params, MethodJson),
    
    {Url, QS, Headers, Post} = build_params(BaseUrl, Params, MethodJson, Config),

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


get_command_uri(MethodJson, Config)->
    case  proplists:get_value(<<"mediaUpload">>, MethodJson) of 
	undefined ->
	    Path = proplists:get_value(<<"path">>, MethodJson),
	    { << ?SLASH/binary, 
	       (proplists:get_value(service, Config))/binary, ?SLASH/binary,
	       (proplists:get_value(version, Config))/binary, ?SLASH/binary,
	       Path/binary >>, []};
	{UploadConfig} ->
	    {Protocols} = proplists:get_value(<<"protocols">>, UploadConfig),
	    {Simple} = proplists:get_value(<<"simple">>, Protocols),
	    { proplists:get_value(<<"path">>, Simple), [ { <<"uploadType">>, <<"media">>}] }
    end.
    
  
build_params(BaseUrl, Params , MethodJson, Config) ->
    {Command, QS} = get_command_uri(MethodJson, Config),
    build_params(<< BaseUrl/binary, Command/binary >>, Params , MethodJson, Config, {_QS = QS, _Headers = [], _Postbody = <<>>}).

build_params(BaseUrl, _Params = [] , _MethodJson, _Config, {QS, Headers, Postbody}) ->
    {BaseUrl, QS, Headers, Postbody};
build_params(BaseUrl, [{ParamName, ParamValue} = Param |RestParams], MethodJson, Config, {QS, Headers, Postbody}) ->
    {Parameters} = proplists:get_value(<<"parameters">>, MethodJson),


    case proplists:get_value(ParamName, Parameters) of 
	undefined ->
	    case ParamName of
		body ->
		    build_params(BaseUrl, RestParams, MethodJson, Config, {QS, Headers, << Postbody/binary, ParamValue/binary >>} );
		alt ->
		    build_params(BaseUrl, RestParams, MethodJson, Config, {lists:keystore( ParamName, 1, QS, Param), 
									   Headers, << Postbody/binary, ParamValue/binary >>} );
		'content-type' ->
		    build_params(BaseUrl, RestParams, MethodJson, Config, {QS, [{<<"Content-Type">>, ParamValue} | Headers], Postbody} );
		_ ->
		    build_params(BaseUrl, RestParams, MethodJson, Config, {QS, Headers, Postbody} )
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

	    build_params(NewUrl, RestParams, MethodJson, Config, {NewQS, NewHeaders, NewPostbody} )
    end.

    
convert_qs_to_bin(QS)->
    binary:list_to_bin(utils:url_encode(QS)).
