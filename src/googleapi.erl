-module(googleapi).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(USER_EMAIL, "EMAIL").
-define(PEM_FILE, "PEM FILE").

-endif.

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% Service exports
-export([build/2]).

-export([start_deps/0, init_credentials/3, init_credentials/2, init_credentials/1, close_credentials/0, call/4, stop_client/1]).


-type name() :: binary()|string().
-export_type([name/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec start(_StartType :: atom(), _StartArgs :: [any()] ) -> {error,_} | {ok,pid()} | {ok,pid(),_} .
start(_StartType, _StartArgs) ->
    start_deps(),
    googleapi_sup:start_link().

-spec stop(_State :: atom() ) -> ok.
stop(_State) ->
    ok.

-spec start_deps() -> ok.
start_deps() ->
    ok = ensure_started([crypto, asn1, public_key, ssl, hackney]).


-spec init_credentials( Service_account_name :: name() , 
			Private_key :: name(), 
			Scope :: name() | [name() ]) -> {error,_} | {ok,pid()} | {ok,pid(),_} .
init_credentials(Service_account_name, Private_key, Scope)->
    googleapi_sup:add_child(auth_http, 
			    auth_http, 
			    worker, 
			    [Service_account_name, Private_key, Scope]).

-spec init_credentials( JsonFilePath :: name(), 
			Scope :: name() | [name() ]) -> {error,_} | {ok,pid()} | {ok,pid(),_} .
init_credentials(JsonFilePath, Scope)->
    googleapi_sup:add_child(auth_http, 
			    auth_http, 
			    worker, 
			    [JsonFilePath, Scope]).

-spec init_credentials( service ) -> {error,_} | {ok,pid()} | {ok,pid(),_} .
init_credentials(service)->
    googleapi_sup:add_child(auth_http, 
			    auth_http, 
			    worker, 
			    []).


-spec close_credentials()-> ok.
close_credentials()->
    googleapi_sup:stop_child(auth_http).


-spec call(Service :: name(), Object:: name(), Command :: name(), Params :: [name()]) ->
		  {_StatusCode :: integer(), RespHeaders :: [{binary(), binary()}], binary()} | {error, _}.
call(Service, Object, Command, Params) ->
    googleapi_client:call(Service, Object, Command, Params).


-spec stop_client(Client :: name())-> ok.
stop_client(Client)->
    googleapi_client:stop(Client).
%%% service_functions


-spec ensure_started([atom()]) -> ok | {error, _}.
ensure_started([]) ->
    ok;
ensure_started([App|RestApps]) ->
    case application:ensure_started(App) of 
	ok ->
	    ensure_started(RestApps);
	Other ->
	    {error, Other}
    end.


-spec build(Service::name(), Version :: name()) -> {error,_} | {ok,pid()} | {ok,pid(),_} .
build(Service, Version) when  is_list(Service)->
    build( binary:list_to_bin(Service), Version);

build(Service, Version) when  is_list(Version)->
    build( Service, binary:list_to_bin(Version));

build(Service, Version) when is_binary(Service) andalso is_binary(Version) ->
    ServiceUrl = << <<"https://www.googleapis.com/discovery/v1/apis/">>/binary,
		    Service/binary, <<"/">>/binary, Version/binary, <<"/rest">>/binary >>,
    lager:info("ServiceUrl = ~p~n",[ServiceUrl]),
    {ok, Json} = service_builder:bring_service_json(ServiceUrl),
    lager:info("Json OK"),
    ok = service_builder:validate_service_json(Json, Service, Version),
    lager:info("validate_service_json OK"),
    googleapi_sup:add_child(Service, googleapi_client, worker, [Service, 
				Version,
				_ServiceJson = Json]).




-ifdef(TEST).

check_response_delete({Code, _RespHead, RespBody}) ->
    ?debugFmt("Delete response : ~p, ~p~n", [Code, RespBody]),
    case Code of
	204 ->
	    ok;
	_ ->
	    Code
    end.
    
check_response( {'EXIT', Reason} ) ->
    ?debugFmt("Failed as ~p", [Reason]),
    Reason;
check_response( {Code, RespHead, RespBody} ) ->
    case Code of
	200 ->
	    Content_Type = proplists:get_value(<<"Content-Type">>, RespHead),
	    
	    case binary:longest_common_prefix([Content_Type, <<"application/json">>]) of
		16  -> %% length of application/json
		    RespJson = case RespBody of 
				   <<>> ->
				       [];
				   _ ->
				       {ARespJson} = jiffy:decode(RespBody),
				       ARespJson
			       end,

		    Items = proplists:get_value(<<"items">>, RespJson),
		    case Items of
			undefined ->
			    %% ?debugFmt("Response json: ~p", [RespJson]),
			    ok;

			_ ->
			    lists:foldl(fun ({Item}, _) ->
					ItemId = proplists:get_value(<<"id">>, Item),
						?debugFmt("Found item ~p", [ItemId])
					end, 0, Items)
		    end;
		_ ->
		    ok
	    end,
	    ok;
	_ ->
	    ?debugFmt("1) Response content: ~p", [RespBody]),
	    ?debugFmt("2) Response header: ~p", [RespHead]),
	    Code
    end.


%% test_download_file({Code, RespHead, RespBody}, AuthHttp)->
%%     case Code of 
%% 	200 ->
%% 	    {RespJson} = jiffy:decode(RespBody),
%% 	    Link = proplists:get_value(<<"mediaLink">>, RespJson),
%% 	    lager:info("Link: ~p", [Link]),
%% 	    Response = auth_http:get(AuthHttp, Link, []),
%% 	    lager:info("Response: ~p", [Response]),
%% 	    ok;
%% 	_ ->
%% 	    error
%%     end.

test_storage_init()->
    ?debugFmt("Run build storage test ~n",[]),
    lager:start(),

    ok = application:ensure_started(googleapi),



    HTTPAuth = googleapi:init_credentials(_Service_account_name = ?USER_EMAIL,
					  _Private_key = ?PEM_FILE,
					  _Scope="https://www.googleapis.com/auth/devstorage.full_control"),

    ?debugFmt("HTTPAuth = ~p~n",[ HTTPAuth] ),

    ClientRes = build("storage", "v1"),
    ?debugFmt("ClientRes = ~p~n",[ ClientRes] ),
    ok.


build_test_()->
    Bucket_name = <<"testbucket24566123a2">>,
    Bucket_object = jiffy:encode({[{<<"name">>, Bucket_name}]}),
    ?debugFmt("Bucket_object=~p~n", [Bucket_object]),

    [
     {timeout, 60000,  ?_assert(  test_storage_init() == ok  ) },
     %% ?_assertEqual(ok, check_response(googleapi:call("storage", "buckets", "insert", [{<<"project">>,<<"wixpop-gce">>}, 
     %% 											     {'content-type', <<"application/json">>},
     %% 											     {body, Bucket_object  }
     %% 											    ])) ),

     {timeout, 60000,
      ?_assertEqual(ok, check_response(googleapi:call("storage", "buckets", "list", [{<<"project">>,<<"wixpop-gce">>}, 
										     {<<"prefix">>, <<"video-dev">>}])) ) },

     %% %%-------------------------------

     %% ?_assertEqual(ok, check_response(googleapi:call("storage", "objects", "list", [{<<"bucket">>,Bucket_name}, {<<"maxResults">>, <<"10">>}])) ),
     %% ?_assertEqual({error,"Unsupported method lista"}, 
     %% 		   check_response(googleapi:call("storage", "objects", "lista", [{<<"bucket">>,Bucket_name}, {<<"maxResults">>, <<"10">>}])) ),
     %% ?_assertEqual({error,"Required parameter 'bucket' not found"}, 
     %% 		   check_response(googleapi:call("storage", "objects", "list", [{<<"prefix">>, <<"micons/">>}, {<<"maxResults">>, <<"10">>}])) ),
     %% %%-------------------------------

     %% ?_assertEqual(ok, 
     %% 		   check_response(googleapi:call("storage", "objects", "insert", [{<<"bucket">>,Bucket_name},
     %% 											 {<<"name">>, <<"erltest3.txt">>},
     %% 											 {'content-type', <<"text/plain">>},
     %% 											 {body, <<"sometext data\ndata5">>}])) ),

     %% ?_assertEqual(ok, 
     %% 		   check_response(googleapi:call("storage", "objects", "list", [{<<"bucket">>,Bucket_name}])) ),

     %% ?_assertEqual(ok, 
     %% 		   check_response(googleapi:call("storage", "objects", "get", [{<<"bucket">>,Bucket_name},
     %% 										      {<<"object">>, <<"erltest3.txt">>}])) ),

     %% ?_assertEqual(ok, 
     %% 		   check_response(googleapi:call("storage", "objects", "get_media", [{<<"bucket">>,Bucket_name},
     %% 											    {<<"object">>, <<"erltest3.txt">>}])) ),

     %% {timeout, 1000, ?_assertEqual(ok, 
     %% 		   check_response_delete(googleapi:call("storage", "objects", "delete", [{<<"bucket">>,Bucket_name},
     %% 												{<<"object">>, <<"erltest3.txt">>}])) ) },


     %% ?_assertEqual(ok, 
     %% 		   check_response_delete(googleapi:call("storage", "buckets", "delete", [{<<"bucket">>,Bucket_name}])) ),


     ?_assertEqual(ok, googleapi:stop_client("storage")),
      ?_assertEqual(ok, googleapi:close_credentials())
    ]
	.


test_bq_init()->
    ?debugFmt("Run build bq test ~n",[]),

    ok = application:ensure_started(googleapi),

    HTTPAuth = googleapi:init_credentials(_Service_account_name = ?USER_EMAIL,
    			     _Private_key = ?PEM_FILE,
    			     _Scope="https://www.googleapis.com/auth/bigquery"),

    ?debugFmt("HTTPAuth = ~p~n",[ HTTPAuth] ),

    ClientRes = build("bigquery", "v2"),
    ?debugFmt("ClientRes = ~p~n",[ ClientRes] ),
    ok.


build_bq_test_()->

    [
     {timeout, 60000, ?_assert(  test_bq_init() == ok  ) },
     {timeout, 60000, 
      ?_assertEqual(ok, check_response(googleapi:call("bigquery", "datasets", "list", [{<<"projectId">>, <<"wixpop-gce">>}])) ) },
     {timeout, 60000,
      ?_assertEqual(ok, check_response(googleapi:call("bigquery", "tables", "list", [{<<"projectId">>, <<"wixpop-gce">>},
										     {<<"datasetId">>, <<"prospero">>}])) ) },

     ?_assertEqual(ok, googleapi:stop_client("bigquery")),
     ?_assertEqual(ok, googleapi:close_credentials())
    ].

-endif.

