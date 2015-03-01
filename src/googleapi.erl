-module(googleapi).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% Service exports
-export([build/3]).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    googleapi_sup:start_link().

stop(_State) ->
    ok.



%%% service_functions
build(Service, Version, AuthHttp) when  is_list(Service)->
    build( binary:list_to_bin(Service), Version, AuthHttp);

build(Service, Version, AuthHttp) when  is_list(Version)->
    build( Service, binary:list_to_bin(Version), AuthHttp);

build(Service, Version, AuthHttp) when is_binary(Service) andalso is_binary(Version) ->
    ServiceUrl = << <<"https://www.googleapis.com/discovery/">>/binary, Version/binary,
		    <<"/apis/">>/binary, Service/binary, <<"/v1/rest">>/binary >>,
    {ok, Json} = service_builder:bring_service_json(ServiceUrl),
    ok = service_builder:validate_service_json(Json, Service, Version),

    googleapi_client:start_link(Service, 
				Version,
				_ServiceJson = Json, 
				AuthHttp).
    



-ifdef(TEST).

check_response_delete({Code, _RespHead, _RespBody}) ->
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
		    %% ?debugFmt("RespBody: ~p", [RespBody]),
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
			    ok;
			    %% ?debugFmt("Response json: ~p", [RespJson])

			_ ->
			    %% lager:info("~nItems=~p~n", [Items]),
			    lists:foldl(fun ({Item}, _) ->
					ItemId = proplists:get_value(<<"id">>, Item),
						?debugFmt("Found item ~p", [ItemId])
					end, 0, Items)
		    end;
		_ ->
		    ok
		    %% ?debugFmt("Response headers=~p~n", [RespHead]),
		    %% ?debugFmt("~nResponse content=~p~n", [RespBody])
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

build_test_()->
    io:format("Run build_test ~n"),
    application:start(asn1),
    hackney:start(),

    {ok, AuthHttp} = auth_http:start_link(_Service_account_name = "EMAIL",
					  _Private_key = "PEM PATH",
					  _Scope="https://www.googleapis.com/auth/devstorage.full_control"),
    {ok, Client} = build("storage", "v1", AuthHttp),

    Bucket_name = <<"testbucket24566">>,
    Bucket_object = jiffy:encode({[{<<"name">>, Bucket_name}]}),
    ?debugFmt("Bucket_object=~p~n", [Bucket_object]),
    [ 
      ?_assertEqual(ok, check_response(googleapi_client:call(Client, "buckets", "insert", [{<<"project">>,<<"wixpop-gce">>}, 
											   {'content-type', <<"application/json">>},
											   {body, Bucket_object  }
											  ])) ),

      ?_assertEqual(ok, check_response(googleapi_client:call(Client, "buckets", "list", [{<<"project">>,<<"wixpop-gce">>}, 
      											 {<<"prefix">>, <<"wixtestbucket">>}])) ),

      %%-------------------------------

      ?_assertEqual(ok, check_response(googleapi_client:call(Client, "objects", "list", [{<<"bucket">>,Bucket_name}, {<<"maxResults">>, <<"10">>}])) ),
      ?_assertEqual({error,"Unsupported method lista"}, 
      		    check_response(googleapi_client:call(Client, "objects", "lista", [{<<"bucket">>,Bucket_name}, {<<"maxResults">>, <<"10">>}])) ),
      ?_assertEqual({error,"Required parameter 'bucket' not found"}, 
      		    check_response(googleapi_client:call(Client, "objects", "list", [{<<"prefix">>, <<"micons/">>}, {<<"maxResults">>, <<"10">>}])) ),
      %%-------------------------------

      ?_assertEqual(ok, 
      		    check_response(googleapi_client:call(Client, "objects", "insert", [{<<"bucket">>,Bucket_name},
      										       {<<"name">>, <<"erltest3.txt">>},
      										       {'content-type', <<"text/plain">>},
      										       {body, <<"sometext data\ndata5">>}])) ),

      ?_assertEqual(ok, 
      		    check_response(googleapi_client:call(Client, "objects", "list", [{<<"bucket">>,Bucket_name}])) ),

      ?_assertEqual(ok, 
       		    check_response(googleapi_client:call(Client, "objects", "get", [{<<"bucket">>,Bucket_name},
      										    {<<"object">>, <<"erltest3.txt">>}])) ),

      ?_assertEqual(ok, 
      		    check_response(googleapi_client:call(Client, "objects", "get_media", [{<<"bucket">>,Bucket_name},
      											  {<<"object">>, <<"erltest3.txt">>}])) ),

      ?_assertEqual(ok, 
      		    check_response_delete(googleapi_client:call(Client, "objects", "delete", [{<<"bucket">>,Bucket_name},
      											      {<<"object">>, <<"erltest3.txt">>}])) ),


      ?_assertEqual(ok, 
       		    check_response_delete(googleapi_client:call(Client, "buckets", "delete", [{<<"bucket">>,Bucket_name}])) ),


      ?_assertEqual(ok, googleapi_client:stop(Client))
    ].


-endif.
