-module(service_builder).

-export([bring_service_json/1,
	 validate_service_json/3,
	 get_object_json/2,
	 get_method_json/2]).

 
-spec bring_service_json(Url::googleapi:name())-> {ok, binary()} | {error, [any()], binary() }.
bring_service_json(Url)->
    Method = get,
    URL = Url,
    Headers = [],
    Payload = <<>>,
    Options = [],
    {ok, StatusCode, _RespHeaders, ClientRef} = hackney:request(Method, URL,
                                                        Headers, Payload,
							       Options),
    {ok, Body, _Client2} = hackney:body(ClientRef),

    case StatusCode of 
	200 ->
	    Json = jiffy:decode(Body) ,
	    { ok, Json};
	_ -> 
	    {error ,StatusCode, Body}
    end.


validate_service_json(Json, Service, Version)->
    {JsonList} = Json,
    case {proplists:get_value(<<"name">>, JsonList),
	  proplists:get_value(<<"version">>, JsonList)} of 
	{Service, Version} ->
	    ok;
	Other ->
	    {error , Other}
    end.

get_object_json(Json, Object) when is_list(Object)->
    get_object_json(Json, binary:list_to_bin(Object));
get_object_json({JsonList}, Object) when is_binary(Object)->
    {Resources} = proplists:get_value(<<"resources">>, JsonList),
    case proplists:get_value(Object, Resources) of
	undefined ->
	    exit({error, "Unsupported object "++binary:bin_to_list(Object)});
	ObjectJson->
	    ObjectJson
    end.


-spec get_method_json(Json :: {[any()]}, googleapi:name()) -> any().
get_method_json(Json, Method) when is_list(Method) ->
    get_method_json(Json, binary:list_to_bin(Method) );
get_method_json({JsonList}, Method) when is_binary(Method) ->
    {Methods} = proplists:get_value(<<"methods">>, JsonList),
    case proplists:get_value(Method, Methods) of
	undefined ->
	    exit({error, "Unsupported method "++binary:bin_to_list(Method)});
	M ->
	    M
    end.

