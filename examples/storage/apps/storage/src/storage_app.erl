-module(storage_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(USER_EMAIL, "EMAIL").
-define(PEM_FILE, "PEM FILE").


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Bucket_name = <<"testbucket24566">>,
    Bucket_object = jiffy:encode({[{<<"name">>, Bucket_name}]}),


    googleapi:init_credentials(_Service_account_name = ?USER_EMAIL,
			       _Private_key = ?PEM_FILE,
			       _Scope="https://www.googleapis.com/auth/devstorage.full_control"),

    {ok, _ClientRef} = googleapi:build("storage", "v1"),

    {200, _InsertHeaders, <<>>} = googleapi:call("storage", "buckets", "insert", [{<<"project">>,<<"wixpop-gce">>}, 
								  {'content-type', <<"application/json">>},
								  {body, Bucket_object  }
								 ]),
    

    {204, _DeleteHeaders, <<>>} = googleapi:call("storage", "buckets", "delete", [{<<"bucket">>,Bucket_name}]),


    ok = googleapi:stop_client("storage"),

    storage_sup:start_link().

stop(_State) ->
    ok.
