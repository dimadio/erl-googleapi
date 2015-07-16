
**Erlang googleapi**

---

Aim is to build server-side oriented erlang library for work with google API

Working with the lib is simple:

Start app:

    application:ensure_started(googleapi).


Init credentials:

    googleapi:init_credentials(_Service_account_name = ?USER_EMAIL,
        			     _Private_key = ?PEM_FILE,
    	    		     _Scope="Desired Scope ").


or
   
    googleapi:init_credentials(_JsonFilePath = ?JSON_FILE_PATH,
                               _Scope="Desired Scope ").


or

    googleapi:init_credentials(service).
    
    
    
Email, json file and pem file are retrieved from "Credentials" screen of GCP project console

Initializing as *service* is intended to work inside GCE instance and will use instance permissions and scope

For example, working with BigQuery, the scope will be "https://www.googleapis.com/auth/bigquery"


Init the API client:

    googleapi:build("bigquery", "v2").


Calling object methods:

    googleapi_client:call("bigquery", "datasets", "list", [{<<"projectId">>, <<"project name">>}]).

Insert data to BigQuery:

    Datarow = {[
                {<<"kind">>, <<"bigquery#tableDataInsertAllRequest">>},
                {<<"skipInvalidRows">>, <<"false">> },
                {<<"ignoreUnknownValues">>, <<"true">>},
                {<<"rows">>, [ ... ]} ]}
    Dataset_Json = jiffy:encode(Datarow),
    {Code, Headers , InsertResult} = googleapi:call("bigquery", "tabledata","insertAll",
                                                       [{<<"projectId">>, ?PROJECT_ID},
                                                        {<<"datasetId">>, ?DATASET},
                                                        {<<"tableId">>, ?TABLENAME},
                                                        {body, Dataset_Json }]).
