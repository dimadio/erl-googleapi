# erl-googleapi
Aim is to build server-side oriented erlang library for work with google API

Working with the lib is simple:

Start app:
application:ensure_started(googleapi).

Init credentials:
googleapi:init_credentials(_Service_account_name = ?USER_EMAIL,
    			     _Private_key = ?PEM_FILE,
    			     _Scope="Desired Scope ").

Email and pem file are retrieved from "Credentials" screen of GCP project console
For example, working with BigQuery, the scope will be "https://www.googleapis.com/auth/bigquery"

Init the API client:
googleapi:build("bigquery", "v2").

Calling object methods:
googleapi_client:call("bigquery", "datasets", "list", [{<<"projectId">>, <<"project name">>}]).