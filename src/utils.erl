-module(utils).

-export([url_encode/1,
	 build_body/1]).


%% @doc A function to URL encode form data.
%% @spec url_encode(list()).

-spec(url_encode(list()) -> string()).
url_encode(Data) ->
    url_encode(Data,"").

-spec(url_encode(list(), string()) -> string()).
url_encode([],Acc) ->
    Acc;

url_encode([{Key,Value}|R],Acc) when is_binary(Key) ->
    url_encode([{binary:bin_to_list(Key),Value}|R],Acc);
url_encode([{Key,Value}|R],Acc) when is_atom(Key) ->
    url_encode([{atom_to_list(Key),Value}|R],Acc);
url_encode([{Key,Value}|R],Acc) when is_binary(Value) ->
        url_encode([{Key, binary:bin_to_list(Value)}|R],Acc);
url_encode([{Key,Value}|R],"") ->
    url_encode(R, edoc_lib:escape_uri(Key) ++ "=" ++ edoc_lib:escape_uri(Value));
url_encode([{Key,Value}|R],Acc) ->
    url_encode(R, Acc ++ "&" ++ edoc_lib:escape_uri(Key) ++ "=" ++ edoc_lib:escape_uri(Value)).


-spec build_body(Data::[{term(), term()}])->binary().
build_body(Data)->
    build_body(Data, <<>>).

-spec build_body(Data::[{term(), term()}], binary())->binary().
build_body([], Acc)->
    Acc;

build_body([{Key, Value}|Rest], Acc) when is_list(Key) ->
    build_body([{binary:list_to_bin(Key), Value}|Rest], Acc);
build_body([{Key, Value}|Rest], Acc) when is_atom(Key) ->
    build_body([{binary:list_to_bin(atom_to_list(Key)), Value}|Rest], Acc);
build_body([{Key, Value}|Rest], Acc) when is_list(Value) ->
    build_body([{Key, binary:list_to_bin(Value)}|Rest], Acc);

build_body([{Key, Value}|Rest], <<>>) when is_binary(Key) andalso is_binary(Value) ->
    build_body(Rest, << Key/binary, <<"=">>/binary, Value/binary >>);
build_body([{Key, Value}|Rest], Acc)->
    build_body(Rest, << Acc/binary, <<"&">>/binary, Key/binary, <<"=">>/binary, Value/binary >>).
    
    
