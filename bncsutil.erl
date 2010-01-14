-module(bncsutil).
-on_load(init/0).
-export([init/0]

-export([echo/1,
	 hash_cdkey/3,
	 extract_mpq_number/1,
	 check_revision/3,
	 get_exe_version/2
	]).

init() ->
    ok = erlang:load_nif("./bin/bncsutil", 0),
    true.

%% string()
echo(_Str) ->
    nif_error().

%% {ok, PublicValue = int(), Product = int(), Hash = string()}
%% {error, Reason = string()}
hash_cdkey(_CDKey,_ClientToken,_ServerToken) ->
    nif_error().

%% {ok, MpqNumber = int()}
%% {error, Reason = string()}
extract_mpq_number(_MPQName) ->
    nif_error().

%% {ok, Checksum = long()}
%% {error, Reason = string()}
check_revision(_ValueString, _Files, _MpqNumber) ->
    nif_error().

%% {ok, ExeInfo = string(), Version = int()}
%% {error, Reason = string()}
get_exe_version(_FileName, _Platform) ->
    nif_error().
    

nif_error() ->
    {error, "NIF not loaded yet"}.
					    
