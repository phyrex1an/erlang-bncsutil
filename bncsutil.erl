-module(bncsutil).
-on_load(init/0).
-export([init/0,echo/1,hash_cdkey/3]).

init() ->
    ok = erlang:load_nif("./bin/bncsutil", 0),
    true.

echo(_Str) ->
    nif_error().

hash_cdkey(_CDKey,_ClientToken,_ServerToken) ->
    nif_error().

nif_error() ->
    {error, "NIF not loaded yet"}.
					    
