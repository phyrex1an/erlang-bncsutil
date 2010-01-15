-module(bncsutil).
-on_load(init/0).
-export([init/0]).

-export([echo/1,
	 hash_cdkey/3,
	 extract_mpq_number/1,
	 check_revision/3,
	 get_exe_version/2,
	 nls_init/2,
	 nls_free/1,
	 nls_get_S/3,
	 nls_get_v/2,
	 nls_get_A/1,
	 nls_get_K/2,
	 nls_get_M1/3,
	 nls_check_M2/2,
	 nls_check_signature/2
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


%%
%% Version check
%%

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
    
%%
%% Logon proof
%%

%% {ok, NLS = long()}
nls_init(_Username, _Password) ->
    nif_error().

%% {ok}
nls_free(_NLS) ->
    nif_error().

%% {ok, S = binary()}
nls_get_S(_NLS, _B, _Salt) ->
    nif_error().

%% {ok, V = binary()} (small v)
nls_get_v(_NLS, _Salt) ->
    nif_error().

%% {ok, A = binary()}
nls_get_A(_NLS) ->
    nif_error().

%% {ok, K = binary()}
nls_get_K(_NLS, _S) ->
    nif_error().

%% {ok, M1 = binary}
nls_get_M1(_NLS, _B, _Salt) ->
    nif_error().

%% {ok}
%% {fail}
nls_check_M2(_NLS, _M2) ->
    nif_error().

%% {ok}
%% {fail} 
nls_check_signature(_Address, _RawSignature) ->
    nif_error().

nif_error() ->
    {error, "NIF not loaded yet"}.
					    
