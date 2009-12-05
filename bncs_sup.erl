%%%-------------------------------------------------------------------
%%% File    : bncs_sup.erl
%%% Author  :  <phyrex1an@phyrex1an-desktop>
%%% Description : 
%%%
%%% Created :  5 Dec 2009 by  <phyrex1an@phyrex1an-desktop>
%%%-------------------------------------------------------------------
-module(bncs_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link(_Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    Server = {bncs_server, {bncs_server, start_link, []},
	      permanent,2000,worker,[bncs_server]},
    {ok,{{one_for_one,0,1}, [Server]}}.

%%====================================================================
%% Internal functions
%%====================================================================
