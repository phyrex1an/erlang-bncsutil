-module(bncsutils_drv).
-export([start/0]).

-export([echo/2, stop/1]).


start() ->
    Port = open_port(
	     {spawn,"/home/phyrex1an/git/wc3-hosting-service/external/bncsutils-drv/bncsutils_drv"}, 
	     [{packet, 4}, binary, exit_status]),
    Pid = spawn(fun() -> loop(Port) end),
    port_connect(Port, Pid),
    Pid.


echo(Pid, Message) ->
    SPid = self(),
    Pid ! {echo, SPid, {Message}},
    receive
	Anything -> Anything
    end.

stop(Pid) ->
    Pid ! {stop}.

loop(Port) ->
    receive
	{echo, Pid, {Message}}->
	    port_command(Port, term_to_binary({echo, {Message}})),
	    receive 
		{Port, {data, Data}} ->
		    Pid ! binary_to_term(Data);
		Anything ->
		    Pid ! {abnormalexit, Anything}
	    end,
	    loop(Port);
	{stop} ->
	    port_close(Port),
	    ok
    end.
