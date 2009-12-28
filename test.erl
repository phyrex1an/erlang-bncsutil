-module(test).
-export([test/0]).

test() ->
	bncsutil:hash_cdkey("KKMRVG8ZRDYP6RTDTKPKXKHX88", 5887, 888904320).
