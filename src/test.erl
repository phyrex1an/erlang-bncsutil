-module(test).
-export([test/0]).

test() ->
    bncsutil:hash_cdkey("KKMRVG8ZRDYP6RTDTKPKXKHX88", 5887, 888904320),
    {ok, 0} = bncsutil:extract_mpq_number("ver-IX86-0.mpq"),
    ValueString = "A=3006076652 C=1958850145 B=1097296642 4 A=A^S B=B^C C=C-A A=A-B",
    WarDir = "/home/phyrex1an/.wine/drive_c/Program Files/Warcraft III/",
    Files = [WarDir ++ "war3.exe",
	     WarDir ++ "Storm.dll",
	     WarDir ++ "game.dll"],
    Revision = bncsutil:check_revision(ValueString, Files, 0),
    EXEVersion = bncsutil:get_exe_version(WarDir ++ "war3.exe", 1),
    {Revision, EXEVersion}.
