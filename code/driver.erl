%% @doc The driver allows generic access to different sensors.
-module(driver).
-export([translate/2]).

%% @doc Internal function to do replacement in a string.
sub(Str,Old,New) ->
	RegExp = "\\Q"++Old++"\\E",
	re:replace(Str,RegExp,New,[multiline, {return, list}]).

%% @doc Return the name of the Numth method of Node based on its driver.
translate(Node, Num) ->
	Module = sub(sub(atom_to_list(Node), "@", "_"), ".", "_"),
	compile:file(Module),
	Modulea = list_to_atom(Module),
	list_to_atom(Modulea:translate(Num)).
