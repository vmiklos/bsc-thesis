-module(driver).
-export([translate/2]).

sub(Str,Old,New) ->
	RegExp = "\\Q"++Old++"\\E",
	re:replace(Str,RegExp,New,[multiline, {return, list}]).

translate(Node, Num) ->
	Module = sub(sub(atom_to_list(Node), "@", "_"), ".", "_"),
	compile:file(Module),
	Modulea = list_to_atom(Module),
	list_to_atom(Modulea:translate(Num)).
