-module(sensor0_clevo_local).
-export([translate/1]).

translate(Num) ->
	case Num of
		0 ->
			"querydesc";
		_ ->
			false
	end.
