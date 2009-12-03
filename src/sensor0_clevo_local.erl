%% @doc Sample driver for sensor0.
%% It has a single description query function only.
%% @end
-module(sensor0_clevo_local).
-export([translate/1]).

%% @doc Return the name of the Numth method of this sensor.
translate(Num) ->
	case Num of
		0 ->
			"querydesc";
		_ ->
			false
	end.
