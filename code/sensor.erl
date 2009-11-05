-module(sensor).
-export([start/1, stop/0, ping/0]).

start(ConfigFile) ->
	Message = {data, desc, node(), foo, false},
	{ok, Config} = file:consult(ConfigFile),
	Centers = [ C || {center, C} <- Config],
	register(sensor, spawn(fun() ->
		lists:foreach(fun(I) -> rpc:call(I, center, notify, [Message]) end, Centers),
		loop()
	end)).

stop() -> sensor ! stop.

ping() -> rpc({ping}).

rpc(Q) ->
	sensor ! {self(), Q},
	receive
		{sensor, Reply} ->
			Reply
	end.

loop() ->
	receive
		{From, {ping}} ->
			From ! {sensor, pong},
			loop();
		stop ->
			init:stop()
	end.
