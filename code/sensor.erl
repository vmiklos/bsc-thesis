-module(sensor).
-export([start/0, stop/0, ping/0]).

start() ->
	Message = {data, desc, node(), foo, false},
	register(sensor, spawn(fun() -> rpc:call(center0@clevo.local, center, notify, [Message]), loop() end)).

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
