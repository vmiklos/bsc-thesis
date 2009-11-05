-module(terminal).
-export([start/0, stop/0, ping/0, notify/1]).

start() -> register(center, spawn(fun() -> rpc:call(center0@clevo.local, center, subscribe, [foo, node()]), loop() end)).

stop() -> center ! stop.

ping() -> rpc({ping}).

notify(Message) -> rpc({notify, Message}).

rpc(Q) ->
	center ! {self(), Q},
	receive
		{center, Reply} ->
			Reply
	end.

loop() ->
	receive
		{From, {ping}} ->
			From ! {center, pong},
			loop();
		{From, {notify, Message}} ->
			error_logger:info_msg("got notification: ~p~n", [Message]),
			From ! {center, ok},
			loop();
		stop ->
			erase(sensors),
			erase(subscriptions),
			exit(normal)
	end.
