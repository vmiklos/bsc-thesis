-module(terminal).
-export([start/0, stop/0, ping/0, notify/1]).

start() -> register(terminal, spawn(fun() -> rpc:call(center0@clevo.local, center, subscribe, [foo, node()]), loop() end)).

stop() -> terminal ! stop.

ping() -> rpc({ping}).

notify(Message) -> rpc({notify, Message}).

rpc(Q) ->
	terminal ! {self(), Q},
	receive
		{terminal, Reply} ->
			Reply
	end.

loop() ->
	receive
		{From, {ping}} ->
			From ! {terminal, pong},
			loop();
		{From, {notify, Message}} ->
			error_logger:info_msg("got notification: ~p~n", [Message]),
			From ! {terminal, ok},
			loop();
		stop ->
			init:stop()
	end.
