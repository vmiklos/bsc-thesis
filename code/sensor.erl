-module(sensor).
-export([start/1, stop/0, ping/0]).

% helloworld sensor that just sends a message when it starts, then does
% nothing.

start(ConfigFile) ->
	{ok, Config} = file:consult(ConfigFile),
	Centers = [ {C, E} || {center, C, E} <- Config],
	lists:foreach(fun({C, E}) ->
		Message = {data, desc, node(), E, false},
		rpc:call(C, center, notify, [Message])
	end, Centers),
	register(sensor, spawn(fun() -> loop() end)).

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
