-module(sensor).
-export([start/1, stop/0, ping/0, query_data/1]).

% helloworld sensor that just sends a message when it starts, and it
% always states that 'desc' is 'data' when queried.

start(ConfigFile) ->
	{ok, Config} = file:consult(ConfigFile),
	Centers = [ {C, E} || {center, C, E} <- Config],
	lists:foreach(fun({C, E}) ->
		rpc:call(C, center, reg, [node(), E]),
		Message = {data, desc, node(), E, false},
		rpc:call(C, center, notify, [Message])
	end, Centers),
	register(sensor, spawn(fun() -> loop() end)).

stop() -> sensor ! stop.

ping() -> rpc({ping}).

query_data(Message) -> rpc({query_data, Message}).

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
		{From, {query_data, {_Data, _Desc, Fro, _To, _Recv}}} ->
			rpc:call(Fro, terminal, notify, [{data, desc, node(), Fro, false}]),
			From ! {sensor, ok},
			loop();
		stop ->
			init:stop()
	end.
