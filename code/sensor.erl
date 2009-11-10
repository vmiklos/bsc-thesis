-module(sensor).
-export([start/1, stop/0, ping/0, query_data/1, control/1]).

% helloworld sensor that just sends a message when it starts, and it
% always sends 'desc' from the process dict when queried.

start(ConfigFile) ->
	Data = data,
	{ok, Config} = file:consult(ConfigFile),
	Centers = [ {C, E} || {center, C, E} <- Config],
	lists:foreach(fun({C, E}) ->
		rpc:call(C, center, reg, [node(), E])
	end, Centers),
	register(sensor, spawn(fun() -> put(desc, Data), loop() end)),
	io:format("Adja meg a mert adatot 'tipus szam' formaban, majd usson ENTER-t!\n"),
	io:format("A testhomerseklet merese utan peldaul 'h 38.2'\n"),
	read_stdin(Centers).

stop() -> sensor ! stop.

ping() -> rpc({ping}).

query_data(Message) -> rpc({query_data, Message}).

control(Message) -> rpc({control, Message}).

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
			rpc:call(Fro, terminal, notify, [{get(desc), desc, node(), Fro, false}]),
			From ! {sensor, ok},
			loop();
		{From, {control, {Data, _Desc, _Fro, _To, _Recv}}} ->
			put(desc, Data),
			From ! {sensor, ok},
			loop();
		stop ->
			init:stop()
	end.

read_stdin(Centers) ->
	L = io:get_line("> "),
	[A|[B]] = re:split(L, " "),
	Desc = list_to_atom(binary_to_list(A)),
	{Data, _} = string:to_float(binary_to_list(B)),
	lists:foreach(fun({C, E}) ->
		Message = {Data, Desc, node(), E, false},
		rpc:call(C, center, notify, [Message])
	end, Centers),
	read_stdin(Centers).
