%% @doc A simple sensor, reading its data from stdin.
-module(sensor).
-export([start/1, stop/0, ping/0, query_data/1, control/1]).

%% @doc Starts the sensor.
start(ConfigFile) ->
	Data = data,
	{ok, Config} = file:consult(ConfigFile),
	Centers = [ {C, E} || {center, C, E} <- Config],
	lists:foreach(fun({C, E}) ->
		rpc:call(C, center, reg, [node(), E])
	end, Centers),
	catch unregister(sensor),
	register(sensor, spawn(fun() -> put(desc, Data), loop() end)),
	on_exit(self(),
		fun(_) ->
				io:format("A szenzor hibaval lepett ki, ujrainditas...~n"),
				start(ConfigFile)
		end),
	io:format("Adja meg a mert adatot 'tipus szam' formaban, majd usson ENTER-t!\n"),
	io:format("A testhomerseklet merese utan peldaul 'h 38.2'\n"),
	read_stdin(Centers).

%% @doc Stops the sensor.
stop() -> sensor ! stop.

%% @doc Responds with pong if the sensor server is alive.
ping() -> rpc({ping}).

%% @doc Gets data from the sensor.
query_data(Message) -> rpc({query_data, Message}).

%% @doc Sets data on the sensor.
control(Message) -> rpc({control, Message}).

%% @doc Internal RPC handler.
%% Communicates between the sensor server process and the public
%% functions.
%% @end
rpc(Q) ->
	sensor ! {self(), Q},
	receive
		{sensor, Reply} ->
			Reply
	end.

%% @doc Main server loop.
%% Waits for messages from the RPC handler and responds to them.
%% @end
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
	case Data of
		error ->
			erlang:error(badarg);
		_ ->
			lists:foreach(fun({C, E}) ->
						Message = {Data, Desc, node(), E, false},
						rpc:call(C, center, notify, [Message])
				end, Centers),
			read_stdin(Centers)
	end.

on_exit(Pid, Fun) ->
	spawn(fun() ->
				process_flag(trap_exit, true),
				link(Pid),
				receive
					{'EXIT', Pid, Why} ->
						Fun(Why)
				end
			end).
