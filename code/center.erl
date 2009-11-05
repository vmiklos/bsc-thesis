-module(center).
-export([start/0, stop/0, ping/0, reg/2, lookup/1, subscribe/2, notify/1]).

start() -> register(center, spawn(fun() -> put(sensors, []), put(subscriptions, []), loop() end)).

stop() -> center ! stop.

ping() -> rpc({ping}).

reg(Address, Name) -> rpc({reg, Address, Name}).

lookup(Name) -> rpc({lookup, Name}).

subscribe(Name, Address) -> rpc({subscribe, Name, Address}).

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
		{From, {reg, Address, Name}} ->
			Sensors = get(sensors),
			put(sensors, [{Address, Name}| Sensors]),
			From ! {center, ok},
			loop();
		{From, {lookup, Name}} ->
			Sensors = get(sensors),
			A = [A || {A, N} <- Sensors, N =:= Name],
			From ! {center, {ok,A}},
			loop();
		{From, {subscribe, Name, Address}} ->
			error_logger:info_msg("~p subscribed to ~p~n", [Address, Name]),
			Subscriptions = get(subscriptions),
			put(subscriptions, [{Name, Address}| Subscriptions]),
			From ! {center, ok},
			loop();
		{From, {notify, Message}} ->
			{Data, Description, Fro, To, _Receiver} = Message,
			lists:foreach(fun(I) -> rpc:call(I, terminal, notify, [{Data, Description, Fro, To, node()}]) end,
				[A || {N, A} <- get(subscriptions), N =:= To]),
			From ! {center, ok},
			loop();
		stop ->
			init:stop()
	end.
