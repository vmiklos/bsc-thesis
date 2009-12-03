%% @doc A center is a Node which accepts and forwards messages.
-module(center).
-export([start/0, stop/0, ping/0, reg/2, lookup/1, subscribe/2, notify/1]).

%% @doc Starts the center.
%% Call this function to start the center, so that it can be
%% controlled remotely.
%% @end
start() -> register(center, spawn(fun() -> put(sensors, []), put(subscriptions, []), loop() end)).

%% @doc Stops the center and terminates the current Erlang node.
stop() -> center ! stop.

%% @doc Responds with pong to show that it's alive.
ping() -> rpc({ping}).

%% @doc Register a new sensor in the center.
reg(Address, Name) -> rpc({reg, Address, Name}).

%% @doc Get the address of a sensor based on its name.
lookup(Name) -> rpc({lookup, Name}).

%% @doc Subscribe a terminal to a given event of the center.
subscribe(Name, Address) -> rpc({subscribe, Name, Address}).

%% @doc Signal about a new measure data is available for terminals.
notify(Message) -> rpc({notify, Message}).

%% @doc Internal RPC handler.
%% Communicates between the center server process and the public
%% functions.
%% @end
rpc(Q) ->
	center ! {self(), Q},
	receive
		{center, Reply} ->
			Reply
	end.

%% @doc Main server loop.
%% Waits for messages from the RPC handler and responds to them.
%% @end
loop() ->
	receive
		{From, {ping}} ->
			From ! {center, pong},
			loop();
		{From, {reg, Address, Name}} ->
			io:format("[~p] reg(~p,~p)~n", [node(), Address, Name]),
			Sensors = get(sensors),
			put(sensors, [{Address, Name}| Sensors]),
			From ! {center, ok},
			loop();
		{From, {lookup, Name}} ->
			io:format("[~p] lookup(~p)~n", [node(), Name]),
			Sensors = get(sensors),
			A = [A || {A, N} <- Sensors, N =:= Name],
			From ! {center, {ok,A}},
			loop();
		{From, {subscribe, Name, Address}} ->
			io:format("[~p] subscribe(~p,~p)~n", [node(), Name, Address]),
			Subscriptions = get(subscriptions),
			case lists:member({Name, Address}, Subscriptions) of
				false ->
					put(subscriptions, [{Name, Address}| Subscriptions]);
				_ ->
					ok
			end,
			From ! {center, ok},
			loop();
		{From, {notify, Message}} ->
			io:format("[~p] notify(~p)~n", [node(), Message]),
			{Data, Description, Fro, To, _Receiver} = Message,
			lists:foreach(fun(I) -> rpc:call(I, terminal, notify, [{Data, Description, Fro, To, node()}]) end,
				[A || {N, A} <- get(subscriptions), N =:= To]),
			From ! {center, ok},
			loop();
		stop ->
			init:stop()
	end.
