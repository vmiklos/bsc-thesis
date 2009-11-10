%% @doc A simple terminal that outputs alert and info messages.
-module(terminal).
-export([start/1, stop/0, ping/0, notify/1]).

%% @doc Starts the center.
%% Call this function to start the terminal, so that it can be
%% notified remotely.
%% @end
start(ConfigFile) ->
	{ok, Config} = file:consult(ConfigFile),
	Centers = [ {C, E} || {center, C, E} <- Config],
	lists:foreach(fun({C, E}) -> rpc:call(C, center, subscribe, [E, node()]) end, Centers),
	register(terminal, spawn(fun() -> loop() end)).

%% @doc Stops the terminal and terminates the current Erlang node.
stop() -> terminal ! stop.

%% @doc Responds with pong to show that it's alive.
ping() -> rpc({ping}).

%% @doc Signal about a new measure data is available from a center.
notify(Message) -> rpc({notify, Message}).

%% @doc Internal RPC handler.
%% Communicates between the terminal server process and the public
%% functions.
%% @end
rpc(Q) ->
	terminal ! {self(), Q},
	receive
		{terminal, Reply} ->
			Reply
	end.

%% @doc Main server loop.
%% Waits for messages from the RPC handler and responds to them.
%% @end
loop() ->
	receive
		{From, {ping}} ->
			From ! {terminal, pong},
			loop();
		{From, {notify, Message}} ->
			{Data, Desc, _Fro, _To, _Recv} = Message,
			case Desc of
				h ->
					case Data > 37.2 of
						true ->
							io:fwrite([7]), % bell
							io:format("Figyelem, a homerseklet ~p homerseklet tobb a megengedettnel!~n", [Data]);
						_ ->
							io:format("Uj homerseklet adat: ~p.~n", [Data])
						end;
				_ ->
					io:format("A '~p' uj erteke: ~p.~n", [Desc, Data])
			end,
			From ! {terminal, ok},
			loop();
		stop ->
			init:stop()
	end.
