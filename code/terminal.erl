-module(terminal).
-export([start/1, stop/0, ping/0, notify/1]).

% a helloworld terminal that just outputs everything it gets to stdout

start(ConfigFile) ->
	{ok, Config} = file:consult(ConfigFile),
	Centers = [ {C, E} || {center, C, E} <- Config],
	lists:foreach(fun({C, E}) -> rpc:call(C, center, subscribe, [E, node()]) end, Centers),
	register(terminal, spawn(fun() -> loop() end)).

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
			%io:format("[~p] notify(~p)~n", [node(), Message]),
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
