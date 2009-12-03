#explicit lekerdezes ugy megy, hogy:
#
#erl -name center0@clevo.local, center:start()
#erl -name terminal0@clevo.local, terminal:start('t/t0011-terminal0.conf')
#erl -name sensor0@clevo.local, sensor:start('t/t0011-sensor0.conf')
#
#majd temrinal0ban:
#
#{ok,[S]} = rpc:call(center0@clevo.local, center, lookup, [event]).
#rpc:call(S, sensor, query_data, [{false, desc, node(), S, false}]).
#ill.
#rpc:call(S, sensor, control, [{data2, desc, node(), S, false}]).
