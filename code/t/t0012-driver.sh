#a driver kerdezes ugy megy, hogy:
#
#erl -name center0@clevo.local, center:start()
#erl -name terminal0@clevo.local, terminal:start('t/t0012-terminal0.conf')
#erl -name sensor0@clevo.local, sensor:start('t/t0012-sensor0.conf')
#
#majd temrinal0ban:
#
#{ok,[S]} = rpc:call(center0@clevo.local, center, lookup, [event]).
#M = driver:translate(S, 0).
#rpc:call(S, sensor, M, []).
#
# ha a driver elerheto, ez automatikusan leforditja es igy direkben,
# szinkron el tudja erni a terminal a szenzor adott adatat
