#a riasztas kerdezes ugy megy, hogy:
#
#erl -name center0@clevo.local, center:start()
#erl -noshell -name terminal0@clevo.local -run terminal start t/t0013-terminal0.conf
#erl -name sensor0@clevo.local, sensor:start('t/t0013-sensor0.conf')
#
#majd sensor0ban kell megadni adatokat, ha tul magas a homerseklet akkor a terminal sipol
