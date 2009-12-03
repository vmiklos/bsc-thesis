run_erl()
{
	echo "starting node $1..."
	mkdir -p run/{log,pipe}_dir_$1
	# if the nodes are on different machines, then this is needed as well:
	# -setcookie s3cr3t -kernel inet_dist_listen_min 6000 inet_dist_listen_max 6000
	# (and open 4369&6000 tcp&udp on the firewall)
	/usr/lib/erlang/bin/run_erl -daemon run/pipe_dir_$1/ run/log_dir_$1/ "erl -name $1@clevo.local -noshell -run $2"
	sleep 1
	eval ${1}_pid=$(sed 's/.*\[\(.*\)\].*/\1/;q' run/log_dir_$1/run_erl.log)
}

grep_erl()
{
	# filter dates
	grep -v "^=====" run/log_dir_$1/erlang.log.1
}

cd ..
rm -rf run
