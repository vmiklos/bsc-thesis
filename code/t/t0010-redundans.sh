. ./lib.sh

cat >expected <<EOF
['center0@clevo.local'] subscribe('terminal0@clevo.local',event)
['center0@clevo.local'] subscribe('terminal1@clevo.local',event)
['center0@clevo.local'] notify({data,desc,'sensor0@clevo.local',event,false})
['center0@clevo.local'] notify({data,desc,'sensor1@clevo.local',event,false})

['center1@clevo.local'] subscribe('terminal0@clevo.local',event)
['center1@clevo.local'] subscribe('terminal1@clevo.local',event)
['center1@clevo.local'] notify({data,desc,'sensor0@clevo.local',event,false})
['center1@clevo.local'] notify({data,desc,'sensor1@clevo.local',event,false})

['terminal0@clevo.local'] notify({data,desc,'sensor0@clevo.local',event,
                                     'center0@clevo.local'})
['terminal0@clevo.local'] notify({data,desc,'sensor0@clevo.local',event,
                                     'center1@clevo.local'})
['terminal0@clevo.local'] notify({data,desc,'sensor1@clevo.local',event,
                                     'center0@clevo.local'})
['terminal0@clevo.local'] notify({data,desc,'sensor1@clevo.local',event,
                                     'center1@clevo.local'})

['terminal1@clevo.local'] notify({data,desc,'sensor0@clevo.local',event,
                                     'center0@clevo.local'})
['terminal1@clevo.local'] notify({data,desc,'sensor0@clevo.local',event,
                                     'center1@clevo.local'})
['terminal1@clevo.local'] notify({data,desc,'sensor1@clevo.local',event,
                                     'center0@clevo.local'})
['terminal1@clevo.local'] notify({data,desc,'sensor1@clevo.local',event,
                                     'center1@clevo.local'})
EOF

run_erl center0 "center start"
run_erl center1 "center start"
run_erl terminal0 "terminal start t/t0010-terminal0.conf"
run_erl terminal1 "terminal start t/t0010-terminal1.conf"
run_erl sensor0 "sensor start t/t0010-sensor0.conf"
run_erl sensor1 "sensor start t/t0010-sensor1.conf"

(grep_erl center0
grep_erl center1
grep_erl terminal0
grep_erl terminal1
grep_erl sensor0
grep_erl sensor1) > result
kill $center0_pid $center1_pid $terminal0_pid $terminal1_pid $sensor0_pid $sensor1_pid
diff -uwB expected result
exit $?
