#!/bin/bash

TOUR=~/Tour

cd $TOUR
tth=0
date
for f in running/*/*.log
do
	thr=$(head -6 $f | grep threads: | awk '{print $4}')
	lal=$(tail -1 $f | grep Started)
	if [ ! -z "$thr" -a ! -z "$lal" ]
	then
		sta=$(echo $lal | awk '{print $3}')
		tot=$(echo $lal | awk '{print $5}')
		rem=$((($tot - $sta) * 4 / $thr + 4))
		tou=$(basename $f .log)
		echo $tou finish in $rem minutes - s/t/t = $sta/$tot/$thr
		tth=$(($tth + $thr))
	fi
done
echo $tth threads running
