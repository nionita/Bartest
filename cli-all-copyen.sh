#!/usr/bin/env bash

ahosts=$(./cli-all-hosts.sh E)
cd /j/Projects/MakeEc2/Engines

elist=''
for e in $*
do
	eng=Barbarossa-0.2.0-$e
	elist="$elist $eng"
	scp 'coreto:~/Engines/'$eng .
done

for m in $ahosts
do
	echo Server $m
	for e in $elist
	do
		scp $e $m':~/Engines'
	done
	ssh $m 'cd ~/Engines; chmod +x '$elist
done
