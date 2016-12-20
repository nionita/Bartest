#!/usr/bin/env bash

ahosts=$(./cli-all-hosts.sh E)

mkdir tempEng
cd tempEng

elist=''
for e in $*
do
	eng=Barbarossa-0.4.0-$e
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

# cleanup
cd ..
rm -r tempEng
