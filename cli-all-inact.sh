#!/usr/bin/env bash

ahosts=$(./cli-all-hosts.sh R)

elist=''
for e in $*
do
	eng=Barbarossa-0.3.0-$e
	elist="$elist $eng"
done

for m in $ahosts
do
	echo Server $m
	ssh $m 'cd ~/Engines; mkdir -p inactive; mv '$elist inactive
done
