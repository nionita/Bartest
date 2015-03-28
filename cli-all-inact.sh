#!/usr/bin/env bash

ahosts=$(./cli-all-hosts.sh R)

elist=''
for v in 0.3.0 0.2.0 0.1.0
do
	for e in $*
	do
		eng=Barbarossa-$v-$e
		elist="$elist $eng"
	done
done

for m in $ahosts
do
	echo Server $m
	ssh $m 'cd ~/Engines; mkdir -p inactive; for e in '$elist';do if [ -x $e ]; then echo Engine $e; mv $e inactive; fi; done'
done
