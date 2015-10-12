#!/usr/bin/env bash

ahosts=$(./cli-all-hosts.sh R)

elist=$*

for m in $ahosts
do
	echo Server $m
	ssh $m 'cd ~/Tour; for e in '$elist';do echo $e;done >> .inactive.txt'
done
