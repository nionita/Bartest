#!/usr/bin/env bash

ahosts=$(./cli-all-hosts.sh R)

for m in $ahosts
do
	echo Server $m
	ssh $m '\
		cd ~/Engines;\
		for e in $(cat ~/Tour/.inactive.txt);\
		do \
			for v in 0.4.0 0.3.0 0.2.0 0.1.0; \
			do \
				eng=Barbarossa-$v-$e; \
				if [ -e $eng ]; \
				then \
					echo $eng; \
					mv $eng inactive; \
				fi; \
			done; \
		done; \
		cat /dev/null > ~/Tour/.inactive.txt'
done
