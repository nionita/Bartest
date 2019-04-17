#!/usr/bin/env bash

ahosts=$(./cli-all-hosts.sh R)
versions=$(cat versions.txt)

for m in $ahosts
do
	echo Server $m
	ssh $m '\
		cd ~/Engines;\
		for e in $(cat ~/Tour/.inactive.txt);\
		do \
			for v in '$versions'; \
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
