#!/usr/bin/env bash

ahosts=$(./cli-all-hosts.sh R)
versions=$(cat versions.txt)

elist=$*

for m in $ahosts
do
	echo Server $m
	ssh $m '\
		cd ~/Engines/inactive;\
		for e in '$elist'; \
		do \
			for v in '$versions'; \
			do \
				eng=Barbarossa-$v-$e; \
				if [ -e $eng ]; \
				then \
					echo $eng; \
					mv $eng ..; \
				fi; \
			done; \
		done;'
done
