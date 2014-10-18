#!/usr/bin/env bash

for m in $(./cli-all-hosts.sh R)
do
	echo "Server $m"
	ssh $m bin/running.sh
	echo
done
