#!/usr/bin/env bash

for m in $(./cli-all-hosts.sh I)
do
	echo Server $m
	ssh $m sudo ipmi-sensors|grep -E "Ambient|PECI|RPM|AVG"|sed 's/          //'
	echo
done
