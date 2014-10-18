#!/usr/bin/env bash

what=$1

grep -v "^#" active_hosts.txt | while read a host
do
	case $a in
	*${what}*) echo $host;;
	*);;
	esac
done
