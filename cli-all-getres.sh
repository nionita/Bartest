#!/usr/bin/env bash

# Take all results from the working hosts

# Make a copy of the original results.pgn
cp results.pgn results.pgn.copy

# Take the results
for host in $(./cli-all-hosts.sh G)
do
	echo Taking the results from $host
	./takepgns.sh $host
done
echo All done
