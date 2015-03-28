#!/usr/bin/env bash

ahosts=$(./cli-all-hosts.sh R)

ratf=$(ls -t rating-201*.txt | head -1)

echo -n "Do you want to send rating file $ratf? [y/N]: "

read ans;

case "$ans" in
y) ;;
*) exit 0;;
esac

for m in $ahosts
do
	echo Server $m
	scp $ratf $m:'~/Tour/.rating.next'
	ssh $m 'cd ~/Tour; mv .rating.next .rating.txt'
done
