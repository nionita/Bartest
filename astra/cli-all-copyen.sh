#!/usr/bin/env bash

ahosts=$(./cli-all-hosts.sh E)
versions=$(cat versions.txt)

mkdir tempEng
cd tempEng

ssh coreto 'ls Engines/Barbarossa-*' | sed 's:Engines/::' > existing.txt

elist=''
for e in $*
do
	for ver in $versions
	do
		eng=Barbarossa-${ver}-$e
		if grep $eng existing.txt > /dev/null 2>&1
		then
			elist="$elist $eng"
		fi
	done
done

echo Found: $elist

for e in $elist
do
	scp 'coreto:~/Engines/'$e .
done

for m in $ahosts
do
	echo Server $m
	for e in $elist
	do
		scp $e $m':~/Engines'
	done
	ssh $m 'cd ~/Engines; chmod +x '$elist
done

# cleanup
cd ..
rm -r tempEng
