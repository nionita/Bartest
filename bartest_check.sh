#!/usr/bin/env bash

function exmesg {
	echo $1
	exit 1
}

function compare {
	dir=$1
	echo
	echo Comparing $dir
	cd $dir
	flst=""
	mess="Not in repo:"
	prtd=""
	for f in *
	do
		if [ -f ../../$dir/$f ]
		then
			flst="$f $flst"
		else
			if [ -z "$prtd" ]
			then
				echo $mess
				prtd=j
			fi
			echo $f
		fi
	done
	cd ../../$dir
	mess="Not in orig:"
	prtd=""
	for f in *
	do
		if [ ! -f ../$TMP/$dir/$f ]
		then
			if [ -z "$prtd" ]
			then
				echo $mess
				prtd=j
			fi
			echo $f
		fi
	done
	cd ../$TMP
	for f in $flst
	do
		diff $dir/$f ../$dir > diff-$f
		if [ -s diff-$f ]
		then
			echo Difference in $f
			ls -l diff-$f
		else
			rm diff-$f
		fi
	done
}

TMP=bartest.$$
mkdir $TMP || exmesg "Can't create $TMP"
cd $TMP

echo Take the files from astra:
mkdir astra
for f in /c/astra/*.sh
do
	echo $f
	cp $f astra
done
compare astra

echo
echo Take files from coreto/bin:
ssh coreto 'tar cf - bin' | tar xvf -
compare bin

echo
echo Take files from coreto/rrd:
ssh coreto 'tar cf - rrd/*.sh rrd/*.pl' | tar xvf -
compare rrd
