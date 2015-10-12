#!/bin/bash

loc=/tmp/loc.$$
rem=/tmp/rem.$$

user=nicu

cd Engines
ls > $loc

for h in $*
do
	ssh $user@$h ls Engines > $rem

	echo Copy to $h:
	scp $(comm -23 $loc $rem) $user@$h:'~/Engines'

	el=""
	for f in $(comm -13 $loc $rem)
	do
		el="$el $f"
	done
	echo Inactivate in $h: $el
	ssh $user@$h 'cd Engines; mv '$el' inactive'
done

rm $loc
rm $rem
