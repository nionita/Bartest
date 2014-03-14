#!/bin/bash

TOUR=/home/nicu/Tour

cd $TOUR/take

list=*
flist=""

for d in $list
do
	flist="$flist $d/$d.pgn"
	cd $TOUR/take
	mv $d $TOUR/archive
done

cd $TOUR/archive
tar cfj - $flist
