#!/bin/bash

TOUR=/home/nicu/Tour

cd $TOUR/take

list=$(ls)
flist=""

if [ -z "$list" ]
then
	# echo "givepgns.sh: No files to give" > &2
	exit 0	# nothing to give
fi

for d in $list
do
	flist="$flist $d/$d.pgn"
	mv $d $TOUR/archive
done

cd $TOUR/archive
tar cfj - $flist
