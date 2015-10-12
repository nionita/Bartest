#!/bin/bash

TOUR=$HOME/Tour
ENGS=$HOME/Engines

echo Rename and move in $ENGS
cd $ENGS

for f in TEST*.pgn
do
	bn=$(basename $f .pgn)
	dn=${bn}a
	fn=${bn}a.pgn
	echo $fn
	mkdir $TOUR/take/$dn
	mv $f $TOUR/take/$dn/$fn
done
