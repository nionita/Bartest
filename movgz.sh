#!/bin/bash

CUTE=$HOME/cutechess-cli
ENGS=$HOME/Engines
PGNS=$CUTE/pgns

echo Rename and move in $ENGS
cd $ENGS
for f in TEST*.pgn
do
	bn=$(basename $f .pgn)
	n=${bn}a.pgn
	echo $n
	mv $f $CUTE/$n
done

echo Move old gz to $PGNS
cd $CUTE
ls -l TEST*.gz
mv TEST*.gz $PGNS

echo Gzip pgns in $CUTE
ls -l TEST*.pgn
gzip TEST*.pgn

echo Done
ls -l TEST*.pgn.gz
