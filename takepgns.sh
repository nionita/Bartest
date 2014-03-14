#!/bin/bash

host=$1
dir=get.$$

mkdir $dir
cd $dir

ssh $host bin/givepgns.sh | tar xfj -

for d in *
do
	mv $d/$d.pgn ..
	echo $d.pgn
done

cd ..
rm -r $dir

for f in TEST*.pgn
do
	echo $f
	awk '$1=="[White" || $1=="[Black" {print $0}
	     $1=="[Result" {print $0;print $2;print ""}' $f \
		| sed -e 's/^"\(.*\)".*/\1/' >> results.pgn
done
gzip TEST*.pgn
mv TEST*.pgn.gz pgns
