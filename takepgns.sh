#!/bin/bash

host=$1
if [ -z "$host" ]
then
	host=coreto
fi

dir=get.$$
mkdir $dir
cd $dir

echo Take the tar file:
ssh $host bin/givepgns.sh | tar xvfj -

if [ $? -eq 0 ]
then

	echo After tar:
	ls -l

	echo Move pgsn files:
	for d in *
	do
		mv $d/$d.pgn ..
		echo $d.pgn
	done

	cd ..
	rm -r $dir

	echo Add to results.pgn:
	for f in TEST*.pgn
	do
		echo $f
		awk '$1=="[White" || $1=="[Black" {print $0}
		$1=="[Result" {print $0;print $2;print ""}' $f \
			| sed -e 's/^"\(.*\)".*/\1/' >> results.pgn
	done

	echo Gzip and backup:
	gzip TEST*.pgn
	mv TEST*.pgn.gz pgns

	echo Done
else
	echo Tar ended with error, abort
	exit 1
fi
