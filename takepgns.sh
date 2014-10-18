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

if [ $? -ne 0 ]
then
	echo Tar ended with error, abort
	exit 1	# leave the dir there, for researches
fi

echo After tar:
ls -l

list=$(ls)
if [ -z "$list" ]
then
	echo No file found, exit
	cd ..
	rmdir $dir	# it is empty
	exit 0
fi

echo Move pgn files:
flist=""
for d in $list
do
	dc=$(echo $d | sed "s/localhost/$host/")
	mv $d/$d.pgn ./$dc.pgn	# move here and rename
	echo $d.pgn
	flist="$flist $dc.pgn"
	rmdir $d	# we get only the pgn, no more
done

echo Backup old results.pgn
cp ../results.pgn ../results.pgn.old

echo Add to results.pgn, gzip and backup:
mkdir ../pgns 2>/dev/null	# to be sure...
for f in $flist
do
	echo $f
	awk '/^\[White/  {lin1=$0}
		 /^\[Black/  {lin2=$0}
		 /^\[Result/ {lin3=$0; rez=$2}
		 /^\[Termination/ {
			if ($2!~"time") {print lin1;print lin2;print lin3;print rez;print ""}
			}' $f | sed -e 's/^"\(.*\)".*/\1/' >> ../results.pgn
	gzip $f
	mv $f.gz ../pgns
done

cd ..
rmdir $dir	# should be empty

echo Done
