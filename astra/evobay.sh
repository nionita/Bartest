#!/bin/bash

cdir=evolve-$$
mkdir $cdir
cd $cdir

sdir=$1
if [ ! -z "$2" ]
then
	host=$1
	sdir=$2
	scp $host:'~/Learn/'$sdir'/Games/*-res.txt' . || exit 1
	sdir=.
fi

res=results-$$.pgn

for f in $sdir/*-res.txt
do
	echo $f
	awk '/^[^ ]/ {
			wi = $4;
			lo = $6;
			dr = $8;
			rwi="1-0"; rlo="0-1"; rdr="1/2-1/2";
			if (wi>0)
				for (i=1;i<=wi;i++) {
					print "[White \"" $1 "\"]";
					print "[Black \"" $3 "\"]";
					print "[Result \"" rwi "\"]";
					print rwi; print ""
				}
			if (dr>0)
				for (i=1;i<=dr;i++) {
					print "[White \"" $1 "\"]";
					print "[Black \"" $3 "\"]";
					print "[Result \"" rdr "\"]";
					print rdr; print ""
				}
			if (lo>0)
				for (i=1;i<=lo;i++) {
					print "[White \"" $1 "\"]";
					print "[Black \"" $3 "\"]";
					print "[Result \"" rlo "\"]";
					print rlo; print ""
				}
			}' $f >> $res
done

cd ..
./bay1.sh $cdir/$res