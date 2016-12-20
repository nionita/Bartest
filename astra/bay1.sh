#!/usr/bin/env bash

if [ -z "$1" ]
then res=results.pgn
else res=$1
fi

(
echo readpgn $res
echo elo
echo mm
echo ratings
echo x
echo x
) | /c/Tools/bayeselo.exe
