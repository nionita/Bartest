#!/usr/bin/env bash

base=i1811
if [ ! -z "$1" ]
then
    base=$1
fi

/c/Tools/ordo-1.2.6-win/ordo-win64.exe -J -s 10 -D -a 0 -A $base -W -p results.pgn
