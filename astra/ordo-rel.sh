#!/usr/bin/env bash

base=c4c
if [ ! -z "$1" ]
then
    base=$1
fi

ordo=/c/Tools/ordo-1.2.6-win/ordo-win64.exe

DATE=$(date +%Y%m%d)
H2H=head2head-$DATE.txt
RES=ordorate-$DATE.txt

$ordo -M -Q -s 300 -n 3 -D -a 0 -A $base -W -p results.pgn -j $H2H -o $RES
