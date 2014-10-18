#!/usr/bin/env bash

MYFILE=/home/nicu/myhost
TOUR=/home/nicu/Tour
ENGDIR=/home/nicu/Engines
CRONLOG=/home/nicu/log/jiffy.log

# Check if we already initialised by checking the host name
# against the name written under home:
me=$(uname -a | cut -d" " -f2)

# We need the identity file
if [ ! -f $MYFILE ]
then
	echo Init: file $MYFILE does not exist
	exit 1
fi

# if find archive aborted running take -name 'TEST-'$me'-*.log' > /dev/null
if [ $me == $(cat $MYFILE) ]
then
	echo We are already initialized
	exit 0
fi
# We are on a new server: initialise
echo $(date) : We are on a new server, initialise

# Our identity:
echo -n $me > $MYFILE

# delete all foreign tournament files
cd $TOUR
rm -r archive/*
rm -r abort/*
rm -r running/*
rm -r take/*

# see how many processors we have:
cpus=$(grep processor /proc/cpuinfo | wc -l)
if [[ $cpus -le 3 ]]
then
	touch .restricted
else
	rm -f .restricted
fi

# delete the stop file, if any
rm .stop

# delete the collected errors
rm $ENGDIR/Barbarossa_collected_errors.txt

# rename the cron log (these messages will be in the old one!)
mv $CRONLOG $CRONLOG.old
exit 0
