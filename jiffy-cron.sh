#!/usr/bin/env bash

# This script is started by cron on a jiffy box
# If some conditions are met, then it will start a tournament
# with pre-configured engines

ENGLIST=/home/nicu/Tour/.automatic
INIT=/home/nicu/bin/jiffy-init.sh
TOURO=/home/nicu/bin/touro.sh

echo

# Check first if some cutechess is running:
if ps -u nicu -o command | cut -d' ' -f1 | grep cutechess
then
	echo $(date) - There is already a running tournament
	exit 0
fi

# Start the initialisation script
$INIT || exit 1

# Check if we wanted to stop the automatic run:
if [ -f $TOUR/.stop ]
then
	echo $(date) - The stop file is present, exit
	exit 0
fi

# Take the engines to run and start the tournament
engines=$(cat $ENGLIST)
echo $(date) - Start with the engines: $engines
$TOURO -T 2 -t 2 $engines
