#!/usr/bin/env bash

# Send the automatic engine list to all working hosts

engs=$*
echo These engines will be sent: $engs

for host in $(cat active_hosts.txt)
do
	echo Host $host
	echo $engs | ssh $host 'cd /home/nicu/Tour; cat > .automatic.new; mv .automatic.new .automatic'
done
