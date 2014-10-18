#!/bin/bash
awk '/^\[White/  {lin1=$0}
     /^\[Black/  {lin2=$0}
     /^\[Result/ {lin3=$0; rez=$2}
	 /^\[Termination/ {
		if ($2!~"time") {print lin1;print lin2;print lin3;print rez;print ""}
		}' $1 | sed -e 's/^"\(.*\)".*/\1/'
