#!/usr/bin/env bash

pmins=$1
mins=${pmins:=120}

rrdtool graph /var/www/images/cpu-t.png \
    -t "CPU Temperatures" \
    -s "end-${mins}minutes" \
    -a PNG \
    DEF:CPU0=/home/nicu/rrd/coreto_tf:CPU0Temp:AVERAGE \
    DEF:CPU1=/home/nicu/rrd/coreto_tf:CPU1Temp:AVERAGE \
    LINE2:CPU0#CC4411 \
    LINE2:CPU1#1144CC

rrdtool graph /var/www/images/cpu-f.png \
    -t "CPU Fan speeds" \
    -s "end-${mins}minutes" \
    -a PNG \
    DEF:CPU0=/home/nicu/rrd/coreto_tf:CPU0Fan:AVERAGE \
    DEF:CPU1=/home/nicu/rrd/coreto_tf:CPU1Fan:AVERAGE \
    LINE2:CPU0#CC4411 \
    LINE2:CPU1#1144CC

rrdtool graph /var/www/images/dimm-t.png \
    -t "DIMM & Case Temperatures" \
    -s "end-${mins}minutes" \
    -a PNG \
    DEF:DIMM0=/home/nicu/rrd/coreto_tf:DIMM0Temp:AVERAGE \
    DEF:DIMM1=/home/nicu/rrd/coreto_tf:DIMM1Temp:AVERAGE \
    DEF:SRTemp=/home/nicu/rrd/coreto_tf:SRTemp:AVERAGE \
    LINE2:SRTemp#44AA44 \
    LINE2:DIMM0#CC4411 \
    LINE2:DIMM1#1144CC

rrdtool graph /var/www/images/case-f.png \
    -t "Case Fan speeds" \
    -s "end-${mins}minutes" \
    -a PNG \
    DEF:CFan=/home/nicu/rrd/coreto_tf:REARFan:AVERAGE \
    LINE2:CFan#44AA44

rrdtool graph /var/www/images/dx-temp.png \
    -t "CPU Temperatures" \
    -s "end-${mins}minutes" \
    -a PNG \
    DEF:CPUA0Temp=/home/nicu/rrd/dx360_tf:CPUA0Temp:AVERAGE \
    DEF:CPUA1Temp=/home/nicu/rrd/dx360_tf:CPUA1Temp:AVERAGE \
    DEF:CPUB0Temp=/home/nicu/rrd/dx360_tf:CPUB0Temp:AVERAGE \
    DEF:CPUB1Temp=/home/nicu/rrd/dx360_tf:CPUB1Temp:AVERAGE \
    LINE2:CPUA0Temp#770000 \
    LINE2:CPUA1Temp#990000 \
    LINE2:CPUB0Temp#772200 \
    LINE2:CPUB1Temp#992200

rrdtool graph /var/www/images/dx-fans.png \
    -t "CPU Fan Speeds" \
    -s "end-${mins}minutes" \
    -a PNG \
    DEF:Fan1=/home/nicu/rrd/dx360_tf:Fan1:AVERAGE \
    DEF:Fan2=/home/nicu/rrd/dx360_tf:Fan2:AVERAGE \
    DEF:Fan3=/home/nicu/rrd/dx360_tf:Fan3:AVERAGE \
    DEF:Fan4=/home/nicu/rrd/dx360_tf:Fan4:AVERAGE \
    LINE2:Fan1#123456 \
    LINE2:Fan2#345678 \
    LINE2:Fan3#56789A \
    LINE2:Fan4#789ABC

rrdtool graph /var/www/images/env-temps.png \
    -t "Environment Temperatures" \
    -s "end-${mins}minutes" \
    -a PNG \
    DEF:FPATemp=/home/nicu/rrd/dx360_tf:FPATemp:AVERAGE \
    DEF:FPBTemp=/home/nicu/rrd/dx360_tf:FPBTemp:AVERAGE \
    DEF:AMBTemp0=/home/nicu/rrd/beta_tf:AMBTemp:AVERAGE \
    DEF:AMBTemp1=/home/nicu/rrd/x3850m2_tf:AMBTemp:AVERAGE \
    LINE2:FPATemp#44AA44 \
    LINE2:FPBTemp#CC4411 \
    LINE2:AMBTemp0#00BB11 \
    LINE2:AMBTemp1#00BB11

rrdtool graph /var/www/images/ibm24-fans.png \
    -t "CPU Fan Speeds" \
    -s "end-${mins}minutes" \
    -a PNG \
    DEF:Fan1=/home/nicu/rrd/ibm24_tf:Fan1:AVERAGE \
    DEF:Fan2=/home/nicu/rrd/ibm24_tf:Fan2:AVERAGE \
    DEF:Fan3=/home/nicu/rrd/ibm24_tf:Fan3:AVERAGE \
    DEF:Fan4=/home/nicu/rrd/ibm24_tf:Fan4:AVERAGE \
    DEF:Fan5=/home/nicu/rrd/ibm24_tf:Fan5:AVERAGE \
    DEF:Fan6=/home/nicu/rrd/ibm24_tf:Fan6:AVERAGE \
    LINE2:Fan1#123456 \
    LINE2:Fan2#234567 \
    LINE2:Fan3#345678 \
    LINE2:Fan4#456789 \
    LINE2:Fan5#56789A \
    LINE2:Fan6#6789AB

rrdtool graph /var/www/images/beta-fans.png \
    -t "CPU Fan Speeds" \
    -s "end-${mins}minutes" \
    -a PNG \
    DEF:Fan1=/home/nicu/rrd/beta_tf:Fan1:AVERAGE \
    DEF:Fan2=/home/nicu/rrd/beta_tf:Fan2:AVERAGE \
    DEF:Fan3=/home/nicu/rrd/beta_tf:Fan3:AVERAGE \
    DEF:Fan4=/home/nicu/rrd/beta_tf:Fan4:AVERAGE \
    DEF:Fan5=/home/nicu/rrd/beta_tf:Fan5:AVERAGE \
    DEF:Fan6=/home/nicu/rrd/beta_tf:Fan6:AVERAGE \
    LINE2:Fan1#123456 \
    LINE2:Fan2#234567 \
    LINE2:Fan3#345678 \
    LINE2:Fan4#456789 \
    LINE2:Fan5#56789A \
    LINE2:Fan6#6789AB

rrdtool graph /var/www/images/x3850m2-fans.png \
    -t "CPU Fan Speeds" \
    -s "end-${mins}minutes" \
    -a PNG \
    DEF:Fan1=/home/nicu/rrd/x3850m2_tf:Fan1:AVERAGE \
    DEF:Fan2=/home/nicu/rrd/x3850m2_tf:Fan2:AVERAGE \
    DEF:Fan3=/home/nicu/rrd/x3850m2_tf:Fan3:AVERAGE \
    DEF:Fan4=/home/nicu/rrd/x3850m2_tf:Fan4:AVERAGE \
    DEF:Fan5=/home/nicu/rrd/x3850m2_tf:Fan5:AVERAGE \
    DEF:Fan6=/home/nicu/rrd/x3850m2_tf:Fan6:AVERAGE \
    LINE2:Fan1#123456 \
    LINE2:Fan2#234567 \
    LINE2:Fan3#345678 \
    LINE2:Fan4#456789 \
    LINE2:Fan5#56789A \
    LINE2:Fan6#6789AB

rrdtool graph /var/www/images/ibm24-cputemps.png \
    -t "CPU Temperatures" \
    -s "end-${mins}minutes" \
    -a PNG \
    DEF:CPU1Temp=/home/nicu/rrd/ibm24_tf:CPU1Temp:AVERAGE \
    DEF:CPU2Temp=/home/nicu/rrd/ibm24_tf:CPU2Temp:AVERAGE \
    DEF:CPU3Temp=/home/nicu/rrd/ibm24_tf:CPU3Temp:AVERAGE \
    DEF:CPU4Temp=/home/nicu/rrd/ibm24_tf:CPU4Temp:AVERAGE \
    LINE2:CPU1Temp#FDB975 \
    LINE2:CPU2Temp#ECA864 \
    LINE2:CPU3Temp#DB9753 \
    LINE2:CPU4Temp#CA8642

rrdtool graph /var/www/images/beta-cputemps.png \
    -t "CPU Temperatures" \
    -s "end-${mins}minutes" \
    -a PNG \
    DEF:CPU1Temp=/home/nicu/rrd/beta_tf:CPU1Temp:AVERAGE \
    DEF:CPU2Temp=/home/nicu/rrd/beta_tf:CPU2Temp:AVERAGE \
    DEF:CPU3Temp=/home/nicu/rrd/beta_tf:CPU3Temp:AVERAGE \
    DEF:CPU4Temp=/home/nicu/rrd/beta_tf:CPU4Temp:AVERAGE \
    LINE2:CPU1Temp#FDB975 \
    LINE2:CPU2Temp#ECA864 \
    LINE2:CPU3Temp#DB9753 \
    LINE2:CPU4Temp#CA8642

rrdtool graph /var/www/images/x3850m2-cputemps.png \
    -t "CPU Temperatures" \
    -s "end-${mins}minutes" \
    -a PNG \
    DEF:CPU1Temp=/home/nicu/rrd/x3850m2_tf:CPU1Temp:AVERAGE \
    DEF:CPU2Temp=/home/nicu/rrd/x3850m2_tf:CPU2Temp:AVERAGE \
    LINE2:CPU1Temp#FDB975 \
    LINE2:CPU2Temp#ECA864
