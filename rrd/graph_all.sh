#!/usr/bin/env bash

pmins=$1
mins=${pmins:=120}

rrdtool graph /var/www/images/cpu-t.png \
    -t "CPU Temperatures" \
    -s "end-${mins}minutes" \
    -A -E -a PNG \
    DEF:CPU0=/home/nicu/rrd/coreto_tf:CPU0Temp:AVERAGE \
    DEF:CPU1=/home/nicu/rrd/coreto_tf:CPU1Temp:AVERAGE \
    LINE1:CPU0#CC4411:CPU0 \
    LINE1:CPU1#1144CC:CPU1

rrdtool graph /var/www/images/cpu-f.png \
    -t "CPU Fan speeds" \
    -s "end-${mins}minutes" \
    -A -E -a PNG \
    DEF:CPU0=/home/nicu/rrd/coreto_tf:CPU0Fan:AVERAGE \
    DEF:CPU1=/home/nicu/rrd/coreto_tf:CPU1Fan:AVERAGE \
    LINE1:CPU0#CC4411:CPU0 \
    LINE1:CPU1#1144CC:CPU1

rrdtool graph /var/www/images/dimm-t.png \
    -t "DIMM & Case Temperatures" \
    -s "end-${mins}minutes" \
    -A -E -a PNG \
    DEF:DIMM0=/home/nicu/rrd/coreto_tf:DIMM0Temp:AVERAGE \
    DEF:DIMM1=/home/nicu/rrd/coreto_tf:DIMM1Temp:AVERAGE \
    DEF:SRTemp=/home/nicu/rrd/coreto_tf:SRTemp:AVERAGE \
    LINE1:SRTemp#44AA44:SRT \
    LINE1:DIMM0#CC4411:DIMM0 \
    LINE1:DIMM1#1144CC:DIMM1

rrdtool graph /var/www/images/case-f.png \
    -t "Case Fan speeds" \
    -s "end-${mins}minutes" \
    -A -E -a PNG \
    DEF:CFan=/home/nicu/rrd/coreto_tf:REARFan:AVERAGE \
    LINE1:CFan#44AA44:CaseFan

rrdtool graph /var/www/images/dx-temp.png \
    -t "CPU Temperatures" \
    -s "end-${mins}minutes" \
    -A -E -a PNG \
    DEF:CPUA0Temp=/home/nicu/rrd/dx360_tf:CPUA0Temp:AVERAGE \
    DEF:CPUA1Temp=/home/nicu/rrd/dx360_tf:CPUA1Temp:AVERAGE \
    DEF:CPUB0Temp=/home/nicu/rrd/dx360_tf:CPUB0Temp:AVERAGE \
    DEF:CPUB1Temp=/home/nicu/rrd/dx360_tf:CPUB1Temp:AVERAGE \
    LINE1:CPUA0Temp#CC4411:CPU0-A \
    LINE1:CPUA1Temp#44AA44:CPU1-A \
    LINE1:CPUB0Temp#0000FF:CPU0-B \
    LINE1:CPUB1Temp#AA00FF:CPU1-B

rrdtool graph /var/www/images/dx-fans.png \
    -t "CPU Fan Speeds" \
    -s "end-${mins}minutes" \
    -A -E -a PNG \
    DEF:Fan1=/home/nicu/rrd/dx360_tf:Fan1:AVERAGE \
    DEF:Fan2=/home/nicu/rrd/dx360_tf:Fan2:AVERAGE \
    DEF:Fan3=/home/nicu/rrd/dx360_tf:Fan3:AVERAGE \
    DEF:Fan4=/home/nicu/rrd/dx360_tf:Fan4:AVERAGE \
    LINE1:Fan1#CC4411:Fan1 \
    LINE1:Fan2#44AA44:Fan2 \
    LINE1:Fan3#0000FF:Fan3 \
    LINE1:Fan4#AA00FF:Fan4

rrdtool graph /var/www/images/m3-temp.png \
    -t "CPU Temperatures" \
    -s "end-${mins}minutes" \
    -A -E -a PNG \
    DEF:CPUA0Temp=/home/nicu/rrd/m3a_tf:CPUA0Temp:AVERAGE \
    DEF:CPUA1Temp=/home/nicu/rrd/m3a_tf:CPUA1Temp:AVERAGE \
    DEF:CPUB0Temp=/home/nicu/rrd/m3b_tf:CPUB0Temp:AVERAGE \
    DEF:CPUB1Temp=/home/nicu/rrd/m3b_tf:CPUB1Temp:AVERAGE \
    LINE1:CPUA0Temp#CC4411:CPU0-A \
    LINE1:CPUA1Temp#44AA44:CPU1-A \
    LINE1:CPUB0Temp#0000FF:CPU0-B \
    LINE1:CPUB1Temp#AA00FF:CPU1-B

rrdtool graph /var/www/images/m3-fans.png \
    -t "CPU Fan Speeds" \
    -s "end-${mins}minutes" \
    -A -E -a PNG \
    DEF:Fan1=/home/nicu/rrd/m3a_tf:Fan1:AVERAGE \
    DEF:Fan2=/home/nicu/rrd/m3a_tf:Fan2:AVERAGE \
    DEF:Fan3=/home/nicu/rrd/m3a_tf:Fan3:AVERAGE \
    DEF:Fan4=/home/nicu/rrd/m3a_tf:Fan4:AVERAGE \
    LINE1:Fan1#CC4411:Fan1 \
    LINE1:Fan2#44AA44:Fan2 \
    LINE1:Fan3#0000FF:Fan3 \
    LINE1:Fan4#AA00FF:Fan4

rrdtool graph /var/www/images/env-temps.png \
    -t "Environment Temperatures" \
    -s "end-${mins}minutes" \
    -A -E -a PNG \
    DEF:FPATemp=/home/nicu/rrd/dx360_tf:FPATemp:AVERAGE \
    DEF:FPBTemp=/home/nicu/rrd/dx360_tf:FPBTemp:AVERAGE \
    DEF:AMBTemp0=/home/nicu/rrd/beta_tf:AMBTemp:AVERAGE \
    DEF:AMBTemp1=/home/nicu/rrd/x3850m2_tf:AMBTemp:AVERAGE \
    LINE1:FPATemp#CC4411:FPATemp \
    LINE1:FPBTemp#44AA44:FPBTemp \
    LINE1:AMBTemp0#0000FF:Amb-beta \
    LINE1:AMBTemp1#AA00FF:Amb-x3850m2

rrdtool graph /var/www/images/m3-env.png \
    -t "Environment Temperatures" \
    -s "end-${mins}minutes" \
    -A -E -a PNG \
    DEF:FPATemp=/home/nicu/rrd/m3a_tf:FPATemp:AVERAGE \
    DEF:FPBTemp=/home/nicu/rrd/m3b_tf:FPBTemp:AVERAGE \
    LINE1:FPATemp#CC4411:FPATemp \
    LINE1:FPBTemp#44AA44:FPBTemp

rrdtool graph /var/www/images/beta-fans.png \
    -t "CPU Fan Speeds" \
    -s "end-${mins}minutes" \
    -A -E -a PNG \
    DEF:Fan1=/home/nicu/rrd/beta_tf:Fan1:AVERAGE \
    DEF:Fan2=/home/nicu/rrd/beta_tf:Fan2:AVERAGE \
    DEF:Fan3=/home/nicu/rrd/beta_tf:Fan3:AVERAGE \
    DEF:Fan4=/home/nicu/rrd/beta_tf:Fan4:AVERAGE \
    DEF:Fan5=/home/nicu/rrd/beta_tf:Fan5:AVERAGE \
    DEF:Fan6=/home/nicu/rrd/beta_tf:Fan6:AVERAGE \
    LINE1:Fan1#CC4411:Fan1 \
    LINE1:Fan2#44AA44:Fan2 \
    LINE1:Fan3#0000FF:Fan3 \
    LINE1:Fan4#FFAA11:Fan4 \
    LINE1:Fan5#AA00FF:Fan5 \
    LINE1:Fan6#00FFFF:Fan6

rrdtool graph /var/www/images/x3850m2-fans.png \
    -t "CPU Fan Speeds" \
    -s "end-${mins}minutes" \
    -A -E -a PNG \
    DEF:Fan1=/home/nicu/rrd/x3850m2_tf:Fan1:AVERAGE \
    DEF:Fan2=/home/nicu/rrd/x3850m2_tf:Fan2:AVERAGE \
    DEF:Fan3=/home/nicu/rrd/x3850m2_tf:Fan3:AVERAGE \
    DEF:Fan4=/home/nicu/rrd/x3850m2_tf:Fan4:AVERAGE \
    DEF:Fan5=/home/nicu/rrd/x3850m2_tf:Fan5:AVERAGE \
    DEF:Fan6=/home/nicu/rrd/x3850m2_tf:Fan6:AVERAGE \
    LINE1:Fan1#CC4411:Fan1 \
    LINE1:Fan2#44AA44:Fan2 \
    LINE1:Fan3#0000FF:Fan3 \
    LINE1:Fan4#FFAA11:Fan4 \
    LINE1:Fan5#AA00FF:Fan5 \
    LINE1:Fan6#00FFFF:Fan6

rrdtool graph /var/www/images/beta-cputemps.png \
    -t "CPU Temperatures" \
    -s "end-${mins}minutes" \
    -A -E -a PNG \
    DEF:CPU1Temp=/home/nicu/rrd/beta_tf:CPU1Temp:AVERAGE \
    DEF:CPU2Temp=/home/nicu/rrd/beta_tf:CPU2Temp:AVERAGE \
    DEF:CPU3Temp=/home/nicu/rrd/beta_tf:CPU3Temp:AVERAGE \
    DEF:CPU4Temp=/home/nicu/rrd/beta_tf:CPU4Temp:AVERAGE \
    LINE1:CPU1Temp#CC4411:CPU1 \
    LINE1:CPU2Temp#44AA44:CPU2 \
    LINE1:CPU3Temp#0000FF:CPU3 \
    LINE1:CPU4Temp#AA00FF:CPU4

rrdtool graph /var/www/images/x3850m2-cputemps.png \
    -t "CPU Temperatures" \
    -s "end-${mins}minutes" \
    -A -E -a PNG \
    DEF:CPU1Temp=/home/nicu/rrd/x3850m2_tf:CPU1Temp:AVERAGE \
    DEF:CPU2Temp=/home/nicu/rrd/x3850m2_tf:CPU2Temp:AVERAGE \
    LINE1:CPU1Temp#FDB975:CPU1 \
    LINE1:CPU2Temp#ACA864:CPU2
