rrdtool create coreto_tf -s 60 \
  DS:CPU0Temp:GAUGE:180:0:100 \
  DS:CPU1Temp:GAUGE:180:0:100 \
  DS:DIMM0Temp:GAUGE:180:0:100 \
  DS:DIMM1Temp:GAUGE:180:0:100 \
  DS:SRTemp:GAUGE:180:0:100 \
  DS:CPU0Fan:GAUGE:180:0:10000 \
  DS:CPU1Fan:GAUGE:180:0:10000 \
  DS:REARFan:GAUGE:180:0:2000 \
  RRA:AVERAGE:0.7:1:1440 \
  RRA:MAX:0.8:60:72 \
  RRA:MIN:0.8:60:72
