rrdtool create m3b_tf -s 60 \
  DS:CPUB0Temp:GAUGE:180:0:100 \
  DS:CPUB1Temp:GAUGE:180:0:100 \
  DS:FPBTemp:GAUGE:180:0:50 \
  RRA:AVERAGE:0.7:1:1440 \
  RRA:MAX:0.8:60:72 \
  RRA:MIN:0.8:60:72
