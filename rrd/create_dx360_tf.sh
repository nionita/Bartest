rrdtool create dx360_tf -s 60 \
  DS:CPUA0Temp:GAUGE:180:0:100 \
  DS:CPUA1Temp:GAUGE:180:0:100 \
  DS:CPUB0Temp:GAUGE:180:0:100 \
  DS:CPUB1Temp:GAUGE:180:0:100 \
  DS:FPATemp:GAUGE:180:0:50 \
  DS:FPBTemp:GAUGE:180:0:50 \
  DS:Fan1:GAUGE:180:0:5000 \
  DS:Fan2:GAUGE:180:0:5000 \
  DS:Fan3:GAUGE:180:0:5000 \
  DS:Fan4:GAUGE:180:0:5000 \
  RRA:AVERAGE:0.7:1:1440 \
  RRA:MAX:0.8:60:72 \
  RRA:MIN:0.8:60:72
