#!/usr/bin/gnuplot -p

set xdata time
set timefmt "%Y-%m-%d"
# set format x "%d-%M\n%Y"
set xtics format "%d %b\n%Y"

# set autoscale x
# set autoscale y

set style data points
plot  "nexus.dat" using 1:2:xtic(1) title 'TS-Rain', "" using 1:3:xtic(1) title 'Rainwater harvesting'
