#!/usr/bin/gnuplot -p

set xdata time
set timefmt "%Y-%m-%d"
set format x "%Y-%m-%d"
set autoscale x

set style data linespoints
plot  "nexus.dat" using 1:2 title 'Raining'
