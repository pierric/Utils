#!/usr/bin/gnuplot -persist
set ylabel "float"
set xlabel "word"
set grid
set pointsize 0.3
set xtics (0,7,120,128,135,248,255)
set format x "%.2x"
plot [0:255] [-250:250] "float.csv" using 1:2 title "floats of 8 bits" with points
