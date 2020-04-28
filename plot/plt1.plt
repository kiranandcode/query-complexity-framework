# set size 1.5,0.7
# set boxwidth 0.5
# set style fill solid 1.00
# set notitle
set yrange [-0.01 to 0.02]
# set yrange [0 to 7]
# set xtics font ",0.01" nomirror rotate by 30 right
# set format x '\tiny %s'
set ylabel "No. Bugs found" font ",4"
set tic scale 0
set grid ytics lc rgb "#50505050"
# set style line  1 lt 1 lc rgb '#440154' # dark purple
# set style line  2 lt 1 lc rgb '#ffa600' # purple
# set style line  3 lt 1 lc rgb '#3b518b' # blue
# set style line  4 lt 1 lc rgb '#2c718e' # blue
# set style line  5 lt 1 lc rgb '#21908d' # blue-green
# set style line  6 lt 1 lc rgb '#27ad81' # green
# set style line  7 lt 1 lc rgb '#5cc863' # green
# set style line  8 lt 1 lc rgb '#ff6361' # lime green
# set style fill solid

plot "../data/plt1_1inst_is.dat" using 1:2 title "1-instance" linecolor rgb '#003f5c'
