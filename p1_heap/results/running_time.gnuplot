set terminal png xFFFFFF nocrop enhanced font arial 8 size 800,600

# set output 'binary.png'
# set ylabel "time in s"
# set xlabel "vertices"
# plot "< awk '$1==\"random\" {print $2,$3,$4} ' Binary" using 1:3 title 'Binary' with lines

# set output 'running_time.png'
#, 'Fibonacci', 'Primitive'

set output 'binary.png'
set logscale x2 
plot 'binary-random.dat' using 1:3 title 'Binary heap' with lines,'binary-random.dat' using 1:3:4:5 title 'Binary heap errors' with yerrorbars