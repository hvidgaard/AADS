set terminal png transparent nocrop enhanced font arial 8 size 1200,900
set key left

set xlabel 'vertices'
set ylabel 'time'

set output 'binary-all.png'
plot \
'binary-random.dat' using 1:3 title 'random graph' with lines, \
'binary-random.dat' using 1:3:4 title 'random error margin' with yerrorbars, \
'binary-dkmax.dat' using 1:3 title 'dkmax graph' with lines, \
'binary-dkmax.dat' using 1:3:4 title 'dkmax error margin' with yerrorbars, \
'binary-dkmax2.dat' using 1:3 title 'dkmax2 graph' with lines, \
'binary-dkmax2.dat' using 1:3:4 title 'dkmax2 error margin' with yerrorbars

set output 'binary-random-dkmax.png'
plot \
'binary-random.dat' using 1:3 title 'random graph' with lines, \
'binary-random.dat' using 1:3:4 title 'random error margin' with yerrorbars, \
'binary-dkmax.dat' using 1:3 title 'dkmax graph' with lines, \
'binary-dkmax.dat' using 1:3:4 title 'dkmax error margin' with yerrorbars


set output 'fibonacci-all.png'
plot \
'fibonacci-random.dat' using 1:3 title 'random graph' with lines, \
'fibonacci-random.dat' using 1:3:4 title 'random error margin' with yerrorbars, \
'fibonacci-dkmax.dat' using 1:3 title 'dkmax graph' with lines, \
'fibonacci-dkmax.dat' using 1:3:4 title 'dkmax error margin' with yerrorbars, \
'fibonacci-dkmax2.dat' using 1:3 title 'dkmax2 graph' with lines, \
'fibonacci-dkmax2.dat' using 1:3:4 title 'dkmax2 error margin' with yerrorbars

set output 'fibonacci-random-dkmax.png'
plot \
'fibonacci-random.dat' using 1:3 title 'random graph' with lines, \
'fibonacci-random.dat' using 1:3:4 title 'random error margin' with yerrorbars, \
'fibonacci-dkmax.dat' using 1:3 title 'dkmax graph' with lines, \
'fibonacci-dkmax.dat' using 1:3:4 title 'dkmax error margin' with yerrorbars


set output 'primitive-all.png'
plot \
'primitive-random.dat' using 1:3 title 'random graph' with lines, \
'primitive-random.dat' using 1:3:4 title 'random error margin' with yerrorbars, \
'primitive-dkmax.dat' using 1:3 title 'dkmax graph' with lines, \
'primitive-dkmax.dat' using 1:3:4 title 'dkmax error margin' with yerrorbars, \
'primitive-dkmax2.dat' using 1:3 title 'dkmax2 graph' with lines, \
'primitive-dkmax2.dat' using 1:3:4 title 'dkmax2 error margin' with yerrorbars

set output 'primitive-random-dkmax.png'
plot \
'primitive-random.dat' using 1:3 title 'random graph' with lines, \
'primitive-random.dat' using 1:3:4 title 'random error margin' with yerrorbars, \
'primitive-dkmax.dat' using 1:3 title 'dkmax graph' with lines, \
'primitive-dkmax.dat' using 1:3:4 title 'dkmax error margin' with yerrorbars



set output 'fibonacci-binary-random.png'
plot \
'fibonacci-random.dat' using 1:3 title 'fibonacci heap' with lines, \
'fibonacci-random.dat' using 1:3:4 title 'fibonacci error margin' with yerrorbars, \
'binary-random.dat' using 1:3 title 'binary heap' with lines, \
'binary-random.dat' using 1:3:4 title 'binary error margin' with yerrorbars

set output 'fibonacci-binary-dkmax.png'
plot \
'fibonacci-dkmax.dat' using 1:3 title 'fibonacci heap' with lines, \
'fibonacci-dkmax.dat' using 1:3:4 title 'fibonacci error margin' with yerrorbars, \
'binary-dkmax.dat' using 1:3 title 'binary heap' with lines, \
'binary-dkmax.dat' using 1:3:4 title 'binary error margin' with yerrorbars

set output 'fibonacci-binary-dkmax2.png'
plot \
'fibonacci-dkmax2.dat' using 1:3 title 'fibonacci heap' with lines, \
'fibonacci-dkmax2.dat' using 1:3:4 title 'fibonacci error margin' with yerrorbars, \
'binary-dkmax2.dat' using 1:3 title 'binary heap' with lines, \
'binary-dkmax2.dat' using 1:3:4 title 'binary error margin' with yerrorbars


set output 'fibonacci-binary-primitive-random.png'
plot \
'fibonacci-random.dat' using 1:3 title 'fibonacci heap' with lines, \
'fibonacci-random.dat' using 1:3:4 title 'fibonacci error margin' with yerrorbars, \
'binary-random.dat' using 1:3 title 'binary heap' with lines, \
'binary-random.dat' using 1:3:4 title 'binary error margin' with yerrorbars, \
'primitive-random.dat' using 1:3 title 'primitive heap' with lines, \
'primitive-random.dat' using 1:3:4 title 'primitive error margin' with yerrorbars

set output 'fibonacci-binary-primitive-dkmax.png'
plot \
'fibonacci-dkmax.dat' using 1:3 title 'fibonacci heap' with lines, \
'fibonacci-dkmax.dat' using 1:3:4 title 'fibonacci error margin' with yerrorbars, \
'binary-dkmax.dat' using 1:3 title 'binary heap' with lines, \
'binary-dkmax.dat' using 1:3:4 title 'binary error margin' with yerrorbars, \
'primitive-dkmax.dat' using 1:3 title 'primitive heap' with lines, \
'primitive-dkmax.dat' using 1:3:4 title 'primitive error margin' with yerrorbars

set output 'fibonacci-binary-primitive-dkmax2.png'
plot \
'fibonacci-dkmax2.dat' using 1:3 title 'fibonacci heap' with lines, \
'fibonacci-dkmax2.dat' using 1:3:4 title 'fibonacci error margin' with yerrorbars, \
'binary-dkmax2.dat' using 1:3 title 'binary heap' with lines, \
'binary-dkmax2.dat' using 1:3:4 title 'binary error margin' with yerrorbars, \
'primitive-dkmax2.dat' using 1:3 title 'primitive heap' with lines, \
'primitive-dkmax2.dat' using 1:3:4 title 'primitive error margin' with yerrorbars

