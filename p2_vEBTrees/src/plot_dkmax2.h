#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <stdint.h>

#ifndef UINT_MAX
#define UINT_MAX 16777215
//#define UINT_MAX 268435456
#endif
uint32_t ** generate_instance(int size, uint32_t **w);
void plot_dkmax2_veb(uint32_t num_vertices, uint32_t source, FILE *gnuplot_ins, FILE *gnuplot_dm, FILE *gnuplot_total, FILE *gnuplot_dk);
void plot_dkmax2_bin(uint32_t num_vertices, uint32_t source, FILE *gnuplot_ins, FILE *gnuplot_dm, FILE *gnuplot_total, FILE *gnuplot_dk);
void plot_dkmax2_fib(uint32_t num_vertices, uint32_t source, FILE *gnuplot_ins, FILE *gnuplot_dm, FILE *gnuplot_total, FILE *gnuplot_dk);
void increment(struct timespec *t, struct timespec *start, struct timespec *end);