#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <stdint.h>

typedef struct list list;

struct list{
	uint32_t key;
	list * next;
	list * prev;
};

void plot_succ_veb(int n, int cutoff, FILE *gnuplot_ins, FILE *gnuplot_succ, FILE *gnuplot_total);
void plot_succ_rb(int n, int cutoff, FILE *gnuplot_ins, FILE *gnuplot_succ, FILE *gnuplot_total);
list * get_key_array(rb_tree * tree, rb_node * n, uint32_t height, uint32_t cutoff);