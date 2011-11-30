#ifndef PLOTSUCC
#define PLOTSUCC

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <stdint.h>
#include "linked_list.h"

typedef struct list list;

struct list{
	uint32_t key;
	list * next;
	list * prev;
};

void plot_succ_veb(int n, int cutoff, FILE *gnuplot_ins, FILE *gnuplot_succ, FILE *gnuplot_total);
void plot_succ_rb(int n, int cutoff, FILE *gnuplot_ins, FILE *gnuplot_succ, FILE *gnuplot_total);
linked_list * get_key_list(rb_tree * tree, uint32_t cutoff);

#endif