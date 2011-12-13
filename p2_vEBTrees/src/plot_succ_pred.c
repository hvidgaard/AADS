#include "rb_tree.h"
#include "vebtrees.h"
#include <stdio.h>
#include <math.h>
#include <time.h>
#include <sys/time.h>
#include <stdint.h>
#include "plot_dkmax2.h"
#include "plot_succ_pred.h"

void get_keys(rb_tree * tree, rb_node * n, uint32_t height, uint32_t cutoff, linked_list * ll);

void plot_succ_veb(int n, int cutoff, FILE *gnuplot_ins, FILE *gnuplot_succ, FILE *gnuplot_total){
	printf("vEB: %d elements\n",n);
	srandom(235423);
	struct timespec succ, ins, start, end;
	succ.tv_nsec = 0;
	succ.tv_sec = 0;
	ins.tv_nsec = 0;
	ins.tv_sec = 0;
	
	vebtree * vebt = veb_initialize(24, 64);
	rb_tree * rbt = rb_init();
	
	int i;
	for (i = 0; i < n; i++){
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start);
		veb_insert(i, NULL, vebt);
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end);
		increment(&ins, &start, &end);
		rb_insert(i, rbt);
	}
	linked_list * ll = get_key_list(rbt, cutoff);
	linked_list_node * node= ll->first;
	int ii = 0;
	while(node){
		ii++;
		int32_t s;
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start);
		veb_findsucc(node->data, &s, vebt);
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end);
		node = node->next;
		increment(&succ, &start, &end);
	}
	printf("searched %d keys\n", ii);
	if(gnuplot_ins)
		fprintf(gnuplot_ins, "%d %ld\n", n, ((ins.tv_sec*  1000000000)+(ins.tv_nsec))/n);
	if(gnuplot_succ)
		fprintf(gnuplot_succ, "%d %ld\n", n, ((succ.tv_sec*1000000000)+(succ.tv_nsec))/(ii));
	if(gnuplot_total)
		fprintf(gnuplot_total, "%d %ld\n", n, (((ins.tv_sec + succ.tv_sec) *  1000)+(ins.tv_nsec + succ.tv_nsec)/1000000));	
	veb_destruct(vebt);
	rb_destruct(rbt);
	linked_list_destruct(ll);
}
void plot_succ_rb(int n, int cutoff, FILE *gnuplot_ins, FILE *gnuplot_succ, FILE *gnuplot_total){
	printf("RBt: %d elements\n",n);
	srandom(235423);
	struct timespec succ, ins, start, end;
	succ.tv_nsec = 0;
	succ.tv_sec = 0;
	ins.tv_nsec = 0;
	ins.tv_sec = 0;
	
	rb_tree * rbt = rb_init();
	
	int i;
	for (i = 0; i < n; i++){
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start);
		rb_insert(i, rbt);
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end);
		increment(&ins, &start, &end);
	}
	linked_list * ll = get_key_list(rbt, cutoff);
	linked_list_node * node= ll->first;
	int ii = 0;
	while(node){
		ii++;
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start);
		rb_search(node->data, rbt);
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end);
		node = node->next;
		increment(&succ, &start, &end);
	}
	printf("searched %d keys\n", ii);
	if(gnuplot_ins)
		fprintf(gnuplot_ins, "%d %ld\n", n, ((ins.tv_sec*  1000000000)+(ins.tv_nsec))/n);
	if(gnuplot_succ)
		fprintf(gnuplot_succ, "%d %ld\n", n, ((succ.tv_sec*1000000000)+(succ.tv_nsec))/(ii));
	if(gnuplot_total)
		fprintf(gnuplot_total, "%d %ld\n", n, (((ins.tv_sec + succ.tv_sec) *  1000)+(ins.tv_nsec + succ.tv_nsec)/1000000));	
	rb_destruct(rbt);
	linked_list_destruct(ll);
}


linked_list * get_key_list(rb_tree * tree, uint32_t cutoff){
	linked_list *ll = linked_list_init();
	get_keys(tree, tree->root, 1, cutoff, ll);
	return ll;
}

void get_keys(rb_tree * tree, rb_node * n, uint32_t height, uint32_t cutoff, linked_list * ll){
	if (height > cutoff)
		linked_list_add_tail(n->key, ll);
	if (n->left != tree->nil)
		get_keys(tree, n->left, height+1, cutoff, ll);
	if (n->right != tree->nil)
		get_keys(tree, n->right, height+1, cutoff, ll);
}