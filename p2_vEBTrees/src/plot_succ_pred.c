#include "rb_tree.h"
#include "vebtrees.h"
#include <stdio.h>
#include <math.h>
#include <time.h>
#include <sys/time.h>
#include <stdint.h>
#include "plot_dkmax2.h"
#include "plot_succ_pred.h"


void plot_succ_veb(int n, int cutoff, FILE *gnuplot_ins, FILE *gnuplot_succ, FILE *gnuplot_total){
	printf("vEB: %d elements\n",n);
	srandom(235423);
	struct timespec succ, ins, start, end;
	succ.tv_nsec = 0;
	succ.tv_sec = 0;
	ins.tv_nsec = 0;
	ins.tv_sec = 0;
	
	vebtree * vebt = veb_initialize(24, 64);
	//rb_tree * rbt = rb_init();
	
	int i;
	for (i = 0; i < n; i++){
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start);
		veb_insert(i, NULL, vebt);
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end);
		increment(&ins, &start, &end);
		//rb_insert(i, rbt);
	}
	/*list * l = get_key_array(rbt, rbt->root, 0, cutoff);
	list * tl;*/
	int ii = 0;
	//while(l){
	for (i = 1; i <= n/8; i++){
		ii++;
		//int32_t k = random() % n;
		int32_t s;
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start);
		//veb_findsucc(l->key, &s, vebt);
		veb_findsucc(n-i, &s, vebt);
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end);
		if (s != n-i)
			printf("FEJL\n");
		/*tl = l;
		l = l->next;
		free(l);*/
		increment(&succ, &start, &end);
	}
	printf("searched %d keys\n", ii);
	if(gnuplot_ins)
		fprintf(gnuplot_ins, "%d %ld\n", n, ((ins.tv_sec*  1000000000)+(ins.tv_nsec))/n);
	if(gnuplot_succ)
		fprintf(gnuplot_succ, "%d %ld\n", n, ((succ.tv_sec*1000000000)+(succ.tv_nsec))/(n/8));
	if(gnuplot_total)
		fprintf(gnuplot_total, "%d %ld\n", n, (((ins.tv_sec + succ.tv_sec) *  1000)+(ins.tv_nsec + succ.tv_nsec)/1000000));	
	veb_destruct(vebt);
	//rb_destruct(rbt);
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
	/*list * l = get_key_array(rbt, rbt->root, 0, cutoff);
	list * tl;*/
	//while (l){
	for (i = 1; i <= n/8; i++){
		//int32_t k = random() % n;
		int32_t s;
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start);
		//rb_search(l->key, rbt);
		s = rb_search(n-i, rbt);
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end);
		if (s != n-i)
			printf("FEJL\n");
		/*tl = l;
		l = l->next;
		free(tl);*/
		increment(&succ, &start, &end);
	}
	if(gnuplot_ins)
		fprintf(gnuplot_ins, "%d %ld\n", n, ((ins.tv_sec*  1000000000)+(ins.tv_nsec))/n);
	if(gnuplot_succ)
		fprintf(gnuplot_succ, "%d %ld\n", n, ((succ.tv_sec*1000000000)+(succ.tv_nsec))/(n/8));
	if(gnuplot_total)
		fprintf(gnuplot_total, "%d %ld\n", n, (((ins.tv_sec + succ.tv_sec) *  1000)+(ins.tv_nsec + succ.tv_nsec)/1000000));	
	rb_destruct(rbt);
}


list * get_key_array(rb_tree * tree, rb_node * n, uint32_t height, uint32_t cutoff){
	list * llist = NULL;
	list * rlist = NULL;
	list * clist = NULL;
	if (n->left != tree->nil){
		llist = get_key_array(tree, n->left, height+1, cutoff);	
	}
	if (n->right != tree->nil){
		rlist = get_key_array(tree, n->right, height+1, cutoff);
	}
	list * result;
	if (height >= cutoff){
		clist = malloc(sizeof(list));
		clist->key = n->key;
		clist->next = NULL;
		clist->prev = NULL;
		if (llist){
			llist->prev = clist;
			clist->next = llist;
		}
		if (rlist){
			while (rlist->next != NULL)
				rlist = rlist->next;
			rlist->next = clist;
			clist->prev = rlist;
		}
		while (clist->prev != NULL)
			clist = clist->prev;
		return clist;
	}
	else {
		if (rlist && llist){
			rlist->prev = llist;
			llist->next = rlist;
			while (llist->prev != NULL)
				llist = llist->prev;
			return llist;
		}
		else if (rlist)
			return rlist;
		else
			return llist;
			
	}
	
}