#include "plot_rand_sort.h"
#include "rb_tree.h"
#include "vebtrees.h"
#include "FibonacciHeap.h"
#include "BinaryHeap.h"
#include <stdio.h>
#include <math.h>
#include <time.h>

void plot_rand_sort_veb(int itr, int thres, FILE *gnuplot_ins, FILE *gnuplot_dm, FILE *gnuplot_total){
	srandom(97643);
	printf("Testing with %d elements\n",itr);
	int MAX = pow(2, 24);
	
	double vinit, vins, vdm;
	
	clock_t start = clock();
	vebtree * vebt = veb_initialize(24, thres);
	clock_t end = clock();
	vinit = ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
	
	int i;
	uint8_t * arr = calloc(MAX, sizeof(uint8_t));
	if (arr == NULL){
		printf("dang...could not allocate enough memory\n");
		exit(1);
	}
	vins = 0;
	vdm = 0;
	for (i = 0; i < itr; i++){
		uint32_t s = random() % MAX;
		while(arr[s])
			s = random() % MAX;
		arr[s] = 1;
		
		start = clock();
		veb_insert(s, NULL, vebt);
		end = clock();
		vins += ((double) (end-start) / CLOCKS_PER_SEC) * 1000000;
	}
	for (i = 0; i < itr; i++){
		start = clock();
		veb_delete_min(vebt);
		end = clock();
		vdm += ((double) (end-start) / CLOCKS_PER_SEC) * 1000000;
		}
	free(arr);
	veb_destruct(vebt);
	if(gnuplot_ins)
		fprintf(gnuplot_ins, "%d %f\n", itr, (vins/itr)*1000);
	if(gnuplot_dm)
		fprintf(gnuplot_dm, "%d %f\n", itr, (vdm/itr)*1000);
	if(gnuplot_total)
		fprintf(gnuplot_total, "%d %f\n", itr, (vins+vdm)/1000);
}
void plot_rand_sort_bin(int itr, int thres, FILE *gnuplot_ins, FILE *gnuplot_dm, FILE *gnuplot_total){
	srandom(97643);
	printf("Testing with %d elements\n",itr);
	int MAX = pow(2, 24);
	
	double init, ins, dm;
	
	clock_t start = clock();
	binary_heap * bheap = bh_init_heap(MAX);
	clock_t end = clock();
	init = ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
	
	int i;
	uint8_t * arr = calloc(MAX, sizeof(uint8_t));
	if (arr == NULL){
		printf("dang...could not allocate enough memory\n");
		exit(1);
	}
	ins = 0;
	dm = 0;
	bh_element *e;
	for (i = 0; i < itr; i++){
		uint32_t s = random() % MAX;
		while(arr[s])
			s = random() % MAX;
		arr[s] = 1;
		
		start = clock();
		bh_insert(s, NULL, bheap);
		end = clock();
		ins += ((double) (end-start) / CLOCKS_PER_SEC) * 1000000;
	}
	for (i = 0; i < itr; i++){
		start = clock();
		e= bh_delete_min(bheap);
		end = clock();
		free(e);
		dm += ((double) (end-start) / CLOCKS_PER_SEC) * 1000000;
		}
	free(arr);
	bh_destruct(bheap);
	if(gnuplot_ins)
		fprintf(gnuplot_ins, "%d %f\n", itr, (ins/itr)*1000);
	if(gnuplot_dm)
		fprintf(gnuplot_dm, "%d %f\n", itr, (dm/itr)*1000);
	if(gnuplot_total)
		fprintf(gnuplot_total, "%d %f\n", itr, (ins+dm)/1000);
}
void plot_rand_sort_fib(int itr, int thres, FILE *gnuplot_ins, FILE *gnuplot_dm, FILE *gnuplot_total){
	srandom(97643);
	printf("Testing with %d elements\n",itr);
	int MAX = pow(2, 24);
	
	double init, ins, dm;
	clock_t start = clock();
	FibHeap * fheap = fib_make_heap();
	clock_t end = clock();
	init = ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
	
	int i;
	uint8_t * arr = calloc(MAX, sizeof(uint8_t));
	if (arr == NULL){
		printf("dang...could not allocate enough memory\n");
		exit(1);
	}
	ins = 0;
	dm = 0;
	FibNode *e;
	for (i = 0; i < itr; i++){
		uint32_t s = random() % MAX;
		while(arr[s])
			s = random() % MAX;
		arr[s] = 1;
		
		start = clock();
		fib_insert(s, NULL, fheap);
		end = clock();
		ins += ((double) (end-start) / CLOCKS_PER_SEC) * 1000000;
	}
	for (i = 0; i < itr; i++){
		e = fib_find_min(fheap);
		start = clock();
		fib_delete_min(fheap);
		end = clock();
		free(e);
		dm += ((double) (end-start) / CLOCKS_PER_SEC) * 1000000;
	}
	free(arr);
	free(fheap);
	if(gnuplot_ins)
		fprintf(gnuplot_ins, "%d %f\n", itr, (ins/itr)*1000);
	if(gnuplot_dm)
		fprintf(gnuplot_dm, "%d %f\n", itr, (dm/itr)*1000);
	if(gnuplot_total)
		fprintf(gnuplot_total, "%d %f\n", itr, (ins+dm)/1000);
}
