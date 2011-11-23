#include "plot_rand_sort.h"
#include "rb_tree.h"
#include "vebtrees.h"
#include "FibonacciHeap.h"
#include "BinaryHeap.h"
#include "rb_tree.h"
#include <stdio.h>
#include <math.h>
#include <time.h>

void plot_rand_sort_veb(int itr, int thres, FILE *gnuplot_ins, FILE *gnuplot_dm, FILE *gnuplot_total){
	srandom(97643);
	printf("vEB: %d elements\n",itr);
	int MAX = pow(2, 24);
	
	clock_t init, ins, delmin;
	init = ins = delmin = 0;
	
	clock_t start = clock();
	vebtree * vebt = veb_initialize(24, thres);
	clock_t end = clock();
	init = end-start;

	int i;
	uint8_t * arr = calloc(MAX, sizeof(uint8_t));
	if (arr == NULL){
		printf("dang...could not allocate enough memory\n");
		exit(1);
	}
	for (i = 0; i < itr; i++){
		uint32_t s = random() % MAX;
		while(arr[s])
			s = random() % MAX;
		arr[s] = 1;
		
		start = clock();
		veb_insert(s, NULL, vebt);
		end = clock();
		ins = ins + end - start;
	}
	for (i = 0; i < itr; i++){
		start = clock();
		veb_delete_min(vebt);
		end = clock();
		delmin = delmin + end - start;
	}
	free(arr);
	veb_destruct(vebt);
	if(gnuplot_ins)
		fprintf(gnuplot_ins, "%d %f\n", itr, (((double) ins / CLOCKS_PER_SEC)*1000000)/itr);
	if(gnuplot_dm)
		fprintf(gnuplot_dm, "%d %f\n", itr, (((double) delmin / CLOCKS_PER_SEC)*1000000)/itr);
	if(gnuplot_total)
		fprintf(gnuplot_total, "%d %f\n", itr, (((double) (ins+delmin) / CLOCKS_PER_SEC)*1000));
}
void plot_rand_sort_bin(int itr, int thres, FILE *gnuplot_ins, FILE *gnuplot_dm, FILE *gnuplot_total){
	srandom(97643);
	printf("BinHeap: %d elements\n",itr);
	int MAX = pow(2, 24);
	
	clock_t init, ins, delmin;
	init = ins = delmin = 0;
	
	clock_t start = clock();
	binary_heap * bheap = bh_init_heap(MAX);
	clock_t end = clock();
	init = init + end - start;
	
	int i;
	uint8_t * arr = calloc(MAX, sizeof(uint8_t));
	if (arr == NULL){
		printf("dang...could not allocate enough memory\n");
		exit(1);
	}
	bh_element *e;
	for (i = 0; i < itr; i++){
		uint32_t s = random() % MAX;
		while(arr[s])
			s = random() % MAX;
		arr[s] = 1;
		
		start = clock();
		bh_insert(s, NULL, bheap);
		end = clock();
		ins = ins + end - start;
	}
	for (i = 0; i < itr; i++){
		start = clock();
		e= bh_delete_min(bheap);
		end = clock();
		free(e);
		delmin = delmin + end - start;
	}
	free(arr);
	bh_destruct(bheap);
	if(gnuplot_ins)
		fprintf(gnuplot_ins, "%d %f\n", itr, (((double) ins / CLOCKS_PER_SEC)*1000000)/itr);
	if(gnuplot_dm)
		fprintf(gnuplot_dm, "%d %f\n", itr, (((double) delmin / CLOCKS_PER_SEC)*1000000)/itr);
	if(gnuplot_total)
		fprintf(gnuplot_total, "%d %f\n", itr, (((double) (ins+delmin) / CLOCKS_PER_SEC)*1000));
}
void plot_rand_sort_fib(int itr, int thres, FILE *gnuplot_ins, FILE *gnuplot_dm, FILE *gnuplot_total){
	srandom(97643);
	printf("FibHeap: %d elements\n",itr);
	int MAX = pow(2, 24);
	
	clock_t init, ins, delmin;
	init = ins = delmin = 0;

	clock_t start = clock();
	FibHeap * fheap = fib_make_heap();
	clock_t end = clock();
	init = init + end - start;
	
	int i;
	uint8_t * arr = calloc(MAX, sizeof(uint8_t));
	if (arr == NULL){
		printf("dang...could not allocate enough memory\n");
		exit(1);
	}
	FibNode *e;
	for (i = 0; i < itr; i++){
		uint32_t s = random() % MAX;
		while(arr[s])
			s = random() % MAX;
		arr[s] = 1;
		
		start = clock();
		fib_insert(s, NULL, fheap);
		end = clock();
		ins = ins + end - start;
	}
	for (i = 0; i < itr; i++){
		e = fib_find_min(fheap);
		start = clock();
		fib_delete_min(fheap);
		end = clock();
		free(e);
		delmin = delmin + end - start;
	}
	free(arr);
	free(fheap);
	if(gnuplot_ins)
		fprintf(gnuplot_ins, "%d %f\n", itr, (((double) ins / CLOCKS_PER_SEC)*1000000)/itr);
	if(gnuplot_dm)
		fprintf(gnuplot_dm, "%d %f\n", itr, (((double) delmin / CLOCKS_PER_SEC)*1000000)/itr);
	if(gnuplot_total)
		fprintf(gnuplot_total, "%d %f\n", itr, (((double) (ins+delmin) / CLOCKS_PER_SEC)*1000));
}
void plot_rand_sort_rb(int itr, int thres, FILE *gnuplot_ins, FILE *gnuplot_dm, FILE *gnuplot_total){
	srandom(97643);
	printf("Red Black tree: %d elements\n",itr);
	int MAX = pow(2, 24);
	
	clock_t init, ins, delmin;
	init = ins = delmin = 0;

	clock_t start = clock();
	rb_tree * heap = rb_init();
	clock_t end = clock();
	init = init + end - start;
	
	int i;
	uint8_t * arr = calloc(MAX, sizeof(uint8_t));
	if (arr == NULL){
		printf("dang...could not allocate enough memory\n");
		exit(1);
	}
	rb_node *e;
	for (i = 0; i < itr; i++){
		uint32_t s = random() % MAX;
		while(arr[s])
			s = random() % MAX;
		arr[s] = 1;

		start = clock();
		rb_insert(s, heap);
		end = clock();
		ins = ins + end - start;
	}
	for (i = 0; i < itr; i++){
		if (heap->n == 0)
			printf("something is wrong!!\n");
		e = rb_find_min(heap);
		start = clock();
		rb_delete(e, heap);
		end = clock();
		delmin = delmin + end - start;
	}
	free(arr);
	rb_destruct(heap);
	if(gnuplot_ins)
		fprintf(gnuplot_ins, "%d %f\n", itr, (((double) ins / CLOCKS_PER_SEC)*1000000)/itr);
	if(gnuplot_dm)
		fprintf(gnuplot_dm, "%d %f\n", itr, (((double) delmin / CLOCKS_PER_SEC)*1000000)/itr);
	if(gnuplot_total)
		fprintf(gnuplot_total, "%d %f\n", itr, (((double) (ins+delmin) / CLOCKS_PER_SEC)*1000));
}