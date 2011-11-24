#include "plot_rand_sort.h"
#include "rb_tree.h"
#include "vebtrees.h"
#include "FibonacciHeap.h"
#include "BinaryHeap.h"
#include "rb_tree.h"
#include <stdio.h>
#include <math.h>
#include <time.h>
#include "plot_dkmax2.h"


void plot_rand_sort_veb(int num_vertices, int thres, FILE *gnuplot_ins, FILE *gnuplot_dm, FILE *gnuplot_total){
	srandom(97643);
	printf("vEB: %d elements\n",num_vertices);
	int MAX = pow(2, 24);
	
	struct timespec delmin, ins, start, end;
	delmin.tv_nsec = 0;
	delmin.tv_sec = 0;
	ins.tv_nsec = 0;
	ins.tv_sec = 0;

	vebtree * vebt = veb_initialize(24, thres);

	int i;
	uint8_t * arr = calloc(MAX, sizeof(uint8_t));
	if (arr == NULL){
		printf("dang...could not allocate enough memory\n");
		exit(1);
	}
	for (i = 0; i < num_vertices; i++){
		uint32_t s = random() % MAX;
		while(arr[s])
			s = random() % MAX;
		arr[s] = 1;
		
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start);
		veb_insert(s, NULL, vebt);
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end);
		increment(&ins, &start, &end);
	}
	for (i = 0; i < num_vertices; i++){
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start);
		veb_delete_min(vebt);
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end);
		increment(&delmin, &start, &end);
	}
	free(arr);
	veb_destruct(vebt);
	if(gnuplot_ins)
		fprintf(gnuplot_ins, "%d %ld\n", num_vertices, ((ins.tv_sec*  1000000000)+(ins.tv_nsec))/num_vertices);
	if(gnuplot_dm)
		fprintf(gnuplot_dm, "%d %ld\n", num_vertices, ((delmin.tv_sec*1000000000)+(delmin.tv_nsec))/num_vertices);
	if(gnuplot_total)
		fprintf(gnuplot_total, "%d %ld\n", num_vertices, (((ins.tv_sec + delmin.tv_sec) *  1000)+(ins.tv_nsec + delmin.tv_nsec)/1000000));	
}
void plot_rand_sort_bin(int num_vertices, int thres, FILE *gnuplot_ins, FILE *gnuplot_dm, FILE *gnuplot_total){
	srandom(97643);
	printf("BinHeap: %d elements\n",num_vertices);
	int MAX = pow(2, 24);
	
	struct timespec delmin, ins, start, end;
	delmin.tv_nsec = 0;
	delmin.tv_sec = 0;
	ins.tv_nsec = 0;
	ins.tv_sec = 0;
	
	binary_heap * bheap = bh_init_heap(MAX);
	
	int i;
	uint8_t * arr = calloc(MAX, sizeof(uint8_t));
	if (arr == NULL){
		printf("dang...could not allocate enough memory\n");
		exit(1);
	}
	bh_element *e;
	for (i = 0; i < num_vertices; i++){
		uint32_t s = random() % MAX;
		while(arr[s])
			s = random() % MAX;
		arr[s] = 1;
		
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start);
		bh_insert(s, NULL, bheap);
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end);
		increment(&ins, &start, &end);
	}
	for (i = 0; i < num_vertices; i++){
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start);
		e= bh_delete_min(bheap);
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end);
		increment(&delmin, &start, &end);
		free(e);
	}
	free(arr);
	bh_destruct(bheap);
	if(gnuplot_ins)
		fprintf(gnuplot_ins, "%d %ld\n", num_vertices, ((ins.tv_sec*  1000000000)+(ins.tv_nsec))/num_vertices);
	if(gnuplot_dm)
		fprintf(gnuplot_dm, "%d %ld\n", num_vertices, ((delmin.tv_sec*1000000000)+(delmin.tv_nsec))/num_vertices);
	if(gnuplot_total)
		fprintf(gnuplot_total, "%d %ld\n", num_vertices, (((ins.tv_sec + delmin.tv_sec) *  1000)+(ins.tv_nsec + delmin.tv_nsec)/1000000));	
}
void plot_rand_sort_fib(int num_vertices, int thres, FILE *gnuplot_ins, FILE *gnuplot_dm, FILE *gnuplot_total){
	srandom(97643);
	printf("FibHeap: %d elements\n",num_vertices);
	int MAX = pow(2, 24);
	
	struct timespec delmin, ins, start, end;
	delmin.tv_nsec = 0;
	delmin.tv_sec = 0;
	ins.tv_nsec = 0;
	ins.tv_sec = 0;

	FibHeap * fheap = fib_make_heap();
	
	int i;
	uint8_t * arr = calloc(MAX, sizeof(uint8_t));
	if (arr == NULL){
		printf("dang...could not allocate enough memory\n");
		exit(1);
	}
	FibNode *e;
	for (i = 0; i < num_vertices; i++){
		uint32_t s = random() % MAX;
		while(arr[s])
			s = random() % MAX;
		arr[s] = 1;
		
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start);
		fib_insert(s, NULL, fheap);
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end);
		increment(&ins, &start, &end);
	}
	for (i = 0; i < num_vertices; i++){
		e = fib_find_min(fheap);
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start);
		fib_delete_min(fheap);
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end);
		increment(&delmin, &start, &end);
		free(e);
	}
	free(arr);
	free(fheap);
	if(gnuplot_ins)
		fprintf(gnuplot_ins, "%d %ld\n", num_vertices, ((ins.tv_sec*  1000000000)+(ins.tv_nsec))/num_vertices);
	if(gnuplot_dm)
		fprintf(gnuplot_dm, "%d %ld\n", num_vertices, ((delmin.tv_sec*1000000000)+(delmin.tv_nsec))/num_vertices);
	if(gnuplot_total)
		fprintf(gnuplot_total, "%d %ld\n", num_vertices, (((ins.tv_sec + delmin.tv_sec) *  1000)+(ins.tv_nsec + delmin.tv_nsec)/1000000));	
}
void plot_rand_sort_rb(int num_vertices, int thres, FILE *gnuplot_ins, FILE *gnuplot_dm, FILE *gnuplot_total){
	srandom(97643);
	printf("Red Black tree: %d elements\n",num_vertices);
	int MAX = pow(2, 24);
	
	struct timespec delmin, ins, start, end;
	delmin.tv_nsec = 0;
	delmin.tv_sec = 0;
	ins.tv_nsec = 0;
	ins.tv_sec = 0;
	
	rb_tree * heap = rb_init();
	
	int i;
	uint8_t * arr = calloc(MAX, sizeof(uint8_t));
	if (arr == NULL){
		printf("dang...could not allocate enough memory\n");
		exit(1);
	}
	rb_node *e;
	for (i = 0; i < num_vertices; i++){
		uint32_t s = random() % MAX;
		while(arr[s])
			s = random() % MAX;
		arr[s] = 1;

		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start);
		rb_insert(s, heap);
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end);
		increment(&ins, &start, &end);
	}
	for (i = 0; i < num_vertices; i++){
		e = rb_find_min(heap);
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start);
		rb_delete(e, heap);
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end);
		increment(&delmin, &start, &end);
	}
	free(arr);
	rb_destruct(heap);
	if(gnuplot_ins)
		fprintf(gnuplot_ins, "%d %ld\n", num_vertices, ((ins.tv_sec*  1000000000)+(ins.tv_nsec))/num_vertices);
	if(gnuplot_dm)
		fprintf(gnuplot_dm, "%d %ld\n", num_vertices, ((delmin.tv_sec*1000000000)+(delmin.tv_nsec))/num_vertices);
	if(gnuplot_total)
		fprintf(gnuplot_total, "%d %ld\n", num_vertices, (((ins.tv_sec + delmin.tv_sec) *  1000)+(ins.tv_nsec + delmin.tv_nsec)/1000000));	
}