#include "plot_dkmax2.h"
#include "rb_tree.h"
#include "vebtrees.h"
#include "veb_pq.h"
#include "FibonacciHeap.h"
#include "BinaryHeap.h"
#include "graph_generators.h"
#include <stdio.h>
#include <math.h>
#include <time.h>
#include <sys/time.h>
#include <stdint.h>

/*struct timespec *diff(struct timespec *start, struct timespec *end);
struct timespec *add(struct timespec *t1, struct timespec *t2);

struct timespec *diff(struct timespec *start, struct timespec *end){
	struct timespec *temp = malloc(sizeof(struct timespec));
	if ((end->tv_nsec - start->tv_nsec) < 0) {
		temp->tv_sec = end->tv_sec - start->tv_sec - 1;
		temp->tv_nsec = 1000000000 + end->tv_nsec - start->tv_nsec;
	} else {
		temp->tv_sec = end->tv_sec - start->tv_sec;
		temp->tv_nsec = end->tv_nsec - start->tv_nsec;
	}
	return temp;
}
struct timespec *add(struct timespec *t1, struct timespec *t2){
	struct timespec *temp = malloc(sizeof(struct timespec));
	if ((t1->tv_nsec + t2->tv_nsec) >= 1000000000) {
		temp->tv_sec = t1->tv_sec + t2->tv_sec + 1;
		temp->tv_nsec = t1->tv_nsec + t2->tv_nsec - 1000000000;
	} else {
		temp->tv_sec = t1->tv_sec + t2->tv_sec;
		temp->tv_nsec = t1->tv_nsec + t2->tv_nsec;
	}
	return temp;
}*/

void increment(struct timespec *t, struct timespec *start, struct timespec *end){
	if ((t->tv_nsec + end->tv_nsec) >= 1000000000) {
		t->tv_sec = t->tv_sec + end->tv_sec + 1;
		t->tv_nsec = t->tv_nsec + end->tv_nsec - 1000000000;
	} else {
		t->tv_sec = t->tv_sec + end->tv_sec;
		t->tv_nsec = t->tv_nsec + end->tv_nsec;
	}
	if ((t->tv_nsec - start->tv_nsec) < 0) {
		t->tv_sec = t->tv_sec - start->tv_sec - 1;
		t->tv_nsec = 1000000000 + t->tv_nsec - start->tv_nsec;
	} else {
		t->tv_sec = t->tv_sec - start->tv_sec;
		t->tv_nsec = t->tv_nsec - start->tv_nsec;
	}
}


uint32_t ** generate_instance(int size, uint32_t **w){
	int seed = 987543;
	uint32_t **edges;
	uint32_t *weights = generate_decrease_key_max_graph_2(size, 1000, seed);
	*w = weights;
	edges = malloc((size+1) * sizeof(uint32_t *));
	uint32_t *t_edges = malloc(size * sizeof(uint32_t));
	uint32_t i, j;
	for (i = 0; i < size; i++) {
		uint32_t count = 0;
		for (j = 0; j < size; j++){
			if (weights[(i * size) + j])
				t_edges[++count] = j;
		}
		edges[i] = malloc((count+1) * sizeof(uint32_t));
		edges[i][0] = count;
		for (j = 1; j <= count; j++)
			edges[i][j] = t_edges[j];
	}
	free(t_edges);
	return edges;
}

void plot_dkmax2_veb(uint32_t num_vertices, uint32_t source, FILE *gnuplot_ins, FILE *gnuplot_dm, FILE *gnuplot_total, FILE *gnuplot_dk){
	printf("vEB: %d vertices\n",num_vertices);
	uint32_t **w2 = malloc(sizeof(uint32_t *));
	uint32_t *weights;
	uint32_t **edges = generate_instance(num_vertices, w2);
	weights = *w2;
	struct timespec delmin, deck, ins, start, end;
	delmin.tv_nsec = 0;
	delmin.tv_sec = 0;
	deck.tv_nsec = 0;
	deck.tv_sec = 0;
	ins.tv_nsec = 0;
	ins.tv_sec = 0;
	//start = end = delmin = deck = ins = 0;
	
	vebtree * heap = veb_pq_init(24);

	
	uint32_t *distances = malloc(num_vertices * sizeof(uint32_t));
	veb_pq_node ** vertices = malloc(num_vertices * sizeof(bh_element *));
	
	uint32_t distance;
	uint32_t i;
	veb_pq_node *n;
	for (i = 0; i < num_vertices; i++) {
		if(i == source)
			distance = 0;
		else
			distance = UINT_MAX;
		distances[i] = distance;
		n = malloc(sizeof(veb_pq_node));
		n->node_prio = distance;
		n->node_nr = i;
		vertices[i] = n;
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start);
		veb_pq_insert(n, heap);
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end);
		increment(&ins, &start, &end);
		
	}
	uint32_t decrease_key_calls = 0;
	
	clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start);
	n = veb_pq_deletemin(heap);
	clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end);
	increment(&delmin, &start, &end);
	while (n) {
		uint32_t u = n->node_nr;
		for (i = 1; i <= edges[u][0]; i++) {
			uint32_t v = edges[u][i];
			uint32_t alt = distances[u] + weights[u * num_vertices + v];
			if (alt < distances[v]) {
				clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start);
				veb_pq_decrease_key(heap, vertices[v], distances[v] - alt);
				clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end);
				increment(&deck, &start, &end);
				distances[v] = alt;
				decrease_key_calls++;
			}
		}
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start);
		n = veb_pq_deletemin(heap);
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end);
		increment(&delmin, &start, &end);
	}
	for (i = 0; i < num_vertices; i++)
		free(vertices[i]);
	for (i = 0; i < num_vertices; i++)
		free(edges[i]);
	free(edges);
	veb_destruct(heap);
	free(vertices);
	free(distances);
	free(w2);
	free(weights);
	if(gnuplot_ins)
		fprintf(gnuplot_ins, "%d %ld\n", num_vertices, ((ins.tv_sec*  1000000000)+(ins.tv_nsec))/num_vertices);
	if(gnuplot_dm)
		fprintf(gnuplot_dm, "%d %ld\n", num_vertices, ((delmin.tv_sec*1000000000)+(delmin.tv_nsec))/num_vertices);
	if(gnuplot_dk)
		fprintf(gnuplot_dk, "%d %ld\n", num_vertices, ((deck.tv_sec*  1000000000)+(deck.tv_nsec))/decrease_key_calls);
	if(gnuplot_total)
		fprintf(gnuplot_total, "%d %ld\n", num_vertices, (((ins.tv_sec + delmin.tv_sec + deck.tv_sec) *  1000)+(ins.tv_nsec + delmin.tv_nsec + deck.tv_nsec)/1000000));	
}
void plot_dkmax2_bin(uint32_t num_vertices, uint32_t source, FILE *gnuplot_ins, FILE *gnuplot_dm, FILE *gnuplot_total, FILE *gnuplot_dk){
	printf("BinHeap: %d vertices\n",num_vertices);
	uint32_t **w2 = malloc(sizeof(uint32_t *));
	uint32_t *weights;
	uint32_t **edges = generate_instance(num_vertices, w2);
	weights = *w2;
	struct timespec delmin, deck, ins, start, end;
	delmin.tv_nsec = 0;
	delmin.tv_sec = 0;
	deck.tv_nsec = 0;
	deck.tv_sec = 0;
	ins.tv_nsec = 0;
	ins.tv_sec = 0;
	
	binary_heap * heap = bh_init_heap(num_vertices);
	
	uint32_t *distances = malloc(num_vertices * sizeof(uint32_t));
	bh_element ** vertices = malloc(num_vertices * sizeof(bh_element *));
	
	uint32_t distance;
	uint32_t *data;
	uint32_t i;
	for (i = 0; i < num_vertices; i++) {
		if(i == source)
			distance = 0;
		else
			distance = UINT_MAX;
		distances[i] = distance;
		data = malloc(sizeof(uint32_t));
		*data = i;
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start);
		vertices[i] = bh_insert(distance, data, heap);
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end);
		increment(&ins, &start, &end);
		
	}
	bh_element *node;
	uint32_t decrease_key_calls = 0;
	
	clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start);
	node = bh_delete_min(heap);
	clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end);
	increment(&delmin, &start, &end);
	while (node) {
		uint32_t u = *((uint32_t *)node->data);
		for (i = 1; i <= edges[u][0]; i++) {
			uint32_t v = edges[u][i];
			uint32_t alt = distances[u] + weights[u * num_vertices + v];
			if (alt < distances[v]) {
				clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start);
				bh_decrease_key(distances[v] - alt, vertices[v], heap);
				clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end);
				increment(&deck, &start, &end);
				distances[v] = alt;
				decrease_key_calls++;
			}
		}
		free(node->data);
		free(node);
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start);
		node = bh_delete_min(heap);
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end);
		increment(&delmin, &start, &end);
	}
	for (i = 0; i < num_vertices; i++)
		free(edges[i]);
	free(edges);
	bh_destruct(heap);
	free(vertices);
	free(distances);
	free(w2);
	free(weights);
	if(gnuplot_ins)
		fprintf(gnuplot_ins, "%d %ld\n", num_vertices, ((ins.tv_sec*  1000000000)+(ins.tv_nsec))/num_vertices);
	if(gnuplot_dm)
		fprintf(gnuplot_dm, "%d %ld\n", num_vertices, ((delmin.tv_sec*1000000000)+(delmin.tv_nsec))/num_vertices);
	if(gnuplot_dk)
		fprintf(gnuplot_dk, "%d %ld\n", num_vertices, ((deck.tv_sec*  1000000000)+(deck.tv_nsec))/decrease_key_calls);
	if(gnuplot_total)
		fprintf(gnuplot_total, "%d %ld\n", num_vertices, (((ins.tv_sec + delmin.tv_sec + deck.tv_sec) *  1000)+(ins.tv_nsec + delmin.tv_nsec + deck.tv_nsec)/1000000));	
}
void plot_dkmax2_fib(uint32_t num_vertices, uint32_t source, FILE *gnuplot_ins, FILE *gnuplot_dm, FILE *gnuplot_total, FILE *gnuplot_dk){
	printf("FibHeap: %d vertices\n",num_vertices);
	uint32_t **w2 = malloc(sizeof(uint32_t *));
	uint32_t *weights;
	uint32_t **edges = generate_instance(num_vertices, w2);
	weights = *w2;
	struct timespec delmin, deck, ins, start, end;
	delmin.tv_nsec = 0;
	delmin.tv_sec = 0;
	deck.tv_nsec = 0;
	deck.tv_sec = 0;
	ins.tv_nsec = 0;
	ins.tv_sec = 0;
	
	FibHeap * heap = fib_make_heap();
	
	uint32_t *distances = malloc(num_vertices * sizeof(uint32_t));
	FibNode ** vertices = malloc(num_vertices * sizeof(FibNode *));
	
	uint32_t distance;
	uint32_t *data;
	uint32_t i;
	for (i = 0; i < num_vertices; i++) {
		if(i == source)
			distance = 0;
		else
			distance = UINT_MAX;
		distances[i] = distance;
		data = malloc(sizeof(uint32_t));
		*data = i;
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start);
		vertices[i] = fib_insert(distance, data, heap);
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end);
		increment(&ins, &start, &end);
		
	}
	
	uint32_t decrease_key_calls = 0;
	
	FibNode *node = fib_find_min(heap);

	clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start);
	fib_delete_min(heap);
	clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end);
	increment(&delmin, &start, &end);
	while (node) {
		uint32_t u = *(uint *)node->data;
		for (i = 1; i <= edges[u][0]; i++) {
			uint32_t v = edges[u][i];
			uint32_t alt = distances[u] + weights[u * num_vertices + v];
			if (alt < distances[v]) {
				clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start);
				fib_decrease_key(distances[v] - alt, vertices[v], heap);
				clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end);
				increment(&deck, &start, &end);
				distances[v] = alt;
				decrease_key_calls++;
			}
		}
		node = fib_find_min(heap);
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start);
		fib_delete_min(heap);
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end);
		increment(&delmin, &start, &end);
	}
	for (i = 0; i < num_vertices; i++){
		free(vertices[i]->data);
		free(vertices[i]);
	}
	for (i = 0; i < num_vertices; i++)
		free(edges[i]);
	free(edges);
	free(heap);
	free(vertices);
	free(distances);
	free(w2);
	free(weights);
	if(gnuplot_ins)
		fprintf(gnuplot_ins, "%d %ld\n", num_vertices, ((ins.tv_sec*  1000000000)+(ins.tv_nsec))/num_vertices);
	if(gnuplot_dm)
		fprintf(gnuplot_dm, "%d %ld\n", num_vertices, ((delmin.tv_sec*1000000000)+(delmin.tv_nsec))/num_vertices);
	if(gnuplot_dk)
		fprintf(gnuplot_dk, "%d %ld\n", num_vertices, ((deck.tv_sec*  1000000000)+(deck.tv_nsec))/decrease_key_calls);
	if(gnuplot_total)
		fprintf(gnuplot_total, "%d %ld\n", num_vertices, (((ins.tv_sec + delmin.tv_sec + deck.tv_sec) *  1000)+(ins.tv_nsec + delmin.tv_nsec + deck.tv_nsec)/1000000));	
}
