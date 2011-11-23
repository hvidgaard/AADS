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
#include <stdint.h>

uint32_t ** generate_instance(int size, uint32_t **w){
	int seed = 1234;
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
	uint32_t **w2 = malloc(sizeof(uint32_t *));
	uint32_t *weights;
	uint32_t **edges = generate_instance(num_vertices, w2);
	weights = *w2;
	clock_t start, end;
	double binit = 0;
	double bdm = 0;
	double bdk = 0;
	double bins = 0;
	
	start = clock();
	vebtree * heap = veb_pq_init(24);
	end = clock();
	binit = ((double) (end-start) / CLOCKS_PER_SEC) * 1000;

	
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
		start = clock();
		veb_pq_insert(n, heap);
		end = clock();
		bins += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
		
	}
	uint32_t decrease_key_calls = 0;
	
	start = clock();
	n = veb_pq_deletemin(heap);
	end = clock();
	bdm += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
	while (n) {
		uint32_t u = n->node_nr;
		for (i = 1; i <= edges[u][0]; i++) {
			uint32_t v = edges[u][i];
			uint32_t alt = distances[u] + weights[u * num_vertices + v];
			if (alt < distances[v]) {
				start = clock();
				veb_pq_decrease_key(heap, vertices[v], distances[v] - alt);
				end = clock();
				bdk += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
				distances[v] = alt;
				decrease_key_calls++;
			}
		}
		start = clock();
		n = veb_pq_deletemin(heap);
		end = clock();
		bdm += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
	}
	for (i = 0; i < num_vertices; i++)
		free(vertices[i]);
	veb_destruct(heap);
	free(vertices);
	free(distances);
	free(w2);
	if(gnuplot_ins)
		fprintf(gnuplot_ins, "%d %f\n", num_vertices, (bins/num_vertices)*1000);
	if(gnuplot_dm)
		fprintf(gnuplot_dm, "%d %f\n", num_vertices, (bdm/num_vertices)*1000);
	if(gnuplot_dk)
		fprintf(gnuplot_dk, "%d %f\n", decrease_key_calls, (bdk/decrease_key_calls)*1000);
	if(gnuplot_total)
		fprintf(gnuplot_total, "%d %f\n", num_vertices, (bins+bdm+bdk)/1000);	
}
void plot_dkmax2_bin(uint32_t num_vertices, uint32_t source, FILE *gnuplot_ins, FILE *gnuplot_dm, FILE *gnuplot_total, FILE *gnuplot_dk){
	uint32_t **w2 = malloc(sizeof(uint32_t *));
	uint32_t *weights;
	uint32_t **edges = generate_instance(num_vertices, w2);
	weights = *w2;
	clock_t start, end;
	double binit = 0;
	double bdm = 0;
	double bdk = 0;
	double bins = 0;
	
	start = clock();
	binary_heap * heap = bh_init_heap(num_vertices);
	end = clock();
	binit = ((double) (end-start) / CLOCKS_PER_SEC) * 1000;

	
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
		start = clock();
		vertices[i] = bh_insert(distance, data, heap);
		end = clock();
		bins += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
		
	}
	bh_element *node;
	uint32_t decrease_key_calls = 0;
	
	start = clock();
	node = bh_delete_min(heap);
	end = clock();
	bdm += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
	while (node) {
		uint32_t u = *((uint32_t *)node->data);
		for (i = 1; i <= edges[u][0]; i++) {
			uint32_t v = edges[u][i];
			uint32_t alt = distances[u] + weights[u * num_vertices + v];
			if (alt < distances[v]) {
				start = clock();
				bh_decrease_key(distances[v] - alt, vertices[v], heap);
				end = clock();
				bdk += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
				distances[v] = alt;
				decrease_key_calls++;
			}
		}
		free(node->data);
		free(node);
		start = clock();
		node = bh_delete_min(heap);
		end = clock();
		bdm += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
	}
	bh_destruct(heap);
	free(vertices);
	free(distances);
	free(w2);
	if(gnuplot_ins)
		fprintf(gnuplot_ins, "%d %f\n", num_vertices, (bins/num_vertices)*1000);
	if(gnuplot_dm)
		fprintf(gnuplot_dm, "%d %f\n", num_vertices, (bdm/num_vertices)*1000);
	if(gnuplot_dk)
		fprintf(gnuplot_dk, "%d %f\n", decrease_key_calls, (bdk/decrease_key_calls)*1000);
	if(gnuplot_total)
		fprintf(gnuplot_total, "%d %f\n", num_vertices, (bins+bdm+bdk)/1000);
}
void plot_dkmax2_fib(uint32_t num_vertices, uint32_t source, FILE *gnuplot_ins, FILE *gnuplot_dm, FILE *gnuplot_total, FILE *gnuplot_dk){
	
}
