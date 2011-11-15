#include <stdlib.h>
#include "dijkstra.h"
#include "BinaryHeap.h"
#include "FibonacciHeap.h"
#include "vebtrees.h"

uint dijkstra_bin(uint num_vertices, uint source, uint * weights, uint ** edges) {
	uint *distances = malloc(num_vertices * sizeof(uint));
	
	binary_heap * heap = bh_init_heap(num_vertices);
	bh_element ** vertices = malloc(num_vertices * sizeof(bh_element *));
	
	uint distance;
	uint *data;
	uint i;
	for (i = 0; i < num_vertices; i++) {
		if(i == source)
			distance = 0;
		else
			distance = UINT_MAX;
		distances[i] = distance;
		data = malloc(sizeof(uint));
		*data = i;
		vertices[i] = bh_insert(distance, data, heap);
	}
	bh_element *node;
	uint decrease_key_calls = 0;
	while ((node = bh_find_min(heap))) {
		bh_delete_min(heap);
		uint u = *(uint *)node->data;
		for (i = 1; i <= edges[u][0]; i++) {
			uint v = edges[u][i];
			uint alt = distances[u] + weights[u * num_vertices + v];
			if (alt < distances[v]) {
				bh_decrease_key(distances[v] - alt, vertices[v], heap);
				distances[v] = alt;
				decrease_key_calls++;
			}
		}
	}
	free(heap);
	free(vertices);
	return decrease_key_calls;
}

uint dijkstra_fib(uint num_vertices, uint source, uint * weights, uint ** edges) {
	uint *distances = malloc(num_vertices * sizeof(uint));
	
	FibHeap * heap = fib_make_heap();
	FibNode ** vertices = malloc(num_vertices * sizeof(FibNode *));
	
	uint distance;
	uint *data;
	int i;
	for (i = 0; i < num_vertices; i++) {
		if(i == source)
			distance = 0;
		else
			distance = UINT_MAX;
		distances[i] = distance;
		data = malloc(sizeof(uint));
		*data = i;
		vertices[i] = fib_insert(distance, data, heap);
	}
	FibNode *node;
	uint decrease_key_calls = 0;
	while ((node = fib_find_min(heap))) {
		fib_delete_min(heap);
		uint u = *(uint *)node->data;
		for (i = 1; i <= edges[u][0]; i++) {
			uint v = edges[u][i];
			uint alt = distances[u] + weights[u * num_vertices + v];
			if (alt < distances[v]) {
				fib_decrease_key(distances[v] - alt, vertices[v], heap);
				distances[v] = alt;
				decrease_key_calls++;
			}
		 }
	}
	free(vertices);
	free(heap);
	free(distances);
	return decrease_key_calls;
}