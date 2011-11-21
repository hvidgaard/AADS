#include <stdlib.h>
#include <unistd.h>
#include "dijkstra.h"
#include "BinaryHeap.h"
#include "FibonacciHeap.h"
#include "vebtrees.h"
#include "veb_pq.h"
#include "time.h"
#include <stdio.h>

//#define PRINT

#ifdef PRINT
#include <stdio.h>
#endif

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
uint dijkstra_veb(uint num_vertices, uint source, uint* weights, uint** edges){
	uint *distances = malloc(num_vertices * sizeof(uint));
	
	vebtree * heap = veb_pq_init(24);
	
	veb_pq_node ** vertices = malloc(num_vertices * sizeof(veb_pq_node));
	
	uint distance;
	int i;
	
	#ifdef PRINT
	printf("source: %d, num_vertices: %d\n", source, num_vertices);
	int j;
	for (i = 0; i < num_vertices; i++){
		//printf("node %d have %d outdegree\n", i, edges[i][0]);
		for (j = 1; j <= edges[i][0]; j++){
			printf("edge from %d to %d : %d\n", i, edges[i][j], weights[i * num_vertices + edges[i][j]]);
		}
	}
	#endif
	

	for (i = 0; i < num_vertices; i++) {
		if(i == source)
			distance = 0;
		else
			distance = UINT_MAX;
		distances[i] = distance;
		//data = malloc(sizeof(uint));
		//*data = i;
		veb_pq_node * node = malloc(sizeof(veb_pq_node));
		if (!node)
			exit(1);
		node->node_nr = i;
		node->node_prio = distance;
		vertices[i] = node;
		veb_pq_insert(node, heap); 
	}
	veb_pq_node *node;
	uint decrease_key_calls = 0;
	node = veb_pq_deletemin(heap);
	int m = 0;
	while (node) {
		uint u = node->node_nr;
		for (i = 1; i <= edges[u][0]; i++) {
			uint v = edges[u][i];
			uint alt = distances[u] + weights[u * num_vertices + v];
			if (alt < distances[v] && vertices[v]->parent) {
				veb_pq_decrease_key(heap, vertices[v], distances[v] - alt);
				distances[v] = alt;
				decrease_key_calls++;
			}
		 }
		 node = veb_pq_deletemin(heap);
		 m++;
	}
	free(distances);
	veb_pq_destruct(heap);
	for (i=0; i < num_vertices; i++)
			free(vertices[i]);
	free(vertices);
	return decrease_key_calls;
}