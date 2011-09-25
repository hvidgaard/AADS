#include <stdlib.h>
#include <stdio.h>
#include <BinaryHeap.h>
#include <FibonacciHeap.h>
#include <BinaryHeap.h>

unsigned int *dijkstra_bin(unsigned int num_vertices, unsigned int source, unsigned int * w, unsigned int ** edges){
	unsigned int * dist = malloc(num_vertices * sizeof(unsigned int));
	
	binary_heap * heap = bh_init_heap(num_vertices);
	bh_element ** vertices = malloc(num_vertices * sizeof(bh_element *));
	bh_element * sourceNode;
	
	unsigned int distance;
	unsigned int *data;
	int i;
	for (i = 0; i < num_vertices; i++){
		if(i == source)
			distance = 0;
		else
			distance = UINT_MAX;
		dist[i] = distance;
		data = malloc(sizeof(unsigned int));
		*data = i;
		vertices[i] = bh_insert(distance, data, heap);
	}
	bh_element *node;
	while (node = bh_find_min(heap)) {
		bh_delete_min(heap);
		unsigned int u = *(unsigned int *)node->data;
		for (i = 1; i <= edges[u][0]; i++) {
			 unsigned int v = edges[u][i];
			 unsigned int alt = dist[u] + w[u * num_vertices + v];
			 if (alt < dist[v]) {
				 bh_decrease_key(dist[v] - alt, vertices[v], heap);
				 dist[v] = alt;
			 }
		 }
	}
	return dist;
}

unsigned int *dijkstra_fib(unsigned int num_vertices, unsigned int source, unsigned int * w, unsigned int ** edges)
{
	unsigned int * dist = malloc(num_vertices * sizeof(unsigned int));
	
	FibHeap * heap = fib_make_heap();
	FibNode ** vertices = malloc(num_vertices * sizeof(FibNode *));
	FibNode * sourceNode;
	
	unsigned int distance;
	unsigned int *data;
	int i;
	for (i = 0; i < num_vertices; i++){
		if(i == source)
			distance = 0;
		else
			distance = UINT_MAX;
		dist[i] = distance;
		data = malloc(sizeof(unsigned int));
		*data = i;
		vertices[i] = fib_insert(distance, data, heap);
	}
	FibNode *node;
	while (node = fib_find_min(heap)) {
		fib_delete_min(heap);
		unsigned int u = *(unsigned int *)node->data;
		for (i = 1; i <= edges[u][0]; i++) {
			 unsigned int v = edges[u][i];
			 unsigned int alt = dist[u] + w[u * num_vertices + v];
			 if (alt < dist[v]) {
				 fib_decrease_key(dist[v] - alt, vertices[v], heap);
				 dist[v] = alt;
			 }
		 }
	}
	return dist;
}
