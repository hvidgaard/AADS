#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <BinaryHeap.h>
#include <FibonacciHeap.h>
#include <PrimitiveQueue.h>
#include <DijkstraSSSP.h>

unsigned int *generate_decrease_key_max(unsigned int vertices){
	int i, j;
	unsigned int *weights = calloc(1+(((2*vertices) * (2* vertices)),sizeof(unsigned int));
	for (i = 0; i < vertices; i++) {
		weights[i+1] = i;
	}
	for (i = 1; i < vertices; i++) {
		for (j = vertices +1; j < vertices*2; j++)
			weights[i * (2*vertices) +j] = 2*i+1;
	}
	return weights;
}

unsigned int *generate_graph(unsigned int vertices, unsigned int edge_chance, unsigned int max_weight, unsigned int seed)
{
	printf("Generating random graph.\n", seed);
	printf("    Random seed:    %8d    Number of vertices: %6d\n", seed, vertices);
	printf("    Chance of edge: %8d%%   Maximum weight:     %6d\n", edge_chance, max_weight);
	srandom(seed);
	int i, j;
	unsigned int *weights = calloc(vertices * vertices,sizeof(unsigned int));
	for(i = 0; i < vertices; i++) {
		printf("\rProgress: %3d%%", (int)round(((double)i/vertices*100)));
		for(j = 0; j < vertices; j++)
			if(j == i+1 || (i != j && random()%101 < edge_chance))
				weights[i * vertices + j] = random()%max_weight+1;
	}
	printf("\rProgress: 100%%\n");
	return weights;
}

unsigned int *dijkstra_bin(unsigned int num_vertices, unsigned int source, unsigned int * weights, unsigned int ** edges)
{
	unsigned int *distances = malloc(num_vertices * sizeof(unsigned int));
	
	binary_heap * heap = bh_init_heap(num_vertices);
	bh_element ** vertices = malloc(num_vertices * sizeof(bh_element *));
	bh_element * sourceNode;
	
	unsigned int distance;
	unsigned int *data;
	unsigned int i;
	for (i = 0; i < num_vertices; i++) {
		if(i == source)
			distance = 0;
		else
			distance = UINT_MAX;
		distances[i] = distance;
		data = malloc(sizeof(unsigned int));
		*data = i;
		//printf("inserting vertex: %i, with key: %d\n", *data, distance);
		vertices[i] = bh_insert(distance, data, heap);
	}
	bh_element *node;
	while (node = bh_find_min(heap)) {
		bh_delete_min(heap);
		unsigned int u = *(unsigned int *)node->data;
		for (i = 1; i <= edges[u][0]; i++) {
			unsigned int v = edges[u][i];
			unsigned int alt = distances[u] + weights[u * num_vertices + v];
			if (alt < distances[v]) {
				bh_decrease_key(distances[v] - alt, vertices[v], heap);
				distances[v] = alt;
			}
		}
	}
	return distances;
}

unsigned int *dijkstra_fib(unsigned int num_vertices, unsigned int source, unsigned int * weights, unsigned int ** edges)
{
	unsigned int *distances = malloc(num_vertices * sizeof(unsigned int));
	
	FibHeap * heap = fib_make_heap();
	FibNode ** vertices = malloc(num_vertices * sizeof(FibNode *));
	FibNode * sourceNode;
	
	unsigned int distance;
	unsigned int *data;
	int i;
	for (i = 0; i < num_vertices; i++) {
		if(i == source)
			distance = 0;
		else
			distance = UINT_MAX;
		distances[i] = distance;
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
			unsigned int alt = distances[u] + weights[u * num_vertices + v];
			if (alt < distances[v]) {
				fib_decrease_key(distances[v] - alt, vertices[v], heap);
				distances[v] = alt;
			}
		 }
	}
	return distances;
}

unsigned int *dijkstra_pq(unsigned int num_vertices, unsigned int source, unsigned int * weights, unsigned int ** edges)
{
	unsigned int *distances = malloc(num_vertices * sizeof(unsigned int));
	
	PrimitiveQueue * queue = pq_make_heap();
	PrimitiveNode ** vertices = malloc(num_vertices * sizeof(PrimitiveNode *));
	PrimitiveNode * sourceNode;
	
	unsigned int distance;
	unsigned int *data;
	int i;
	for (i = 0; i < num_vertices; i++) {
		if(i == source)
			distance = 0;
		else
			distance = UINT_MAX;
		distances[i] = distance;
		data = malloc(sizeof(unsigned int));
		*data = i;
		vertices[i] = pq_insert(distance, data, queue);
	}
	PrimitiveNode *node;
	while (node = pq_find_min(queue)) {
		pq_delete_min(queue);
		unsigned int u = *(unsigned int *)node->data;
		for (i = 1; i <= edges[u][0]; i++) {
			unsigned int v = edges[u][i];
			unsigned int alt = distances[u] + weights[u * num_vertices + v];
			if (alt < distances[v]) {
				pq_decrease_key(distances[v] - alt, vertices[v], queue);
				distances[v] = alt;
			}
		}
	}
	return distances;
}