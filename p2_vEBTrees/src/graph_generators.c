#include <stdlib.h>
#include "graph_generators.h"
#include "debug.h"

#ifdef PRINT
#include <stdio.h>
#endif

uint *generate_random_graph(uint vertices, uint max_weight, uint seed) {
	uint edge_chance = 15;
	srandom(seed);
	uint i, j;
	uint *weights = calloc(vertices * vertices,sizeof(uint));
	#ifdef PRINT
	printf("vertice: %d\n", vertices);
	#endif
	for(i = 0; i < vertices; i++) {
		for(j = 0; j < vertices; j++)
			if(j == i+1 || (i != j && random()%101 < edge_chance)){
				weights[i * vertices + j] = random()%max_weight+1;
				#ifdef PRINT
				printf("edge from %d to %d : %d\n", i, j, weights[i * vertices + j]);
				#endif
			}
	}
	return weights;
}

uint *generate_decrease_key_max_graph(uint vertices, uint max_weight, uint seed) {
	if (!(vertices % 2))
		vertices++;
	uint num = vertices / 2;
	uint i, j;
	uint *weights = calloc(vertices * vertices,sizeof(uint));
	for (i = 0; i < num; i++) {
		weights[i+1] = i+1;
	}
	for (i = 1; i < num+1; i++) {
		for (j = num +1; j < vertices; j++)
			weights[i * vertices +j] = 2*(num-i+1)+1;
	}

	return weights;
}

uint *generate_decrease_key_max_graph_2(uint vertices, uint max_weight, uint seed) {
	uint i, j, n;
	n = vertices;
	uint *weights = calloc((vertices * vertices),sizeof(uint));
	uint max = n - 1;
	for (j = 1; j < vertices; j++) {
		weights[j] = (vertices*vertices) - j;
	}
	weights[n-1] = 1;
	for (i = n - 1; i > 1; i--){
		for (j = 1; j < i; j++){
			if (i != j)
				weights[i * n + j] = n*n - ((n - i) * n) - j + 1;
		}
		weights[i*n+max-1] = 1;
		max--;
	}
	
	return weights;
}