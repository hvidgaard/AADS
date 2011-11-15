#include <stdlib.h>
#include <stdio.h>
#include "graph_generators.h"

uint *generate_random_graph(uint vertices, uint min_weight, uint max_weight, uint seed) {
	uint edge_chance = 15;
	printf("Generating random graph.\n");
	printf("    Random seed:    %8d    Number of vertices: %6d\n", seed, vertices);
	printf("    Chance of edge: %8d%%   Maximum weight:     %6d\n", edge_chance, max_weight);
	srandom(seed);
	uint i, j;
	uint *weights = calloc(vertices * vertices,sizeof(uint));
	for(i = 0; i < vertices; i++) {
		//printf("\rProgress: %3d%%", (int)round(((double)i/vertices*100)));
		for(j = 0; j < vertices; j++)
			if(j == i+1 || (i != j && random()%101 < edge_chance))
				weights[i * vertices + j] = random()%max_weight+1;
	}
	printf("\rProgress: 100%%\n");
	return weights;
}

uint *generate_decrease_key_max_graph(uint vertices, uint min_weight, uint max_weight, uint seed) {
	if (!(vertices % 2)) {
		printf("must be an odd number of vertices");
		exit(0);
	}
	printf("Generating graph for forcing a many decrease key calls.\n");
	printf("    Number of vertices: %6d\n", vertices);
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
	/*for (i = 0; i < vertices; i++){
		for (j = 0; j < vertices; j++)
			printf("%2d ", weights[i*vertices + j]);
		printf("\n");
	}*/

	return weights;
}

uint *generate_decrease_key_max_graph_2(uint vertices, uint min_weight, uint max_weight, uint seed) {
	uint i, j, n;
	n = vertices;
	uint *weights = calloc((vertices * vertices),sizeof(uint));
	uint max;
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
	//for (i = 0; i < n; i++)
	//	weights[i*n+i] = 0;
	
	if (n < 12){
	for (i = 0; i < vertices; i++){
		for (j = 0; j < vertices; j++)
			printf("%4d ", weights[i*vertices + j]);
		printf("\n");
	}
	}
	
	return weights;
}