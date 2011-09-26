#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include <time.h>
#include <string.h>
#include <BinaryHeap.h>
#include <FibonacciHeap.h>
#include <DijkstraSSSP.h>

int main(int argc, char **argv)
{
	if(argc == 1) {
		printf("Usage\n    test heap [seed] [vertices] [edgechance] [maxweight] [source]\n");
		exit(1);
	}
	
	unsigned int seed;
	if(argc > 2 && argv[2]) {
		seed = (unsigned int)strtoul(argv[2], NULL, 10);
		srandom(seed);
	} else {
		srandom(time(NULL));
		seed = random()%99999999;
	}
	
	unsigned int vertices = 20;
	if(argc > 3)
		vertices = (unsigned int)strtoul(argv[3], NULL, 10);
	
	unsigned int edge_chance = 15;
	if(argc > 4)
		edge_chance = (unsigned int)strtoul(argv[4], NULL, 10);
	
	unsigned int max_weight = 20;
	if(argc > 5)
		max_weight = (unsigned int)strtoul(argv[5], NULL, 10);
	
	unsigned int source = random()%vertices;
	if(argc > 6)
		source = (unsigned int)strtoul(argv[6], NULL, 10);
		
	unsigned int *weights = generate_graph(vertices, edge_chance, max_weight, seed);
	// unsigned int *weights = generate_decrease_key_max(vertices);
	
	
	printf("Reticulating splines.\n");
	unsigned int *t_edges = malloc(vertices * sizeof(unsigned int));
	unsigned int **edges = malloc(vertices * sizeof(unsigned int *));
	int i, j;
	for (i = 0; i < vertices; i++) {
		unsigned int count = 0;
		for (j = 0; j < vertices; j++)
			if (weights[(i * vertices) + j])
				t_edges[++count] = j;
		
		edges[i] = malloc((count+1) * sizeof(unsigned int));
		edges[i][0] = count;
		for (j = 1; j <= count; j++)
			edges[i][j] = t_edges[j];
	}
	
	char *heap_name = malloc(10*sizeof(char));
	unsigned int (*dijkstra)(unsigned int num_vertices, unsigned int source, unsigned int * w, unsigned int ** edges);
	if(strcmp(argv[1], "bin") == 0) {
		heap_name = "Binary";
		dijkstra = dijkstra_bin;
	} else if(strcmp(argv[1], "fib") == 0) {
		heap_name = "Fibonacci";
		dijkstra = dijkstra_fib;
	} else if(strcmp(argv[1], "pq") == 0) {
		heap_name = "Primitive";
		dijkstra = dijkstra_pq;
	} else {
		printf("Unknown heap type '%s'", argv[2]);
		exit(2);
	}
	
	clock_t start;
	clock_t end;
	unsigned int decrease_key_calls;
	printf("Calculating distances.\n");
	printf("    Heap: %10s   Source:         %8d\n", heap_name, source);
	start = clock();
	decrease_key_calls = dijkstra(vertices, source, weights, edges);
	end = clock();
	double running_time = (double) (end-start) / (double) CLOCKS_PER_SEC;
	printf("    Time: %10gs  dec. key calls: %8d\n", running_time, decrease_key_calls);
}