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

void log_results(char *filename, char *graphtype, unsigned int vertices, unsigned int dk_calls, double running_time);

int main(int argc, char **argv)
{
	if(argc < 4) {
		printf("Usage\n");
		printf("    test random <heap> <vertices> [seed] [edgechance] [maxweight] [source]\n");
		printf("    test dkmax <heap> <vertices>\n");
		printf("        Heaptypes:  bin, fib, pq\n");
		exit(1);
	}
	
	char *heap_name = malloc(10*sizeof(char));
	unsigned int (*dijkstra)(unsigned int num_vertices, unsigned int source, unsigned int * w, unsigned int ** edges, unsigned int *bops);
	if(strcmp(argv[2], "bin") == 0) {
		heap_name = "Binary";
		dijkstra = dijkstra_bin;
	} else if(strcmp(argv[2], "fib") == 0) {
		heap_name = "Fibonacci";
		dijkstra = dijkstra_fib;
	} else if(strcmp(argv[2], "pq") == 0) {
		heap_name = "Primitive";
		dijkstra = dijkstra_pq;
	} else {
		printf("Unknown heap type '%s'\n", argv[2]);
		exit(2);
	}
	
	unsigned int vertices = (unsigned int)strtoul(argv[3], NULL, 10);
	unsigned int source = 0;
	unsigned int *weights;
	unsigned int **edges = malloc(vertices * sizeof(unsigned int *));
	if(strcmp(argv[1], "random") == 0) {
		unsigned int seed;
		if(argc > 4 && argv[4]) {
			seed = (unsigned int)strtoul(argv[4], NULL, 10);
			srandom(seed);
		} else {
			srandom(time(NULL));
			seed = random()%99999999;
		}
		
		unsigned int edge_chance = 15;
		if(argc > 5)
			edge_chance = (unsigned int)strtoul(argv[5], NULL, 10);
		
		unsigned int max_weight = 20;
		if(argc > 6)
			max_weight = (unsigned int)strtoul(argv[6], NULL, 10);
		
		source = random()%vertices;
		if(argc > 7)
			source = (unsigned int)strtoul(argv[7], NULL, 10);
		
		weights = generate_graph(vertices, edge_chance, max_weight, seed);
		
		printf("Reticulating splines.\n");
		unsigned int *t_edges = malloc(vertices * sizeof(unsigned int));
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
		free(t_edges);
	} else if(strcmp(argv[1], "dkmax") == 0) {
		weights = generate_decrease_key_max(vertices);
		
		printf("Reticulating splines.\n");
		unsigned int *t_edges = malloc(vertices * sizeof(unsigned int));
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
		//free(t_edges);
	} else if (strcmp(argv[1], "dkmax2") == 0){
		weights = generate_decrease_key_max2(vertices);

		printf("Reticulating splines.\n");
		unsigned int *t_edges = malloc(vertices * sizeof(unsigned int));
		edges = malloc(vertices * sizeof(unsigned int *));
		int i, j;
		for (i = 0; i < vertices; i++) {
			unsigned int count = 0;
		for (j = 0; j < vertices; j++){
			if (weights[(i * vertices) + j])
				t_edges[++count] = j;
		}

		edges[i] = malloc((count+1) * sizeof(unsigned int));
		edges[i][0] = count;
		for (j = 1; j <= count; j++)
			edges[i][j] = t_edges[j];
	}
	//free(t_edges);
	}
	else {
		printf("Unknown graph algorithm '%s'\n", argv[1]);
		exit(3);
	}
	
	clock_t start;
	clock_t end;
	unsigned int decrease_key_calls;
	printf("Calculating distances.\n");
	printf("    Heap: %10s   Source:         %8d\n", heap_name, source);
	start = clock();
	decrease_key_calls = dijkstra(vertices, source, weights, edges, NULL);
	end = clock();
	double running_time = (double) (end-start) / (double) CLOCKS_PER_SEC;
	printf("    Time: %10gs  dec. key calls: %8d\n", running_time, decrease_key_calls);
	log_results(heap_name, argv[1], vertices, decrease_key_calls, running_time);
}

void log_results(char *filename, char *graphtype, unsigned int vertices, unsigned int dk_calls, double running_time) {
	FILE *handle = fopen(filename, "a+");
	fprintf(handle, "%s\t%d\t%d\t%10g\n", graphtype, vertices, dk_calls, running_time);
	fclose(handle);
	printf("Results have been logged to %s\n", filename);
}
