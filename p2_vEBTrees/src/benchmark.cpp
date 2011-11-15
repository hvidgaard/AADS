#include <cstdlib>
#include <ctime>
#include <sys/time.h>
#include <iostream>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <sys/types.h>

#include "benchmark.h"
extern "C" {
	#include "graph_generators.h"
	#include "dijkstra.h"
}

using namespace std;

int main(int argc, char **argv) {
	if(argc < 8) {
		cout << "Usage" << endl
			<< "  benchmark graph algo seed start end step tests logfile" << endl;
		exit(1);
	}

	Options opt;
	opt.max = 4294967295;
	opt.log_graph = argv[1];
	opt.log_algo = argv[2];
	opt.seed = (uint) strtoul(argv[3], NULL, 10);
	opt.start = (uint) strtoul(argv[4], NULL, 10);
	opt.end = (uint) strtoul(argv[5], NULL, 10);
	opt.step = (uint) strtoul(argv[6], NULL, 10);
	opt.repeat = (uint) strtoul(argv[7], NULL, 10);
	opt.logfile = argv[8];
	
	if(strcmp(opt.log_graph, "random") == 0) {
		opt.generate_graph = generate_random_graph;
	} else if(strcmp(opt.log_graph, "dkmax") == 0) {
		opt.generate_graph = generate_decrease_key_max_graph;
	} else if(strcmp(opt.log_graph, "dkmax2") == 0) {
		opt.generate_graph = generate_decrease_key_max_graph_2;
	} else {
		printf("Unknown graph generator '%s'\n", opt.log_graph);
		exit(2);
	}
	
	if(strcmp(opt.log_algo, "veb") == 0) {
		// opt.dijkstra = dijkstra_veb;
	} else if(strcmp(opt.log_algo, "rb") == 0) {
		// opt.dijkstra = dijkstra_rb;
	} else if(strcmp(opt.log_algo, "bin") == 0) {
		opt.dijkstra = dijkstra_bin;
	} else if(strcmp(opt.log_algo, "fib") == 0) {
		opt.dijkstra = dijkstra_fib;
	} else {
		printf("Unknown priority queue '%s'\n", opt.log_algo);
		exit(2);
	}
	serialBenchmarks(opt);
	return 0;
}

void serialBenchmarks (Options opt) {

	FILE *logfilehandle = fopen(opt.logfile, "a+");
	if(!logfilehandle) {
		cout << "Could not open the logfile." <<endl;
		exit(2);
	}
	uint size;
	uint remaining;
	uint total = ((opt.end-opt.start)/opt.step+1)*opt.repeat;
	uint progress = 0;
	uint previous_progress = 0;
	
	uint* weights;
	for (size = opt.start ; size <= opt.end ; size += opt.step) {
		opt.seed++;
		for (remaining = opt.repeat ; remaining > 0 ; remaining--) {
			weights = opt.generate_graph(size, opt.max, opt.seed);
			
			uint** edges = (uint**) malloc((size+1) * sizeof(uint*));
			uint *t_edges = (uint*) malloc(size * sizeof(uint));
			uint i, j;
			for (i = 0; i < size; i++) {
				uint count = 0;
				for (j = 0; j < size; j++)
					if (weights[(i * size) + j])
						t_edges[++count] = j;
				
				edges[i] = (uint*) malloc((count+1) * sizeof(uint));
				edges[i][0] = count;
				for (j = 1; j <= count; j++)
					edges[i][j] = t_edges[j];
			}
			delete[] t_edges;
			
			struct timeval start_time, end_time;
			clock_t start_clock, end_clock;
			gettimeofday(&start_time, NULL);
			start_clock = clock();
			opt.dijkstra(size, opt.source, weights, edges);
			end_clock = clock();
			gettimeofday(&end_time, NULL);
			double cyc_running_time = (double) (end_clock-start_clock) / (double) CLOCKS_PER_SEC;
			double abs_running_time = elapsedTime(start_time, end_time);
			
			delete[] edges;
			delete[] weights;
			
			fprintf(logfilehandle, "\n%s\t%s\t%d\t%10.10f\t%10.10f",
				opt.log_algo, opt.log_graph, size,
				cyc_running_time, abs_running_time);
			
			progress = (((size-opt.start)/opt.step+1)*opt.repeat-remaining)*100/total;
			if(progress > previous_progress)
				cout << progress << "%" << endl;
			previous_progress = progress;
		}
	}
	if(previous_progress != 100)
		cout << "100%" << endl;
	fclose(logfilehandle);
}

double elapsedTime (struct timeval start, struct timeval stop) {
	return (stop.tv_sec*1000.0 + (stop.tv_usec/1000.0)) - (start.tv_sec*1000.0 + (start.tv_usec/1000.0));
}
