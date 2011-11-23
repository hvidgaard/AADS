#ifndef BENCHMARK_H
#define BENCHMARK_H

struct Options {
	uint source;
	uint max;
	uint seed;
	uint start;
	uint end;
	uint step;
	uint repeat;
	char* logfile;
	uint* (*generate_graph)(uint size, uint max, uint seed);
	uint (*dijkstra)(uint num_vertices, uint source, uint * weights, uint ** edges);
	uint* (*generate_list)(uint size, uint max, uint seed);
	void (*sort)(uint size, uint* list);
	char* log_algo;
	char* log_graph;
};

void serialGraphBenchmarks(Options opt);
void serialListBenchmarks(Options opt);

double elapsedTime (struct timeval start, struct timeval stop);

#endif