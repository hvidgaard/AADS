#ifndef BENCHMARK_H
#define BENCHMARK_H

struct Options {
    uint source;
    uint min;
    uint max;
    uint seed;
    uint start;
    uint end;
    uint step;
    uint repeat;
    char *logfile;
    uint* (*generate_graph)(uint size, uint min, uint max, uint seed);
    uint (*dijkstra)(uint num_vertices, uint source, uint * weights, uint ** edges);
    char * log_algo;
    char * log_graph;
};

void serialBenchmarks(Options opt);

double elapsedTime (struct timeval start, struct timeval stop);

#endif