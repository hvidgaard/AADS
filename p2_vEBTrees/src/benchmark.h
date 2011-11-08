#ifndef _BENCHMARK_H_
#define _BENCHMARK_H_

struct Options {
    uint min;
    uint max;
    uint seed;
    uint start;
    uint end;
    uint step;
    uint repeat;
    char *logfile;
    uint* (*algorithm)(uint *array, uint size);
    char * log_name;
};

void serialBenchmarks(Options opt);

double elapsedTime (struct timeval start, struct timeval stop);

#endif // _BENCHMARK_H_