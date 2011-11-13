#include <cstdlib>
#include <ctime>
#include <sys/time.h>
#include <iostream>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <sys/types.h>

#include "benchmark.h"

using namespace std;

int main(int argc, char **argv) {
	if(argc < 7) {
		cout << "Usage" << endl
			<< "  benchmark algo seed start end step tests logfile" << endl;
		exit(1);
	}

	Options opt;
	opt.min = 0;
	opt.max = 0xffffffff;
	opt.log_name = argv[1];
	opt.seed = (uint) strtoul(argv[2], NULL, 10);
	opt.start = (uint) strtoul(argv[3], NULL, 10);
	opt.end = (uint) strtoul(argv[4], NULL, 10);
	opt.step = (uint) strtoul(argv[5], NULL, 10);
	opt.repeat = (uint) strtoul(argv[6], NULL, 10);
	opt.logfile = argv[7];

	if(strcmp(opt.log_name, "veb") == 0) {
		// opt.algorithm = test_vanEmdeBoasTree;
	} else if(strcmp(opt.log_name, "bin") == 0) {
		// opt.algorithm = test_BinaryHeap;
	} else if(strcmp(opt.log_name, "fib") == 0) {
		// opt.algorithm = test_FibonacciHeap;
	} else {
		printf("Unknown algorithm '%s'\n", opt.log_name);
		exit(2);
	}
	serialBenchmarks(opt);
	return 0;
}

void serialBenchmarks (Options opt) {
	struct timeval start, end;

	FILE *logfilehandle = fopen(opt.logfile, "a+");
	if(!logfilehandle) {
		cout << "Could not open the logfile." <<endl;
		exit(2);
	}
	uint size;
	uint i;
	uint total = ((opt.end-opt.start)/opt.step+1)*opt.repeat;
	uint progress = 0;
    
    uint *tree;
	for (size = opt.start ; size <= opt.end ; size += opt.step) {
		for (i = opt.repeat ; i > 0 ; i--) {
			// tree = generate_tree(size, opt.min, opt.max, opt.seed++);
			gettimeofday(&start, NULL);
			opt.algorithm(tree, size);
			gettimeofday(&end, NULL);
			if(!(size == opt.start && i == opt.repeat))
				fprintf(logfilehandle, "\n%s\t%d\t%10.10f", opt.log_name, size, elapsedTime(start, end));
			delete[] tree;
			progress = ((size-opt.start)/opt.step+1)*opt.repeat-i;
			if((progress*100/total) % 10 == 0 && progress*100/total == (float)progress*100/total)
				cout << progress*100/total << "%" << endl;
		}
	}
	if(progress*100/total % 10 != 0)
		cout << "100%" << endl;
	fclose(logfilehandle);
}

double elapsedTime (struct timeval start, struct timeval stop) {
	return (stop.tv_sec*1000.0 + (stop.tv_usec/1000.0)) - (start.tv_sec*1000.0 + (start.tv_usec/1000.0));
}
