#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include <time.h>
#include <BinaryHeap.h>
#include <FibonacciHeap.h>
#include <DijkstraSSSP.h>

unsigned int * parse_testfile(FILE * testfile, unsigned int * num_vertices, unsigned int * source);

int main(int argc, char **argv)
{
	printf("Commence testing...\n");
	unsigned int * num_vertices = malloc(sizeof(unsigned int));
	unsigned int * source = malloc(sizeof(unsigned int));
	unsigned int * t_edges;
	unsigned int ** edges;
	unsigned int * dist;
	unsigned int n, count;
	
	dist = parse_testfile(fopen(argv[1], "r"), num_vertices, source);
	n = *num_vertices;
	t_edges = malloc(n * sizeof(unsigned int));
	edges = malloc(n * sizeof(unsigned int *));
	if (dist){
		int i, j;
		for (i = 0; i < n; i++){
			count = 0;
			for (j = 0; j < n; j++)
				if (dist[(i * n) + j])
					t_edges[++count] = j;
			
			edges[i] = malloc((count+1) * sizeof(unsigned int));
			edges[i][0] = count;
			for (j = 1; j <= count; j++)
				edges[i][j] = t_edges[j];
		}
		clock_t start;
		if(strcmp(argv[2], "bin") == 0) {
			printf("Timing execution of Dijkstra SSSP with binary heap (%d vertices)\n", n);
			start = clock();
			dijkstra_bin(n, *source, dist, edges);
		} else if(strcmp(argv[2], "fib") == 0) {
			printf("Timing execution of Dijkstra SSSP with fibonacci heap (%d vertices)\n", n);
			start = clock();
			dijkstra_fib(n, *source, dist, edges);
		} else {
			printf("Unknown heap type '%s'", argv[2]);
			exit(2);
		}
		printf("Executed in %g\n", (double) (clock()-start) / (double) CLOCKS_PER_SEC);
	}
	else {
		printf("Failed, testfile could not be opened or was malformed\n");
			exit(3);
	}
	if(argc != 3){
		printf("wrong # of arguments\n");
		exit(1);
	}
}

unsigned int * parse_testfile(FILE * testfile, unsigned int * num_vertices, unsigned int * source) {
	if (testfile) {
		unsigned int n;
		//used for getline and strtoul.
		char *buf = (char *)malloc(n*10 * sizeof(char));
		char *buf_t = buf;
		if (!buf)
			exit(-1);
		char **buf_p = &buf;
		//size_t *line_buf_len = malloc(sizeof(size_t));
		//*line_buf_len = 5000 * 4;
		char **tailptr;
		
		unsigned int line_buf_i = 0;
		unsigned int int_buf_i = 0;
		unsigned int edges_i = 0;
		
		//note that the use of 'getline' is GNU libs non-standard function.
		//it's used because it makes it significantly easier to read lines
		//reliably
		
		//get the number of vertecies;
		if (fgets(*buf_p, n*10, testfile)) {
			*num_vertices = strtoul(*buf_p, NULL, 10);
			n = *num_vertices;
		}
		else {
			//free(line_buf_p);
			//free(line_buf_len);
			return NULL;
		}
		//then the source
		buf = buf_t;
		if (fgets(*buf_p, n*10, testfile))
			*source = strtoul(*buf_p, NULL, 10);
		else {
			//free(line_buf_p);
			//free(line_buf_len);
			return NULL;
		}
		int i, j; //used to index in loops
		
		//create an array with size not known before runtime.
		unsigned int * dist_array = malloc(n * n * sizeof(unsigned int));
		unsigned int value;
		//i is the source vertex, j is the destination vertex.
		for (i = 0; i < n; i++) {
			buf = buf_t;
			fgets(*buf_p, n*10, testfile);
			//the first time around tailptr doesn't point to anything
			//after strtoul is called the first time, tailptr will
			//always point to the next char that is not part of a number
			//i.e. a whitespace or linebreak.
			tailptr = buf_p;
			for (j = 0; j < n; j++) {
				value = strtoul(*tailptr, tailptr, 10);
				dist_array[(i * n) + j] = value;
				//printf("dist[%d][%d] = %d og value er %d og index %d\n", i, j, dist_array[(i*n)+j], value, (i*n)+j);
			}
		}
		/*for (i = 0; i < n; i++) {
			for (j = 0; j < n; j++) {
				printf("dist2[%d][%d] = %d, index: %d\n", i, j, dist_array[i * n + j], i * n + j);
			}
		}*/
		//free(line_buf_p);
		//free(line_buf_len);
		return dist_array;
	}
	else
		return NULL;
}