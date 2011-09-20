#include <stdlib.h>
#include <stdio.h>
#include <BinaryHeap.h>
#include <FibonacciHeap.h>
#include <AbstractHeap.h>
#include <DijkstraSSSP.h>

int test_abstract();
int test_binary();
int test_fibonacci();
unsigned int ** parse_testfile(FILE * testfile, unsigned int * num_vertices, unsigned int * source);

int main(int argc, char **argv)
{
	printf("Commence testing...\n");
	unsigned int * num_vertices = malloc(sizeof(unsigned int));
	unsigned int * source = malloc(sizeof(unsigned int));
	unsigned int ** dist;
	
	dist = parse_testfile(fopen("./../testdata/test1.txt", "r"), num_vertices, source);
	printf("#vertices: %d\nsource: %d\n\n", *num_vertices, *source);
	int i, j;
	for (i = 0; i < *num_vertices; i++){
		for (j = 0; j < *num_vertices; j++)
			printf("Dist[%d][%d]: %d\n", i, j, dist[i][j]);
	}
	/*if(argc != 2)
		exit(1);
	
	if(strcmp(argv[1], "abstract") == 0) {
		exit(test_abstract());
	} else if(strcmp(argv[1], "bin") == 0) {
		exit(test_binary());
	} else if(strcmp(argv[1], "fib") == 0) {
		exit(test_fibonacci());
	}*/
}

int test_abstract() {
	AbstractHeap *heap = make_heap(USE_FIBONACCI_HEAP, 30);
	int i;
	for (i = 1; i <= 16; i++)
		insert(17-i, NULL, heap);
	
	AbstractNode *twenty = insert(21, NULL, heap);
	decrease_key(15, twenty, heap);

	AbstractNode *min = (AbstractNode *) find_min(heap);
	while (min) {
		printf("min is %d\n", min->key);
		delete_min(heap);
		min = find_min(heap);
	}
	return EXIT_SUCCESS;
}

int test_binary()
{
	printf("hello world\n");
	binary_heap *h = bh_init_heap(16);
	int i;
	for (i = 1; i <= 16; i++)
		bh_insert(17-i, NULL, h);
	i = 0;
	while (i < h->size) {
		printf("element %d value %d\n", i, h->data[i].key);
		fflush(stdout);
		i++;
	}
	printf("\n");
	bh_element *e;
	while (h->size != 0){
		printf("heap size: %d - deleting min\n", h->size);
		e = bh_delete_min(h);
		printf("got: %d\n\n", e->key);
		printf("heap size is now: %d\n", h->size);
		fflush(stdout);
		free(e);
	}

	fflush(stdout);
	return EXIT_SUCCESS;
}

int test_fibonacci()
{
	FibHeap *heap = fib_make_heap(30);
	int i;
	for (i = 1; i <= 16; i++)
		fib_insert(17-i, NULL, heap);
	
	FibNode *twenty = fib_insert(21, NULL, heap);
	fib_decrease_key(15, twenty, heap);

	FibNode *min = (FibNode *) fib_find_min(heap);
	while (min) {
		printf("min is %d\n", min->key);
		fib_delete_min(heap);
		min = fib_find_min(heap);
	}
	return EXIT_SUCCESS;
}

unsigned int ** parse_testfile(FILE * testfile, unsigned int * num_vertices, unsigned int * source) {
	if (testfile) {
		//used for getline and strtoul.
		char **line_buf_p = malloc(256 * sizeof(char));
		size_t *line_buf_len = malloc(sizeof(int));
		*line_buf_len = 256;
		char **tailptr;
		
		unsigned int line_buf_i = 0;
		unsigned int int_buf_i = 0;
		unsigned int edges_i = 0;
		
		//note that the use of 'getline' is GNU libs non-standard function.
		//it's used because it makes it significantly easier to read lines
		//reliably
		
		//get the number of vertecies;
		if (getline(line_buf_p, line_buf_len, testfile))
			*num_vertices = strtoul(*line_buf_p, NULL, 10);
		else {
			free(line_buf_p);
			free(line_buf_len);
			return NULL;
		}
		//then the source
		if (getline(line_buf_p, line_buf_len, testfile))
			*source = strtoul(*line_buf_p, NULL, 10);
		else {
			free(line_buf_p);
			free(line_buf_len);
			return NULL;
		}
		
		int i, j; //used to index in loops
		
		//create an array with size not known before runtime.
		unsigned int **dist_array = (unsigned int **)malloc((*num_vertices) * sizeof(unsigned int *));
		dist_array[0] = (unsigned int *)malloc((*num_vertices) * (*num_vertices) * sizeof(unsigned int));
		for(i = 1; i < *num_vertices; i++)
			dist_array[i] = dist_array[0] + i * (*num_vertices);
		
		//i is the source vertex, j is the destination vertex.
		for (i = 0; i < *num_vertices; i++) {
			getline(line_buf_p, line_buf_len, testfile);
			//the first time around tailptr doesn't point to anything
			//after strtoul is called the first time, tailptr will
			//always point to the next char that is not part of a number
			//i.e. a whitespace or linebreak.
			tailptr = line_buf_p;
			for (j = 0; j < *num_vertices; j++)
				dist_array[i][j] = strtoul(*tailptr, tailptr, 10);
		}
		free(line_buf_p);
		free(line_buf_len);
		return dist_array;
	}
	else
		return NULL;
   ;
}