#include <stdlib.h>
#include <stdio.h>
#include <BinaryHeap.h>
#include <FibonacciHeap.h>

unsigned long long test_binary(FILE * testfile);

unsigned long long test_binary(FILE * testfile) {
	if (testfile) {
		//reading the lines 256 chars at a time, it may be longer
		//but that's okay, we'll handle that special case later.
		char *line_buf = malloc(256 * sizeof(char));
		char **line_buf_p = &line_buf;
		size_t *line_buf_len = malloc(sizeof(int));
		*line_buf_len = 256;
		char **tailptr;
		
		unsigned int num_vertices, source, len, value;
		unsigned int line_buf_i = 0;
		unsigned int int_buf_i = 0;
		unsigned int edges_i = 0;
		
		//note that the use of 'getline' is GNU libs non-standard function.
		//it's used because it makes it significantly easier to read lines
		//reliably
		
		
		//get the number of vertecies;
		len = getline(line_buf_p, line_buf_len, testfile);
		if (len)
			num_vertices = strtoul(*line_buf_p, NULL, 10);
		else
			exit(-1);
		//then the source
		len = getline(line_buf_p, line_buf_len, testfile);
		if (len)
			source = strtoul(*line_buf_p, NULL, 10);
		else
			exit(-1);
		//unsigned int dist[] = malloc(num_vertices * num_vertices * sizeof(unsigned int));
		
		int i;
		unsigned int **dist_array = (unsigned int **)malloc(num_vertices * sizeof(unsigned int *));
		dist_array[0] = (unsigned int *)malloc(num_vertices * num_vertices * sizeof(unsigned int));
		for(i = 1; i < num_vertices; i++)
			dist_array[i] = dist_array[0] + i * num_vertices;
		
		i = 0; //the vertex we're reading distances from
		int j = 0; //the vertex we're reading distance to
		while (len = getline(line_buf_p, line_buf_len, testfile)) {
			do {
				dist_array[i][j] = strtoul(*tailptr, tailptr, 10);
				j++;
			}
			while (**tailptr != '\0');
			i++;
		}
		for (i = 0; i < num_vertices; i++) {
			for (j = 0; j < num_vertices; j++) {
				printf("Distance from %d to %d: %d\n", i, j, dist_array[i][j]);
			}
		}
			/*
		int_buf[int_buf_i] = '\0';
		num_vertices = (unsigned int) strtoul(int_buf, NULL, 10);
		unsigned int *edges = malloc(sizeof(unsigned int)*num_vertices*num_vertices);
		line_buf_i = 0;
		while (vertex < num_vertices){
			fgets(line_buf, 256, testfile);
			while (line_buf_i < 256 && line_buf[line_buf_i] != '\0'){
				int_buf[int_buf_i] = line_buf[line_buf_i];
				if (int_buf[int_buf_i] == ' ') {
					int_buf[int_buf_i] = '\0';
					edges[edges_i] = num_vertices = (unsigned int) strtoul(int_buf, NULL, 10);
					int_buf_i = 0;
				}
				line_buf_i++;
				if (line_buf_i == 256){
					fgets(line_buf, 256, testfile);
					line_buf_i = 0;
				}
			}
		}
		/*do {
			fgets(line_buf, 256, testfile);
			while (i1 < 256 && line_buf[i1] != '\0'){
				int_buf[i2] = line_buf[i1];
				i2++;
			}
			if (i1)
		}
		while (!eof_reached);*/
	}
	else
		printf("testfile null\n");
   return 0;
}
/*
Algorithm Dijkstra(V, E, w, s)
Q := MakeQueue
dist[s] := 0
Insert(Q, s, 0)
for v ∈ V \ { s } do
{ s } do 
dist[v] := +∞
Insert(Q, v, +∞)
while Q ≠ ∅ do
v := DeleteMin(Q)
foreach u : (v, u) ∈ E do
if u ∈ Q and dist[v]+w(v, u) < dist[u] then
[ ] ( , )
[ ]
dist[u] := dist[v]+w(v, u)
DecreaseKey(u, dist[u])
*/