#include <stdlib.h>
#include <stdio.h>
#include <BinaryHeap.h>
#include <FibonacciHeap.h>

unsigned long long test_binary(FILE * testfile);

unsigned long long test_binary(FILE * testfile) {
	if (testfile) {
		//used for getline and strtoul.
		char **line_buf_p = malloc(256 * sizeof(char));
		size_t *line_buf_len = malloc(sizeof(int));
		*line_buf_len = 256;
		char **tailptr;
		
		unsigned int num_vertices, source;
		unsigned int line_buf_i = 0;
		unsigned int int_buf_i = 0;
		unsigned int edges_i = 0;
		
		//note that the use of 'getline' is GNU libs non-standard function.
		//it's used because it makes it significantly easier to read lines
		//reliably
		
		//get the number of vertecies;
		if (getline(line_buf_p, line_buf_len, testfile))
			num_vertices = strtoul(*line_buf_p, NULL, 10);
		else
			exit(-1);
		printf("number of vertices: %d\n", num_vertices);
		//then the source
		if (getline(line_buf_p, line_buf_len, testfile))
			source = strtoul(*line_buf_p, NULL, 10);
		else
			exit(-1);
		printf("source: %d\n", source);
		
		int i, j; //used to index in loops
		
		//create an array with size not known before it's run.
		unsigned int **dist_array = (unsigned int **)malloc(num_vertices * sizeof(unsigned int *));
		dist_array[0] = (unsigned int *)malloc(num_vertices * num_vertices * sizeof(unsigned int));
		for(i = 1; i < num_vertices; i++)
			dist_array[i] = dist_array[0] + i * num_vertices;
		
		//i is the source vertex, j is the destination vertex.
		for (i = 0; i < num_vertices; i++) {
			getline(line_buf_p, line_buf_len, testfile);
			//the first time around tailptr doesn't point to anything
			//after strtoul is called the first time, tailptr will
			//always point to the next char that is not part of a number
			tailptr = line_buf_p;
			for (j = 0; j < num_vertices; j++) {
				dist_array[i][j] = strtoul(*tailptr, tailptr, 10);
				printf("dist[%d][%d] = %d\n", i, j, dist_array[i][j]);
			}
		}
		/*for (i = 0; i < num_vertices; i++) {
			for (j = 0; j < num_vertices; j++) {
				printf("Distance from %d to %d: %d\n", i, j, dist_array[i][j]);
			}
		}*/
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