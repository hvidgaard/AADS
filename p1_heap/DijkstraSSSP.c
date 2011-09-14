#include <AADS.h>
#include <stdlib.h>
#include <stdio.h>

/*
Binary_heap *bh_init_heap(unsigned int size);
unsigned int bh_insert(unsigned int key, void *data, Binary_heap *h);
unsigned int bh_find_min(Binary_heap *h);
Element *bh_delete_min(Binary_heap *h);
Binary_heap *bh_meld(Binary_heap *h1, Binary_heap *h2);
unsigned int bh_decrease_key(unsigned int new_key, unsigned int e, Binary_heap *h);
Element *bh_delete_element(unsigned int e, Binary_heap *h);
void bh_min_heapify(unsigned int e, Binary_heap *h);
void bh_exchange(unsigned int e1, unsigned int e2, Binary_heap *h);
 */

unsigned long long test_binary(FILE * testfile);
unsigned long long test_fib(FILE * testfile);

unsigned long long test_binary(FILE * testfile){
	unsigned int size = 0;
	unsigned int source = 0;
	unsigned int dist[size];
	
	if (testfile != NULL){
		char line_buf[256];
		char int_buf[10];
		unsigned int num_vertices;
		unsigned int line_buf_i = 0;
		unsigned int int_buf_i = 0;
		unsigned int edges_i = 0;
		unsigned int vertex = 1;
		fgets(line_buf, 256, testfile);
		while (line_buf_i < 256 && line_buf[line_buf_i] != '\0'){
			int_buf[int_buf_i] = line_buf[line_buf_i];
			line_buf_i++;
			int_buf_i++;
		}
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
   return 0;
	
	Binary_heap *h = bh_init_heap(size);
	dist[source] = 0;
	bh_insert(0, source, h);
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