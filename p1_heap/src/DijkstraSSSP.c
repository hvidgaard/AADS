#include <stdlib.h>
#include <stdio.h>
#include <BinaryHeap.h>
#include <FibonacciHeap.h>
#include <BinaryHeap.h>

void dijkstra(unsigned int num_vertices, unsigned int source, unsigned int ** w, unsigned int which, unsigned int ** edges);

void dijkstra(unsigned int num_vertices, unsigned int source, unsigned int ** w, unsigned int which, unsigned int ** edges){
	unsigned int * dist = malloc(num_vertices * sizeof(unsigned int));
	
	binary_heap * h = bh_init_heap(num_vertices);
	//bh_element ** elements = malloc(num_vertices * sizeof(bh_element *));
	bh_element * v;
	
	dist[source] = 0;
	unsigned int i;
	unsigned int * v_data;
	v_data = malloc(sizeof(unsigned int));
	*v_data = source;
	
	//elements[source] = bh_insert(0, v, h);
	bh_insert(0, v, h);
	for (i = 0; i < source; i++){
		v_data = malloc(sizeof(unsigned int));
		//elements[v_index] = bh_insert(UINT_MAX, v_index, h);
		bh_insert(UINT_MAX, v_data, h);
	}
	for (i++; i < num_vertices; i++){
		v_data = malloc(sizeof(unsigned int));
		//elements[v_data] = bh_insert(UINT_MAX, v_data, h);
		bh_insert(UINT_MAX, v_data, h);
	}
	
	while (h->size > 0) {
		 v = bh_delete_min(h);
		 for (i = 1; i < edges[v->key][0]; i++){
			 
		 }
	}
}
/*
while Q ≠ ∅ do
v := DeleteMin(Q)
foreach u : (v, u) ∈ E do
if u ∈ Q and dist[v]+w(v, u) < dist[u] then
[ ] ( , )
[ ]
dist[u] := dist[v]+w(v, u)
DecreaseKey(u, dist[u])
*/