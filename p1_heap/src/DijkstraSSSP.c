#include <stdlib.h>
#include <stdio.h>
#include <BinaryHeap.h>
#include <FibonacciHeap.h>
#include <BinaryHeap.h>

void dijkstra(unsigned int num_vertices, unsigned int source, unsigned int ** w, unsigned int which);

void dijkstra(unsigned int num_vertices, unsigned int source, unsigned int ** w, unsigned int which){
	unsigned int * dist = malloc(num_vertices * sizeof(unsigned int));
	
	binary_heap * h = bh_init_heap(num_vertices);
	bh_element ** elements = malloc(num_vertices * sizeof(bh_element *));
	bh_element * vp;
	
	dist[source] = 0;
	unsigned int i;
	unsigned int * v;
	v = malloc(sizeof(unsigned int));
	*v = source;
	
	elements[source] = bh_insert(0, v, h);
	for (i = 0; i < source; i++){
		v = malloc(sizeof(unsigned int));
		elements[source] = bh_insert(UINT_MAX, v, h);
	}
	for (i++; i < num_vertices; i++){
		v = malloc(sizeof(unsigned int));
		elements[source] = bh_insert(UINT_MAX, v, h);
	}
	while (h->size > 0) {
		 vp = bh_delete_min(h);
		 
	}
	/*
	unsigned int size;
	if(which == USE_BINARY_HEAP){
		size = heap->bin->size;
		for (i = 1; i <= size; i++){
			//if (i != source && heap->bin->data[i]){
				
				
			
		}
	}
	else{
		size = heap->fib->nodes;
		
	}
	for (i = 0; i < size; i++){
		
	}*/
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