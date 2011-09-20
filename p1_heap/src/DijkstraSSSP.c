#include <stdlib.h>
#include <stdio.h>
#include <BinaryHeap.h>
#include <FibonacciHeap.h>
#include <AbstractHeap.h>

void dijkstra(unsigned int num_vertices, unsigned int source, unsigned int ** w, unsigned int which);

void dijkstra(unsigned int num_vertices, unsigned int source, unsigned int ** w, unsigned int which){
	unsigned int * dist = malloc(num_vertices * sizeof(unsigned int));
	
	AbstractHeap * heap;
	if(which == USE_BINARY_HEAP)
		heap = make_heap(USE_BINARY_HEAP, num_vertices);
	else
		heap = make_heap(USE_FIBONACCI_HEAP, num_vertices);
	
	dist[source] = 0;
	unsigned int i;
	for (i = 0; i < num_vertices; i++){
		if (i != source) {
			unsigned int * v = malloc(sizeof(unsigned int));
			*v = i;
			insert(UINT_MAX, v, heap);
		}
	}
	for (i = 0; i < num_vertices; i++){
		
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