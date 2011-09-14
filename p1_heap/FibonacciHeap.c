#include <AADS.h>
#include <stdlib.h>
#include <stdio.h>

FibHeap *fib_init_heap(unsigned int size);
unsigned int fib_insert(unsigned int key, void *data, FibHeap *h);
unsigned int fib_find_min(FibHeap *h);
Element *fib_delete_min(FibHeap *h);
FibHeap *fib_meld(FibHeap *h1, FibHeap *h2);
unsigned int fib_decrease_key(unsigned int new_key, unsigned int e, FibHeap *h);
Element *fib_delete_element(unsigned int e, FibHeap *h);
void fib_min_heapify(unsigned int e, FibHeap *h);
void fib_exchange(unsigned int e1, unsigned int e2, FibHeap *h);



FibHeap *fib_init_heap(unsigned int size) {
	FibHeap *heap = malloc(sizeof(struct FibHeap));
	heap->max_size = size;
	heap->size = 0;
	heap->data = calloc(size+1, sizeof(struct FibElement));
	return heap;
}