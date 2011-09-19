#ifndef ABSTRACTHEAP_H
#define ABSTRACTHEAP_H

#include <BinaryHeap.h>
#include <FibonacciHeap.h>

#define USE_BINARY_HEAP 1
#define USE_FIBONACCI_HEAP 2

typedef struct AbstractHeap {
	binary_heap *bin;
	FibHeap *fib;
} AbstractHeap;

typedef struct AbstractNode {
	unsigned int key;
	unsigned int rank;
	void *data;
	unsigned int bin;
	FibNode *fib;
} AbstractNode;

AbstractHeap *make_heap(unsigned int which, unsigned int size);
AbstractNode *find_min(AbstractHeap *heap);
AbstractNode *insert(unsigned int key, void *data, AbstractHeap *heap);
void *delete_min(AbstractHeap *heap);
void *decrease_key(unsigned int delta, AbstractNode *node, AbstractHeap *heap);
void *delete(AbstractNode *node, AbstractHeap *heap);

#endif