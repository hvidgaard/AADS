#include <stdlib.h>
#include <stdio.h>
#include <AbstractHeap.h>

/* Initialize a new Fibonacci heap. */
AbstractHeap *make_heap(unsigned int which, unsigned int size)
{
	struct AbstractHeap *heap = calloc(1,sizeof *heap);
	if(which == USE_BINARY_HEAP)
		heap->bin = bh_init_heap(size);
	else
		heap->fib = fib_make_heap();
	return heap;
}

AbstractNode *find_min(AbstractHeap *heap) {
	AbstractNode *node = calloc(1,sizeof(struct AbstractNode));
	if (heap->bin) {
		node->bin = bh_find_min(heap->bin);
		if(!node->bin)
			return NULL;
		node->key = heap->bin->data[node->bin]->key;
		node->data = heap->bin->data[node->bin]->data;
	} else {
		node->fib = fib_find_min(heap->fib);
		if(!node->fib)
			return NULL;
		node->key = node->fib->key;
		node->data = node->fib->data;
	}
	return node;
}

AbstractNode *insert(unsigned int key, void *data, AbstractHeap *heap) {
	AbstractNode *node = calloc(1,sizeof(struct AbstractNode));
	if (heap->bin)
		node->bin = bh_insert(key, data, heap->bin);
	else
		node->fib = fib_insert(key, data, heap->fib);
	node->key = key;
	node->data = data;
	return node;
}

void *delete_min(AbstractHeap *heap) {
	if(heap->bin)
		bh_delete_min(heap->bin);
	else
		fib_delete_min(heap->fib);
}

void *decrease_key(unsigned int delta, AbstractNode *node, AbstractHeap *heap) {
	if(heap->bin)
		bh_decrease_key(delta, node->bin, heap->bin);
	else
		fib_decrease_key(delta, node->fib, heap->fib);
	node->key -= delta;
}
void *delete(AbstractNode *node, AbstractHeap *heap) {
	if(heap->bin)
		bh_delete_element(node->bin, heap->bin);
	else
		fib_delete(node->fib, heap->fib);
}