#include <stdlib.h>
#include "sort.h"
#include "BinaryHeap.h"
#include "FibonacciHeap.h"
#include "vebtrees.h"
#include "RBTree/red_black_tree.h"

void sort_bin(uint size, uint* list) {
	binary_heap * heap = bh_init_heap(size);
	
	uint i;
	for (i = 0; i < size; i++)
		bh_insert(list[i], NULL, heap);
	for (i = 0; i < size; i++)
		bh_delete_min(heap);
	
	free(heap);
}

void sort_fib(uint size, uint* list) {
	FibHeap * heap = fib_make_heap();
	
	uint i;
	for (i = 0; i < size; i++)
		fib_insert(list[i], NULL, heap);
	for (i = 0; i < size; i++)
		fib_delete_min(heap);
	
	free(heap);
}

void sort_veb(uint size, uint* list) {
	vebtree *tree = veb_initialize(24, 2);
	
	uint i;
	for (i = 0; i < size; i++)
		veb_insert(list[i], NULL, tree);
	for (i = 0; i < size; i++)
		veb_delete_min(tree);
	
	free(tree);
}

void sort_rb(uint size, uint* list) {
}