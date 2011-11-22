#include <stdlib.h>
#include <stdio.h>
#include "sort.h"
#include "BinaryHeap.h"
#include "FibonacciHeap.h"
#include "vebtrees.h"
#include "RBTree/red_black_tree.h"

void sort_bin(uint size, uint* list) {
	binary_heap* heap = bh_init_heap(size);
	
	uint i;
	bh_element *e;
	for (i = 0; i < size; i++){
		bh_insert(list[i], NULL, heap);
	}
	for (i = 0; i < size; i++)
		bh_delete_min(heap);
	
	free(heap);
}

void sort_fib(uint size, uint* list) {
	FibHeap* heap = fib_make_heap();
	
	uint i;
	for (i = 0; i < size; i++)
		fib_insert(list[i], NULL, heap);
	for (i = 0; i < size; i++)
		fib_delete_min(heap);
	
	free(heap);
}

void sort_veb(uint size, uint* list) {
	vebtree* tree = veb_initialize(24, 2);
	
	uint i;
	for (i = 0; i < size; i++)
		veb_insert(list[i], NULL, tree);
	for (i = 0; i < size; i++)
		veb_delete_min(tree);
	
	free(tree);
}

void sort_rb(uint size, uint* list) {
	rb_red_blk_tree* tree = RBTreeCreate(CompareKeys, NullFunction, NullFunction, NullFunction, NullFunction);
	
	uint i;
	for (i = 0; i < size; i++)
		RBTreeInsert(tree, &list[i], NULL);
	
	rb_red_blk_node* node = tree->root;
	while(node->left != tree->nil)
		node = node->left;
	
	while(node != tree->nil)
		node = RB_TreeSucc(tree, node);
	RBTreeDestroy(tree);
}

int CompareKeys(const void* a, const void* b) {
	if( *(uint*)a > *(uint*)b) return(1);
	if( *(uint*)a < *(uint*)b) return(-1);
	return(0);
}