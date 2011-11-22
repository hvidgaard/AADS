#include <stdlib.h>
#include <stdio.h>
#include "sort.h"
#include "BinaryHeap.h"
#include "FibonacciHeap.h"
#include "vebtrees.h"
#include "rbtree.h"

void sort_bin(uint size, uint* list) {
	binary_heap* heap = bh_init_heap(size);
	
	uint i;
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
	rb_tree* tree = calloc(1, sizeof(rb_tree));
	
	uint i;
	for (i = 0; i < size; i++) {
		printf("Inserting %d\n", list[i]);
		rb_insert(list[i], tree);
	}
	
	rb_node* node = tree->root;
	while(!is_leaf(node->left))
		node = node->left;
	
	rb_node* successor = node;
	while((node = successor)) {
		successor = rb_succ(node);
		printf("Deleting %d\n", node->key);
		rb_delete(node);
	}
	free(tree);
}