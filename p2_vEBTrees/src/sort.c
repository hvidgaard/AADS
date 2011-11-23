#include <stdlib.h>
#include <stdio.h>
#include "sort.h"
#include "BinaryHeap.h"
#include "FibonacciHeap.h"
#include "vebtrees.h"
#include "veb_pq.h"
#include "rb_tree.h"

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
	//vebtree* tree = veb_initialize(24, 2);
	vebtree * tree = veb_pq_init(24);
	uint i;
	for (i = 0; i < size; i++){
		veb_pq_node *n = malloc(sizeof(veb_pq_node));
		n->node_prio = i;
		veb_pq_insert(n, tree);
		//veb_insert(list[i], NULL, tree);
		
	}
	for (i = 0; i < size; i++)
		free(veb_pq_deletemin(tree));
		//free(tree);
	veb_destruct(tree);
}

void sort_rb(uint size, uint* list) {
	rb_tree* tree = rb_init();
		
	uint i;
	for (i = 0; i < size; i++)
		rb_insert(list[i], tree);
	
	rb_node* node = tree->root;
	while(!is_leaf(node->left, tree))
		node = node->left;
	
	rb_node* successor = node;
	while((node = successor)) {
		successor = rb_succ(node, tree);
		if(successor && successor->key < node->key) {
			printf("Successor is wrong.\n");
			abort();
		}
		rb_delete(node, tree);
		node = successor;
		while(node && (successor = node))
			node = rb_pred(successor, tree);
		
	}
	free(tree);
}