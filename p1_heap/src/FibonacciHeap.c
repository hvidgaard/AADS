#include <AADS.h>
#include <stdlib.h>
#include <stdio.h>

FibHeap *fib_make_heap(unsigned int maxRank);
FibNode *fib_find_min(FibHeap *heap);
FibNode *fib_insert(unsigned int key, FibHeap *heap);
void *fib_meld(FibHeap *heap1, FibHeap *heap2);
void *fib_link(FibNode *left, FibNode *right);
void *fib_union(FibNode *node1, FibNode *node2);
void *fib_delete_min(FibHeap *heap);
void *fib_extract_childnode(FibNode *node, FibHeap *heap);
void *fib_decrease_key(unsigned int delta, FibNode *node, FibHeap *heap);
void *fib_delete(FibNode *node, FibHeap *heap);



FibHeap *fib_make_heap(unsigned int maxRank)
{
	FibHeap *heap = malloc(sizeof(struct FibHeap));
	heap->maxRank = maxRank;
	heap->min = NULL;
	return heap;
}

FibNode *fib_find_min(FibHeap *heap) {
	return heap->min;
}

FibNode *fib_insert(unsigned int key, FibHeap *heap)
{
	FibNode *node;
	if(!heap->min) {
		node = calloc(1,sizeof(struct FibNode));
		node->key = key;
		fib_link(node, node);
		heap->min = node;
		return node;
	}
	FibHeap *insertHeap = fib_make_heap(1);
	node = fib_insert(key, insertHeap);
	fib_meld(heap, insertHeap);
	free(insertHeap);
	return node;
}

void *fib_meld(FibHeap *heap1, FibHeap *heap2)
{
	fib_union(heap1->min, heap2->min);
	if(heap1->min->key > heap2->min->key)
		heap1->min = heap2->min;
	else
		heap2->min = heap1->min;
}

void *fib_link(FibNode *left, FibNode *right)
{
	left->right = right;
	right->left = left;
}

void *fib_union(FibNode *node1, FibNode *node2)
{
	if(!node1)
		node1 = node2;
	if(!node2)
		node2 = node1;
	node1->left->right = node2->right;
	node2->right->left = node1->left;
	node2->right = node1;
	node1->left = node2;
}

void *fib_delete_min(FibHeap *heap)
{
	fib_union(heap->min->child, heap->min->left);

	FibNode *startNode;
	FibNode *node = startNode = heap->min->child;
	int *rootNodes;
	rootNodes = (int *) calloc(heap->maxRank, sizeof(int));
	do {
		while(!rootNodes[node->rank]) {
			if(node->key < node->left->key) {
				fib_union(node->child, node->left);
			} else {
				if(node == startNode)
					startNode = node->left;
				node = node->left;
				fib_union(node->child, node->right);
			}
			node->rank++;
		}
		rootNodes[node->rank] = 1;
		node = node->left;
	} while(node->left != startNode);
	free(rootNodes);
}

void *fib_extract_childnode(FibNode *node, FibHeap *heap)
{
	FibNode *parent = node->parent;
	if(node->left == node) {
		parent->child = NULL;
	} else {
		parent->child = node->left;
		fib_link(node->left, node->right);
		fib_link(node, node);
	}
	parent->rank -= node->rank + 1;
	node->parent = NULL;
	if(parent->marked) {
		fib_extract_childnode(parent, heap);
		fib_union(heap->min, parent);
	}
	parent->marked = !parent->marked;
}

void *fib_decrease_key(unsigned int delta, FibNode *node, FibHeap *heap)
{
	node->key -= delta;
	if(node->parent != NULL) {
		fib_extract_childnode(node, heap);
		fib_union(heap->min, node);
	}
	if(heap->min->key > node->key)
		heap->min = node;
}

void *fib_delete(FibNode *node, FibHeap *heap)
{
	if(heap->min == node) {
		fib_delete_min(heap);
	} else {
		if(node->parent != NULL) {
			fib_extract_childnode(node, heap);
		} else {
			fib_link(node->left, node->right);
		}
		fib_union(heap->min, node->child);
	}
	free(node);
}