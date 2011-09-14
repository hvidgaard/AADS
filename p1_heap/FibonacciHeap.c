#include <AADS.h>
#include <stdlib.h>
#include <stdio.h>

FibHeap *fib_make_heap();
unsigned int fib_insert(unsigned int key, void *data, FibHeap *h);
unsigned int fib_find_min(FibHeap *h);
Element *fib_delete_min(FibHeap *h);
FibHeap *fib_meld(FibHeap *h1, FibHeap *h2);
unsigned int fib_decrease_key(unsigned int new_key, unsigned int e, FibHeap *h);
Element *fib_delete_element(unsigned int e, FibHeap *h);
void fib_min_heapify(unsigned int e, FibHeap *h);
void fib_exchange(unsigned int e1, unsigned int e2, FibHeap *h);



FibHeap *fib_make_heap(unsigned int maxRank) {
	FibHeap *heap = calloc(1,sizeof(struct FibHeap));
	heap->maxRank = maxRank;
	return heap;
}

void fib_insert(unsigned int key, FibHeap *heap) {
	if(heap->child == NULL) {
		FibNode *newNode = calloc(1,sizeof(struct FibNode));
		newNode->key = key;
		newNode.leftSibling = newNode;
		newNode.rightSibling = newNode;
		heap.min = newNode;
		return;
	}
	FibHeap *insertHeap = fib_make_heap();
	fib_insert(insertHeap, priority);
	fib_meld(heap, insertHeap);
	free(insertHeap);
}

void fib_meld(FibHeap *heap, FibHeap *insertHeap) {
	fib_circle(heap->min, insertHeap->min);
	if(heap->min->key > insertHeap->min->key)
		heap.min = insertHeap.min;
}

void fib_circle(FibNod *node1, FibNode *node2) {
	if(node1 == NULL)
		node1 = node2;
	if(node2 == NULL)
		node2 = node1;
	node1->left.right = node2.right;
	node2->right.left = node1.left;
	node2.right = node1;
	node1.left = node2;
}

void fib_delete_min(FibHeap *heap) {
	FibNode *node;
	FibNode *startNode;

	heap->min->child->left.right = heap->min.right;
	heap->min->right.left = heap->min->child.left;
	heap->min->child.left = heap->min.left;
	heap->min->left.right = heap->min.child;

	node = startNode = heap->min.child;
	FibNode *rootNodes = (FibNode*) calloc(heap->maxRank,sizeof(FibNode));
	do {
		while(rootNodes[node.rank] != NULL) {
			if(node->key < node.left->key) {
				fib_circle(node.child, node.left);
			} else {
				if(node == startNode)
					startNode = node.left;
				node = node.left;
				fib_circle(node.child, node.right);
			}
			node.rank++;
		}
		rootNodes[node.rank] = node;
		node = node.left;
	} while(node.left != startNode);
	free(rootNodes);
}

FibNode *fib_find_min(FibHeap *heap) {
	return heap.min;
}



















