#include <stdlib.h>
#include <stdio.h>
#include <FibonacciHeap.h>


FibHeap *fib_make_heap(unsigned int maxRank)
{
	struct FibHeap *heap = malloc(sizeof *heap);
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
		node->parent == NULL;
		node->child == NULL;
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
	if(!node1 || !node2)
		return;
	node1->left->right = node2->right;
	node2->right->left = node1->left;
	node2->right = node1;
	node1->left = node2;
}

void *fib_delete_min(FibHeap *heap)
{
	// Nothing to do if there is no minimum to begin with;
	if(heap->min == NULL)
		return;
	
	FibNode *oldMin = heap->min;
	if(oldMin->left == oldMin) {
		/* If the minimum is the only rootNode set the entire
		set of childNodes of min to be the new root. */
		heap->min = oldMin->child;
		/* If the heap was set to NULL, we are done. */
		if(!oldMin->child) {
			free(oldMin);
			return;
		}
	} else {
		/* Cut out the old minimum */
		fib_link(oldMin->left, oldMin->right);
		/* If the old minimum had children, make them root nodes,
		->parent will be set to NULL, when iterating. */
		if(oldMin->child)
			fib_union(oldMin->child, oldMin->left);
		/* Set the minimum to be just something, will be updated later. */
		heap->min = oldMin->left;
	}
	free(oldMin);

	FibNode *start;
	FibNode *next;
	FibNode *node;
	node = start = heap->min;
	struct FibNode **roots;
	roots = calloc(heap->maxRank,sizeof(struct FibNode*));
	do {
		/* Remove parent pointer if the node is a child from the min node. */
		node->parent = NULL;
		/* Make sure we, know which node is the next one.
		The actions below can mess with the linked list quite a bit. */
		next = node->left;
		/* If there already is a node with the same rank, merge the two.
		Only do that when it is not the same node of course. */
		while(roots[node->rank] && node != roots[node->rank]) {
			FibNode *other = roots[node->rank];
			/* Make one node the child of the other, depending on the key*/
			if(node->key < other->key) {
				fib_union(node->child, other);
				other->parent = node;
			} else {
				/* If the current node is the start, make the next node
				assume this role. Otherwise the outer loop won't end.*/
				if(node == start)
					start = next;
				fib_union(other->child, node);
				node->parent = other;
				/* The other node has for all intents and purposes
				assumed the position of the original node. Make it official*/
				node = other;
			}
			/* The rank of the node we merged with has changed,
			it does not occupy this position any longer. */
			roots[node->rank] = NULL;
			node->rank++;
		}
		roots[node->rank] = node;
		/* Update the minimum of the heap. */
		if(node->key < heap->min->key)
			heap->min = node;
		node = next;
	} while(next != start);
	free(roots);
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