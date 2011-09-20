#include <stdlib.h>
#include <stdio.h>
#include <FibonacciHeap.h>

/* Initialize a new Fibonacci heap. */
FibHeap *fib_make_heap()
{
	struct FibHeap *heap = malloc(sizeof *heap);
	heap->nodes = 0;
	/* No min yet, set it to NULL.
	This essentially means, the heap has no nodes. */
	heap->min = NULL;
	return heap;
}

/* Returns the current minimum of the heap. */
FibNode *fib_find_min(FibHeap *heap) {
	return heap->min;
}

/* Creates a new node with specified key, and inserts it into the heap. */
FibNode *fib_insert(unsigned int key, void *data, FibHeap *heap)
{
	FibNode *node;
	/* If the heap has no nodes, simply insert the new node as the minimum. */
	if (!heap->min) {
		node = calloc(1,sizeof(struct FibNode));
		node->key = key;
		node->data = data;
		fib_link(node, node);
		node->parent == NULL;
		node->child == NULL;
		heap->min = node;
		heap->nodes = 1;
		return node;
	}
	FibHeap *insertHeap = fib_make_heap(1);
	/* This call will result in the execution of the if() above,
	since the insertHeap has no nodes. */
	node = fib_insert(key, data, insertHeap);
	fib_meld(heap, insertHeap);
	free(insertHeap);
	return node;
}

/* Melds two heaps together, maintaining the minimum pointers */
void *fib_meld(FibHeap *heap1, FibHeap *heap2)
{
	fib_union(heap1->min, heap2->min);
	if (heap1->min->key > heap2->min->key)
		heap1->min = heap2->min;
	else
		heap2->min = heap1->min;
	
	heap1->nodes = heap2->nodes = (heap1->nodes + heap2->nodes);
}

/* Links two nodes, by setting the left node to be to the left of the right node. */
void *fib_link(FibNode *left, FibNode *right)
{
	left->right = right;
	right->left = left;
}

/* Assuming node1 and node2 point to two circular, disjunct lists, the two are merged into one. */
void *fib_union(FibNode *node1, FibNode *node2)
{
	/* If either node is NULL, do nothing. */
	if (!node1 || !node2)
		return;
	/* Avoid tmp variables by using weird sequence of assignments */
	node1->left->right = node2->right;
	node2->right->left = node1->left;
	node2->right = node1;
	node1->left = node2;
}

/* Deletes the minimum node of the heap and finds the new minimum,
while doing all other kinds of shit. */
void *fib_delete_min(FibHeap *heap)
{
	// Nothing to do if there is no minimum to begin with;
	if (!heap->min)
		return;
	
	FibNode *oldMin = heap->min;
	if (oldMin->left == oldMin) {
		/* If the minimum is the only rootNode set the entire
		set of childNodes of min to be the new root. */
		heap->min = oldMin->child;
		/* If the heap was set to NULL, we are done. */
		if (!oldMin->child) {
			heap->nodes--;
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
	heap->nodes--;
	free(oldMin);

	/* Since we don't know the size of the list and it changes,
	while we iterate, we simply remember the node we started at instead.
	The list is circular, so we arrive at the start node when we are done. */
	FibNode *start;
	FibNode *next;
	FibNode *node;
	node = start = heap->min;

	/* Allocate a zero initialized array, to check if any two nodes have the same rank. */
	struct FibNode **roots;
	/* heap->nodes is actually too large, this can be smaller. */
	roots = calloc(heap->nodes,sizeof(struct FibNode*));
	do {
		/* Make sure we, know which node is the next one.
		The actions below can mess with the linked list quite a bit. */
		next = node->left;
		/* Remove parent pointer. The node could be a child from the min node. */
		node->parent = NULL;
		/* If there already is a node with the same rank, merge the two.
		Only do that when it is not the same node of course. */
		while (roots[node->rank] && node != roots[node->rank]) {
			FibNode *other = roots[node->rank];
			/* Make one node the child of the other, depending on the key*/
			if (node->key < other->key) {
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
		if (node->key < heap->min->key)
			heap->min = node;
		node = next;
	} while (next != start);
	free(roots);
}

/* Extracts a node from a list of child nodes.
Properly updates the parent if necessary and the siblings as well. */
void *fib_extract_childnode(FibNode *node, FibHeap *heap)
{
	FibNode *parent = node->parent;
	if (node->left == node) {
		/* Single child. Set parent child pointer to null. */
		parent->child = NULL;
	} else {
		/* Set parent child pointer to left sibling.
		Don't really care if it pointed to node in the first place. */
		parent->child = node->left;
		/* Cut out the node, from it's siblings. */
		fib_link(node->left, node->right);
		/* Node is probably going to be inserted somewhere else,
		so make sure left & right pointers are correct */
		fib_link(node, node);
	}
	/* Decrease rank of parent by node rank plus one. */
	parent->rank -= node->rank + 1;
	/* Node does not have a parent now. */
	node->parent = NULL;
	/* Repeat process for the parent, if it was marked. */
	if (parent->marked) {
		fib_extract_childnode(parent, heap);
		fib_union(heap->min, parent);
	}
	parent->marked = !parent->marked;
}

/* Decreases the key of specified node */
void *fib_decrease_key(unsigned int delta, FibNode *node, FibHeap *heap)
{
	node->key -= delta;
	/* Make the node a root node, if it isn't already. */
	if (node->parent) {
		if(node->parent->key < node->key) {
			fib_extract_childnode(node, heap);
			node->parent = NULL;
			fib_union(heap->min, node);
		}
	}
	if (heap->min->key > node->key)
		heap->min = node;
}

void *fib_delete(FibNode *node, FibHeap *heap)
{
	/* If the node is the minimum, it is actually delete_min we want to call. */
	if (heap->min == node) {
		fib_delete_min(heap);
	} else {
		/* If node is a child node, do the marking stuff etc.
		otherwise, we just cut it out. */
		if (node->parent)
			fib_extract_childnode(node, heap);
		else
			fib_link(node->left, node->right);
		/* Make the children root nodes. */
		fib_union(heap->min, node->child);
		heap->nodes--;
		free(node);
	}
}