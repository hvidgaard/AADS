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
		node->parent = NULL;
		node->child = NULL;
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
void fib_meld(FibHeap *heap1, FibHeap *heap2)
{
	fib_union(heap1->min, heap2->min);
	if (heap1->min->key > heap2->min->key)
		heap1->min = heap2->min;
	else
		heap2->min = heap1->min;
	
	heap1->nodes = heap2->nodes = (heap1->nodes + heap2->nodes);
}

/* Links two nodes, by setting the left node to be to the left of the right node. */
void fib_link(FibNode *left, FibNode *right)
{
	left->right = right;
	right->left = left;
}

/* Assuming node1 and node2 point to two circular, disjunct lists, the two are merged into one. */
void fib_union(FibNode *node1, FibNode *node2)
{
	/* Avoid tmp variables by using weird sequence of assignments */
	node1->left->right = node2->right;
	node2->right->left = node1->left;
	node2->right = node1;
	node1->left = node2;
}

/* Deletes the minimum node of the heap and finds the new minimum.
Also does all other kinds of shit. */
void fib_delete_min(FibHeap *heap)
{
	/* Nothing to do if there is no minimum to begin with */
	if (!heap->min)
		return;
	
	FibNode *oldMin = heap->min;
	if (oldMin->left == oldMin) {
		/* Cut out the old minimum */
		fib_extract_rootnode(oldMin);
		/* If oldMin does not have any children we are done. */
		if (!oldMin->child) {
			oldMin->deleted = 1;
			heap->nodes--;
			heap->min = NULL;
			return;
		}
		/* If the minimum is the only rootNode set the entire
		set of childNodes of min to be the new root. */
		heap->min = oldMin->child;
	} else {
		/* If the old minimum had children, make them root nodes */
		if(oldMin->child)
			fib_union(oldMin->child, oldMin->left);
		/* Set the minimum to be just something, will be updated later. */
		heap->min = oldMin->left;
		/* Cut out the old minimum */
		fib_extract_rootnode(oldMin);
	}
	oldMin->deleted = 1;
	heap->nodes--;

	struct FibNode **ranks = calloc(heap->nodes,sizeof(struct FibNode*));
	FibNode *current = heap->min;
	FibNode *next;
	FibNode *end = heap->min->right;
	FibNode *root;
	
	do {
		current->parent = NULL;
		next = current->left;
		root = fib_insert_rank(ranks, current);
		if (root->key <= heap->min->key)
			heap->min = root;
		if(current == end)
			break;
		current = next;
	} while (1);
	free(ranks);
}

FibNode *fib_insert_rank(struct FibNode **ranks, FibNode *insert) {
	FibNode *root = insert;
	FibNode *child;
	if ((child = ranks[root->rank])) {
		if(child->key < root->key) {
			root = child;
			child = insert;
		}
		ranks[root->rank] = NULL;
		fib_extract_rootnode(child);
		if (root->child)
			fib_union(root->child, child);
		else
			root->child = child;
		child->parent = root;
		root->rank++;
		return fib_insert_rank(ranks, root);
	}
	return ranks[root->rank] = root;
}

/* Extracts a node from a list of child nodes.
Properly updates the parent if necessary and the siblings as well. */
void fib_extract_childnode(FibNode *node, FibHeap *heap)
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
	/* Decrease rank of parent 1 */
	parent->rank--;
	
	/* Node does not have a parent now. */
	node->parent = NULL;
	/* Repeat process for the parent, if it was marked. */
	if (parent->marked && parent->parent) {
		fib_extract_childnode(parent, heap);
		parent->parent = NULL;
		fib_union(heap->min, parent);
	}
	parent->marked = !parent->marked;
}

/* Extraction of root nodes is a bit easier than childnodes */
void fib_extract_rootnode(FibNode *node)
{
	fib_link(node->left, node->right);
	fib_link(node, node);
}

/* Decreases the key of specified node */
void fib_decrease_key(unsigned int delta, FibNode *node, FibHeap *heap)
{
	if(node->deleted)
		return;
	node->key -= delta;
	/* Make the node a root node, if it isn't already. */
	if (node->parent && node->parent->key > node->key) {
			fib_extract_childnode(node, heap);
			node->parent = NULL;
			fib_union(heap->min, node);
	}
	if (heap->min->key > node->key) {
		heap->min = node;
	}
}

void fib_delete(FibNode *node, FibHeap *heap)
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