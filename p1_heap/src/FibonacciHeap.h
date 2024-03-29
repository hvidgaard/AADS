#ifndef FIBHEAP_H
#define FIBHEAP_H

typedef struct FibNode {
	unsigned int key;
	unsigned int rank;
	unsigned int marked;
	unsigned int deleted;
	void *data;
	struct FibNode* parent;
	struct FibNode* child;
	struct FibNode* left;
	struct FibNode* right;
} FibNode;

typedef struct FibHeap {
	unsigned int nodes;
	FibNode *min;
} FibHeap;

FibHeap *fib_make_heap();
FibNode *fib_find_min(FibHeap *heap);
FibNode *fib_insert(unsigned int key, void *data, FibHeap *heap);
void fib_delete_min(FibHeap *heap);
void fib_decrease_key(unsigned int delta, FibNode *node, FibHeap *heap);
FibNode *fib_insert_rank(struct FibNode **ranks, FibNode *insert);
void fib_delete(FibNode *node, FibHeap *heap);
void fib_meld(FibHeap *heap1, FibHeap *heap2);
void fib_link(FibNode *left, FibNode *right);
void fib_union(FibNode *node1, FibNode *node2);
void fib_extract_rootnode(FibNode *node);
void fib_extract_childnode(FibNode *node, FibHeap *heap);

#endif