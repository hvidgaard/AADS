#ifndef AADS_H
#define AADS_H
#define UINT_MAX 4294967295

typedef struct bh_element {
	unsigned int key;
	unsigned int index;
	void *data;
} bh_element;

typedef struct binary_heap {
	unsigned int max_size;
	unsigned int size;
	bh_element * data;
} binary_heap;

typedef struct FibNode {
	unsigned int key;
	unsigned int rank;
	unsigned int marked;
	struct FibNode* parent;
	struct FibNode* child;
	struct FibNode* left;
	struct FibNode* right;
} FibNode;

typedef struct FibHeap {
	unsigned int maxRank;
	FibNode *min;
} FibHeap;

#endif