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
	unsigned bool marked;
	Element *parent;
	Element *child;
	Element *left;
	Element *right;
} Element;

typedef struct FibHeap {
	FibElement *min;
	unsigned int maxRank;
} FibHeap;

#endif