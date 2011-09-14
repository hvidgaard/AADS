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

typedef struct FibElement {
	unsigned int key;
	unsigned int index;
	unsigned bool marked;
	Element * parent;
	Element * child;
	Element * leftSibling;
	Element * rightSibling;
	void *data;
} Element;

typedef struct FibHeap {
	//INSERT HERE
} FibHeap;

#endif