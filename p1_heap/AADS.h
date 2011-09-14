#ifndef AADS_H
#define AADS_H
#define UINT_MAX 4294967295

typedef struct Element {
	unsigned int key;
	unsigned int index;
	void *data;
} Element;

typedef struct Binary_heap {
	unsigned int max_size;
	unsigned int size;
	Element * data;
} Binary_heap;

typedef struct Fib_heap {
	//INSERT HERE
} Fib_heap;

#endif