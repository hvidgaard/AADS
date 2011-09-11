#ifndef AADS_H
#define AADS_H

typedef struct Element {
	int key;
	void *data;
} Element;

typedef struct Heap {
	Element min;
	int size;
	Element *data[];
} Heap;

#endif