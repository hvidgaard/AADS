#ifndef BINHEAP_H
#define BINHEAP_H

typedef struct bh_element {
	unsigned int key;
	unsigned int index;
	void *data;
} bh_element;

typedef struct binary_heap {
	unsigned int max_size;
	unsigned int size;
	bh_element **data;
} binary_heap;

binary_heap *bh_init_heap(unsigned int size);
bh_element * bh_insert(int key, void * data, binary_heap *h);
bh_element *bh_find_min(binary_heap *h);
bh_element *bh_delete_min(binary_heap *h);
binary_heap *bh_meld(binary_heap *h1, binary_heap *h2);
unsigned int bh_decrease_key(unsigned int delta, bh_element * e, binary_heap *h);
bh_element *bh_delete_element(unsigned int e, binary_heap *h);
void bh_min_heapify(unsigned int e, binary_heap *h);
void bh_exchange(unsigned int e1, unsigned int e2, binary_heap *h);
void bh_destruct(binary_heap * bh);

#endif