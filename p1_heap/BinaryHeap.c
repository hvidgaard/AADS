#include<AADS.h>
#include <stdlib.h>
#include <stdio.h>

Heap *bh_init_heap(unsigned int size);
unsigned int insert(Element *e, Heap *h);
unsigned int find_min(Heap *h);
Element *delete_min(Heap *h);
Heap *meld(Heap *h1, Heap *h2);
unsigned int bh_decrease_key(unsigned int new_key, unsigned int e, Heap *h);
Element *bh_delete_element(unsigned int e, Heap *h);
void min_heapify(unsigned int e, Heap *h);
void bh_exchange(unsigned int e1, unsigned int e2, Heap *h);
	
//Parent(i): i / 2
//Left(i): i * 2
//Right(i): (i * 2) + 1

void min_heapify(unsigned int e, Heap *h) {
	unsigned int parent = e / 2;
	unsigned int left = e * 2;
	unsigned int right = (e * 2) + 1;
	unsigned int smallest;
	unsigned int t_key;
	void *t_data;

	if (left <= h->size && h->data[left].key < h->data[right].key)
		smallest = left;
	else
		smallest = e;
	if (right <= h->size && h->data[right].key < h->data[smallest].key)
		smallest = right;
	if (smallest != e) {
		bh_exchange(e, smallest, h);
		min_heapify(smallest , h);
	}
}

void bh_exchange(unsigned int e1, unsigned int e2, Heap *h) {
	unsigned int t_key = h->data[e1].key;
	unsigned int t_index = h->data[e1].index;
	void *t_data = h->data[e1].data;
	printf("Exchanging %d key %d and %d key %d\n", e1, h->data[e1].key, e2, h->data[e2].key);
	h->data[e1].key = h->data[e2].key;
	h->data[e1].data = h->data[e2].data;
	h->data[e1].index = h->data[e2].index;
	h->data[e2].key = t_key;
	h->data[e2].data = t_data;
	h->data[e2].index = t_index;
	printf("now %d key %d and %d key %d\n\n", e1, h->data[e1].key, e2, h->data[e2].key);
}

Heap *bh_init_heap(unsigned int size) {
	Heap *h = malloc(sizeof(struct Heap));
	h->max_size = size;
	h->size = 0;
	h->data = calloc(size+1, sizeof(struct Element));
	return h;
}

unsigned int bh_insert(unsigned int key, void *data, Heap *h) {
	unsigned int result;
	if (h->size < h->max_size) {
		printf("insert key %d\n", key);
		h->size++;
		h->data[h->size].key = UINT_MAX;
		h->data[h->size].data = data;
		h->data[h->size].index = h->size;
		result = bh_decrease_key(key, h->size, h);
		printf("inserted key %d, got index %d\n\n", key, result);
		return result;
	}
	else {
		printf("heap is full\n");
		return 0;
	}
}

unsigned int bh_find_min(Heap *h) {
	if (h->size > 1)
		return 1;
	else
		return 0;
}

Element *bh_delete_min(Heap *h) {
	if (h->size > 0)
		return bh_delete_element(1, h);
	else
		return NULL;
}

Heap *bh_meld(Heap *h1, Heap *h2) {
	return 0;
}

unsigned int bh_decrease_key(unsigned int new_key, unsigned int e, Heap *h) {
	unsigned int parent;
	if (new_key >= h->data[e].key)
			//technically it's an error
			return 0;
	if (e <= h->size) {
		h->data[e].key = new_key;
		parent = e / 2;
		while (e > 1 && h->data[parent].key > h->data[e].key) {
			bh_exchange(e, parent, h);
			e = parent;
			parent = e / 2;
		}
	}
	return e;
}

Element *bh_delete_element(unsigned int e, Heap *h){
	Element *result = malloc(sizeof(struct Element));
	printf("Deleting: %d, with key %d\n", e, h->data[e].key);
	if (h->size > 1 && e <= h->size) {
		bh_exchange(e, h->size, h);
		h->size--;
		min_heapify(e, h);
	}
	else if (h->size < 1 || e == 1)
		h->size--;
	else
		return NULL;
	
	result->data = h->data[h->size+1].data;
	result->key = h->data[h->size+1].key;
	result->index = 0;
	return result;
}
