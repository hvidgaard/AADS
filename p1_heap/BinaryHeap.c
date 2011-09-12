#include<AADS.h>
#include <stdlib.h>

Heap *bh_init_heap(unsigned int size);
unsigned int insert(Element *e, Heap *h);
unsigned int find_min(Heap *h);
void delete_min(Heap *h);
Heap *meld(Heap *h1, Heap *h2);
unsigned int bh_decrease_key(unsigned int new_key, unsigned int e, Heap *h);
unsigned int delete_element(Element *e, Heap *h);
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

	if (left <= h->max_size && h->data[left].key < h->data[right].key)
		smallest = left;
	else
		smallest = e;
	if (right <= h->max_size && h->data[right].key < h->data[smallest].key)
		smallest = right;
	if (smallest != e) {
		bh_exchange(e, smallest, h);
		min_heapify(smallest , h);
	}
}

void bh_exchange(unsigned int e1, unsigned int e2, Heap *h) {
	unsigned int t_key = h->data[e1].key;
	void *t_data = h->data[e1].data;
	h->data[e1].key = h->data[e2].key;
	h->data[e1].data = h->data[e2].data;
	h->data[e2].key = t_key;
	h->data[e2].data = t_data;
}

Heap* bh_init_heap(unsigned int size) {
	Heap *h = malloc(sizeof(struct Heap));
	h->max_size = size;
	h->size = 0;
	h->data = calloc(size, sizeof(struct Element));
	return h;
}

unsigned int bh_insert(unsigned int key, void *data, Heap *h) {
	h->size++;
	h->data[h->size].key = UINT_MAX;
	h->data[h->size].data = data;
	h->data[h->size].index = h->size;
	return bh_decrease_key(key, h->size, h);
}

unsigned int bh_find_min(Heap *h) {
	return 0;
}

Element *bh_delete_min(Heap *h) {
	return 0;
}

Heap *bh_meld(Heap *h1, Heap *h2) {
	return 0;
}

unsigned int bh_decrease_key(unsigned int new_key, unsigned int e, Heap *h) {
	unsigned int parent;
	if (new_key >= h->data[e].key)
		//technically it's an error
		return e;
	h->data[e].key = new_key;
	printf("new key is %d\n", new_key);
	parent = e / 2;
	while (e > 0 && h->data[parent].key > h->data[e].key) {
		printf("node key: %d and parent key %d\n", h->data[e].key, h->data[parent].key);
		bh_exchange(e, parent, h);
		e = parent;
		parent = e / 2;
	}
	return e;
}

Element *bh_delete_element(Element *e, Heap *h){
	return 0;
}