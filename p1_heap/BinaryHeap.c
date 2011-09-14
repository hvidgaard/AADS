#include <AADS.h>
#include <stdlib.h>
#include <stdio.h>

Binary_heap *bh_init_heap(unsigned int size);
unsigned int bh_insert(unsigned int key, void *data, Binary_heap *h);
unsigned int bh_find_min(Binary_heap *h);
Element *bh_delete_min(Binary_heap *h);
Binary_heap *bh_meld(Binary_heap *h1, Binary_heap *h2);
unsigned int bh_decrease_key(unsigned int new_key, unsigned int e, Binary_heap *h);
Element *bh_delete_element(unsigned int e, Binary_heap *h);
void bh_min_heapify(unsigned int e, Binary_heap *h);
void bh_exchange(unsigned int e1, unsigned int e2, Binary_heap *h);

/* Given an index in the array, the family relations are as following:
 * 
 * Parent(i): i / 2
 * Left(i): i * 2
 * Right(i): (i * 2) + 1
 * 
 * note this requires that the first element is 1, so in order to not
 * complicate the family relation formulas, the element at index 0 is
 * just sitting and never used.
 */

/* min_heapify repairs the heap after an element is inserted that is larger than
 * it's children. Basicly it will move it down until both of it's children are
 * larger - while making sure that it stays within the size of the current heap.
 */
void bh_min_heapify(unsigned int e, Binary_heap *h) {
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
		bh_min_heapify(smallest , h);
	}
}

void bh_exchange(unsigned int e1, unsigned int e2, Binary_heap *h) {
	unsigned int t_key = h->data[e1].key;
	unsigned int t_index = h->data[e1].index;
	void *t_data = h->data[e1].data;
	//printf("Exchanging %d key %d and %d key %d\n", e1, h->data[e1].key, e2, h->data[e2].key);
	h->data[e1].key = h->data[e2].key;
	h->data[e1].data = h->data[e2].data;
	h->data[e1].index = h->data[e2].index;
	h->data[e2].key = t_key;
	h->data[e2].data = t_data;
	h->data[e2].index = t_index;
	//printf("now %d key %d and %d key %d\n\n", e1, h->data[e1].key, e2, h->data[e2].key);
}

Binary_heap *bh_init_heap(unsigned int size) {
	Binary_heap *h = malloc(sizeof(struct Binary_heap));
	h->max_size = size;
	h->size = 0;
	h->data = calloc(size+1, sizeof(struct Element));
	return h;
}

unsigned int bh_insert(unsigned int key, void *data, Binary_heap *h) {
	unsigned int result;
	if (h->size < h->max_size) {
		//printf("insert key %d\n", key);
		h->size++;
		h->data[h->size].key = UINT_MAX;
		h->data[h->size].data = data;
		h->data[h->size].index = h->size;
		result = bh_decrease_key(key, h->size, h);
		//printf("inserted key %d, got index %d\n\n", key, result);
		return result;
	}
	else {
		//printf("heap is full\n");
		return 0;
	}
}

unsigned int bh_find_min(Binary_heap *h) {
	if (h->size > 0)
		return 1;
	else
		return 0;
}

Element *bh_delete_min(Binary_heap *h) {
	if (h->size > 0)
		return bh_delete_element(1, h);
	else
		return NULL;
}

/* NOT IMPLEMENTED
 */
Binary_heap *bh_meld(Binary_heap *h1, Binary_heap *h2) {
	return 0;
}

/* will decrease the key of an element, and bubble it up until the heap
 * property is reestablished
 */
unsigned int bh_decrease_key(unsigned int new_key, unsigned int e, Binary_heap *h) {
	unsigned int parent;
	if (new_key >= h->data[e].key)
			//technically it's an error, so we return 0
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

Element *bh_delete_element(unsigned int e, Binary_heap *h){
	Element *result = malloc(sizeof(struct Element));
	//printf("Deleting: %d, with key %d\n", e, h->data[e].key);
	if (h->size > 1 && e <= h->size) {
		bh_exchange(e, h->size, h);
		h->size--;
		bh_min_heapify(e, h);
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