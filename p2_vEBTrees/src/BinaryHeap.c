#include <stdlib.h>
#include <stdio.h>
#include <BinaryHeap.h>


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

/* Will return a pointer to the heap
 */
binary_heap *bh_init_heap(unsigned int size) {
	binary_heap *h = malloc(sizeof(struct binary_heap));
	if (!h)
		return NULL;
	h->max_size = size;
	h->size = 0;
	h->data = malloc((size+1) * sizeof(bh_element *));
	//h->elements = malloc(size *sizeof(unsigned int *));
	return h;
}

/* Will return a pointer to the element
 */
bh_element * bh_insert(unsigned int key, void *data, binary_heap *h) {
	if (h->size < h->max_size) {
		//printf("insert key %d\n", key);
		h->size++;
		bh_element * e = malloc(sizeof(struct bh_element));
		if (!e)
			return 0;
		e->key  = key;
		e->data = data;
		e->index = h->size;
		h->data[h->size] = e;
		bh_decrease_key(0, e, h);
		//printf("inserted key %d, got index %d\n\n", key, e->index);
		return e;
	}
	else
		return NULL;
}

/* will decrease the key of an element, and bubble it up until the heap
 * property is reestablished
 * Will return the new index in the array, or 0 if something went wrong.
 */
unsigned int bh_decrease_key(unsigned int delta, bh_element * e, binary_heap *h) {
	//printf("decreasing: node: %d, key: %d, delta %d\n", *	(unsigned int*)e->data, e->key, delta);
	unsigned int parent;
	//error, cannot decrease key to a negative value.
	if (delta > e->key)
		return 0;
	if (e->index != 1) {
		e->key -= delta;
		parent = e->index / 2;
		while (e->index > 1 && h->data[parent]->key > e->key) {
			bh_exchange(e->index, parent, h);
			//e->index = parent;
			parent = e->index / 2;
		}
	}
	else if (e->index == 1)
		e->key -= delta;
	return e->index;
}

void bh_exchange(unsigned int e1, unsigned int e2, binary_heap *h) {
	//printf("Exchanging %d key %d and %d key %d\n", e1, h->data[e1]->key, e2, h->data[e2]->key);
	bh_element * t = h->data[e1];
	h->data[e1] = h->data[e2];
	h->data[e1]->index = e1;
	h->data[e2] = t;
	h->data[e2]->index = e2;
	
	/*unsigned int t_key = h->data[e1].key;
	unsigned int t_index = h->data[e1].index;
	void *t_data = h->data[e1].data;
	//printf("Exchanging %d key %d and %d key %d\n", e1, h->data[e1].key, e2, h->data[e2].key);
	h->data[e1].key = h->data[e2].key;
	h->data[e1].data = h->data[e2].data;
	h->data[e1].index = h->data[e2].index;
	h->data[e2].key = t_key;
	h->data[e2].data = t_data;
	h->data[e2].index = t_index;*/
	//printf("now %d key %d and %d key %d\n\n", e1, h->data[e1].key, e2, h->data[e2].key);
}

bh_element *bh_delete_element(unsigned int e, binary_heap *h){
	bh_element * result = malloc(sizeof(struct bh_element));
	//printf("Deleting index %d, with key %d\n", e, h->data[e]->key);
	if (h->size > 1 && e <= h->size) {
		bh_exchange(e, h->size, h);
		h->size--;
		bh_min_heapify(e, h);
	}
	else if (h->size < 1 || e == 1)
		h->size--;
	else
		return NULL;
	
	result->data = h->data[h->size+1]->data;
	result->key = h->data[h->size+1]->key;
	result->index = 0;
	return result;
}

/* min_heapify repairs the heap after an element is inserted that is larger than
 * it's children. Basicly it will move it down until both of it's children are
 * larger - while making sure that it stays within the size of the current heap.
 */
void bh_min_heapify(unsigned int e, binary_heap *h) {
	unsigned int left = e * 2;
	unsigned int right = (e * 2) + 1;
	unsigned int smallest;

	if (left <= h->size && h->data[left]->key < h->data[e]->key)
		smallest = left;
	else
		smallest = e;
	if (right <= h->size && h->data[right]->key < h->data[smallest]->key)
		smallest = right;
	if (smallest != e) {
		bh_exchange(e, smallest, h);
		bh_min_heapify(smallest , h);
	}
}

bh_element * bh_find_min(binary_heap *h) {
	if (h->size > 0)
		return h->data[1];
	else
		return NULL;
}

bh_element *bh_delete_min(binary_heap *h) {
	if (h->size > 0)
		return bh_delete_element(1, h);
	else
		return NULL;
}

/* NOT IMPLEMENTED
 */
binary_heap *bh_meld(binary_heap *h1, binary_heap *h2) {
	return 0;
}