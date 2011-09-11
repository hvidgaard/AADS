#include<AADS.h>

Heap *bh_init_heap(int size);
void insert(Element *e, Heap *h);
Element *find_min(Heap *h);
Element *delete_min(Heap *h);
Heap *meld(Heap *h1, Heap *h2);
Element *decrease_key(int delta, Element *e, Heap *h);
Element *delete_element(Element *e, Heap *h);

Heap *bh_init_heap(int size) {
	return 0;
}

void bh_insert(Element *e, Heap *h) {
	return;
}

Element *bh_find_min(Heap *h) {
	return 0;
}

Element *bh_delete_min(Heap *h) {
	return 0;
}

Heap *bh_meld(Heap *h1, Heap *h2) {
	return 0;
}

Element *bh_decrease_key(int delta, Element *e, Heap *h) {
	return 0;
}

Element *bh_delete_element(Element *e, Heap *h){
	return 0;
}