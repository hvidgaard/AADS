#ifndef AADS_H
#define AADS_H

struct Element {
	int key;
	void* data;
};

struct Heap {
	Element data[];
};

class BinaryHeap {
	public:
		Heap* initialize_heap(int);
		void insert(Element*, Heap*);
		Element* find_min(Heap*);
		Element* delete_min(Heap*);
		Heap* meld(Heap*);
		Element* decrease_key(int, Element*);
		Element* delete_element(Element*, Heap*);
};

class FibonacciHeap {
	public:
		Heap* initialize_heap(int);
		void insert(Element*, Heap*);
		Element* find_min(Heap*);
		Element* delete_min(Heap*);
		Heap* meld(Heap*);
		Element* decrease_key(int, Element*);
		Element* delete_element(Element*, Heap*);
};

#endif