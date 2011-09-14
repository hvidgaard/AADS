#include <AADS.h>
#include <stdlib.h>
#include <stdio.h>

Fib_heap *fib_init_heap(unsigned int size);
unsigned int fib_insert(unsigned int key, void *data, Fib_heap *h);
unsigned int fib_find_min(Fib_heap *h);
Element *fib_delete_min(Fib_heap *h);
Fib_heap *fib_meld(Fib_heap *h1, Fib_heap *h2);
unsigned int fib_decrease_key(unsigned int new_key, unsigned int e, Fib_heap *h);
Element *fib_delete_element(unsigned int e, Fib_heap *h);
void fib_min_heapify(unsigned int e, Fib_heap *h);
void fib_exchange(unsigned int e1, unsigned int e2, Fib_heap *h);