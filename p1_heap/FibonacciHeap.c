#include <AADS.h>
#include <stdlib.h>
#include <stdio.h>

fib_heap *fib_init_heap(unsigned int size);
unsigned int fib_insert(unsigned int key, void *data, fib_heap *h);
unsigned int fib_find_min(fib_heap *h);
bh_element *fib_delete_min(fib_heap *h);
fib_heap *fib_meld(fib_heap *h1, fib_heap *h2);
unsigned int fib_decrease_key(unsigned int new_key, unsigned int e, fib_heap *h);
bh_element *fib_delete_element(unsigned int e, fib_heap *h);
void fib_min_heapify(unsigned int e, fib_heap *h);
void fib_exchange(unsigned int e1, unsigned int e2, fib_heap *h);