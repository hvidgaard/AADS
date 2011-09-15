#include <stdio.h>
#include <stdlib.h>
#include <AADS.h>

int main(int argc, char **argv)
{
	FibHeap *heap = fib_make_heap(16);
	int i;
	for (i = 1; i <= 16; i++)
		fib_insert(17-i, heap);
	i = 0;
	FibNode *min = (FibNode *) fib_find_min(heap);
	while (min != NULL) {
		printf("min is %d\n", min->key);
		fib_delete_min(heap);
		min = fib_find_min(heap);
	}
	return 0;
	printf("hello world\n");
	binary_heap *h = bh_init_heap(16);
	for (i = 1; i <= 16; i++)
		bh_insert(17-i, NULL, h);
	i = 0;
	while (i < h->size) {
		printf("element %d value %d\n", i, h->data[i].key);
		fflush(stdout);
		i++;
	}
	printf("\n");
	bh_element *e;
	while (h->size != 0){
		printf("heap size: %d - deleting min\n", h->size);
		e = bh_delete_min(h);
		printf("got: %d\n\n", e->key);
		printf("heap size is now: %d\n", h->size);
		fflush(stdout);
		free(e);
	}

	fflush(stdout);
	return 0;
}
