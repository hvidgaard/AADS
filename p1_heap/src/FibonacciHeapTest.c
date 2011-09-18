#include <stdio.h>
#include <stdlib.h>
#include <FibonacciHeap.h>

int main(int argc, char **argv)
{
	FibHeap *heap = fib_make_heap(30);
	int i;
	for (i = 1; i <= 16; i++)
		fib_insert(17-i, heap);
	
	FibNode *twenty = fib_insert(21, heap);
	fib_decrease_key(15, twenty, heap);

	FibNode *min = (FibNode *) fib_find_min(heap);
	while (min != NULL) {
		printf("min is %d\n", min->key);
		fib_delete_min(heap);
		min = fib_find_min(heap);
	}
	return 0;
}