#include <stdlib.h>
#include <stdio.h>
#include <BinaryHeap.h>
#include <FibonacciHeap.h>
#include <AbstractHeap.h>
#include <DijkstraSSSP.h>

int test_abstract();
int test_binary2();
int test_fibonacci();

int main(int argc, char **argv)
{
	printf("Commence testing...\n");
	test_binary(fopen("./../testdata/test1.txt", "w"));
	/*if(argc != 2)
		exit(1);
	
	if(strcmp(argv[1], "abstract") == 0) {
		exit(test_abstract());
	} else if(strcmp(argv[1], "bin") == 0) {
		exit(test_binary());
	} else if(strcmp(argv[1], "fib") == 0) {
		exit(test_fibonacci());
	}*/
}

int test_abstract() {
	AbstractHeap *heap = make_heap(USE_FIBONACCI_HEAP, 30);
	int i;
	for (i = 1; i <= 16; i++)
		insert(17-i, NULL, heap);
	
	AbstractNode *twenty = insert(21, NULL, heap);
	decrease_key(15, twenty, heap);

	AbstractNode *min = (AbstractNode *) find_min(heap);
	while (min) {
		printf("min is %d\n", min->key);
		delete_min(heap);
		min = find_min(heap);
	}
	return EXIT_SUCCESS;
}

int test_binary2()
{
	printf("hello world\n");
	binary_heap *h = bh_init_heap(16);
	int i;
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
	return EXIT_SUCCESS;
}

int test_fibonacci()
{
	FibHeap *heap = fib_make_heap(30);
	int i;
	for (i = 1; i <= 16; i++)
		fib_insert(17-i, NULL, heap);
	
	FibNode *twenty = fib_insert(21, NULL, heap);
	fib_decrease_key(15, twenty, heap);

	FibNode *min = (FibNode *) fib_find_min(heap);
	while (min) {
		printf("min is %d\n", min->key);
		fib_delete_min(heap);
		min = fib_find_min(heap);
	}
	return EXIT_SUCCESS;
}