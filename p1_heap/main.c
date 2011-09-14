#include <stdio.h>
#include <AADS.h>
#include <stdlib.h>

int main(int argc, char **argv)
{
	printf("hello world\n");
	Binary_heap *h = bh_init_heap(16);
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
	Element *e;
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
