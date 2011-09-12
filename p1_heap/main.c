#include <stdio.h>
#include <AADS.h>

int main(int argc, char **argv)
{
	printf("hello world\n");
	Heap* h = bh_init_heap(32);
	printf("%d\n",bh_insert(50, NULL, h));
	printf("%d\n",bh_insert(32, NULL, h));
	printf("%d\n",bh_insert(62, NULL, h));
	printf("%d\n",bh_insert(8, NULL, h));
	printf("%d\n",bh_insert(26, NULL, h));
	printf("%d\n",bh_insert(1, NULL, h));
	printf("%d\n",bh_insert(435, NULL, h));
	printf("%d\n",bh_insert(60, NULL, h));
	printf("%d\n",bh_insert(853, NULL, h));
	printf("%d\n",bh_insert(36, NULL, h));
	int i = 0;
	while (i < h->size) {
		printf("element %d value %d\n", i, h->data[i].key);
		i++;
	}
	printf("hello world\n");
	return 0;
}
