#include <stdio.h>
#include <vebtrees.h>
#include <math.h>

void printinfo(vebtree *tree);

int main(int argc, char **argv)
{
	printf("\nmaking vEB tree\n");
	vebtree *tree = veb_initialize(6, 4);
	veb_insert(10, NULL, tree);
	veb_insert(5, NULL, tree);
	veb_insert(15, NULL, tree);
	veb_insert(18, NULL, tree);
	printinfo(tree);
}

void printinfo(vebtree *tree){
	if (tree->n == 0)
		return;
	printf("Size %d, using %d bits. Currently containing %d elements\n", tree->size, tree->w, tree->n);
	if (tree->size > tree->threshold){
		printf("Recursive with:\n");
		printf("min: %d\nmax: %d\n", tree->min->value, tree->max->value);
		printf("with TOP structure: ");
		printinfo(tree->top);
		printf("with BOTTOM structures: ");
		int i;
		for (i = 0; i < pow(2, (tree->w)/2); i++){
			printinfo((tree->bottom)[i]);
		}
	}
	else {
		int i;
		printf("Leaf with elements: ");
		for (i = 0; i < tree->size; i++){
			printf("index %d: %d ;", i, (tree->arr)[i].value);
		}
		printf("\n\n");
	}
	
}
