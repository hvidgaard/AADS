#include <stdio.h>
#include <vebtrees.h>
#include <math.h>

void printinfo(vebtree *tree);
void printtest(vebtree *tree);
void print_n(vebtree *tree);

int main(int argc, char **argv)
{
	printf("\nmaking vEB tree\n");
	vebtree *tree = veb_initialize(6, 4);
	print_n(tree);
	veb_insert(10, NULL, tree);
	print_n(tree);
	veb_insert(5, NULL, tree);
	print_n(tree);
	veb_insert(18, NULL, tree);
	print_n(tree);
	veb_insert(15, NULL, tree);
	print_n(tree);
	printf("\n5, 10, 15, 18");
	printtest(tree);
	veb_delete(5, tree);
	print_n(tree);
	printf("\n10, 15, 18");
	printtest(tree);
	veb_insert(5, NULL, tree);
	print_n(tree);
	printf("\n5, 10, 15, 18");
	printtest(tree);
	veb_delete(10, tree);
	print_n(tree);
	printf("\n5, 15, 18");
	printtest(tree);
	veb_insert(10, NULL, tree);
	print_n(tree);
	printf("\n5, 10, 15, 18");
	printtest(tree);
	veb_delete(15, tree);
	print_n(tree);
	printf("\n5, 10, 18");
	printtest(tree);
	veb_insert(15, NULL, tree);
	print_n(tree);
	printf("\n5, 10, 15, 18");
	printtest(tree);
	veb_delete(18, tree);
	print_n(tree);
	printf("\n5, 10, 15");
	printtest(tree);
	veb_insert(18, NULL, tree);
	printf("\n5, 10, 15, 18");
	printtest(tree);
	veb_delete(5, tree);
	veb_delete(18, tree);
	printf("\n10, 15");
	printtest(tree);
}

void print_n(vebtree *tree){
	if (tree->size < tree->threshold){
		printf("leaf! n=%d \n", tree->n);
		fflush(stdout);	
	}
	else {
		printf("recursive! n=%d \n", tree->n);
		printf("TOP:\n");
		fflush(stdout);
		print_n(tree->top);
		printf("BOTTOM:\n");
		fflush(stdout);
		int i;
		for (i = 0; i < tree->sqrtsize; i++)
			printf(" b%d:", i);
			fflush(stdout);
			print_n((tree->bottom)[i]);
	}
	fflush(stdout);
}

void printtest(vebtree *tree){
	printf("\nsucc(2): %d\n", veb_findsucc(2, tree));
	printf("succ(5): %d\n", veb_findsucc(5, tree));
	printf("succ(7): %d\n", veb_findsucc(7, tree));
	printf("succ(10): %d\n", veb_findsucc(10, tree));
	printf("succ(12): %d\n", veb_findsucc(12, tree));
	printf("succ(15): %d\n", veb_findsucc(15, tree));
	printf("succ(17): %d\n", veb_findsucc(17, tree));
	printf("succ(18): %d\n", veb_findsucc(18, tree));
	printf("succ(20): %d\n", veb_findsucc(20, tree));
	printf("pred(2): %d\n", veb_findpred(2, tree));
	printf("pred(5): %d\n", veb_findpred(5, tree));
	printf("pred(7): %d\n", veb_findpred(7, tree));
	printf("pred(10): %d\n", veb_findpred(10, tree));
	printf("pred(12): %d\n", veb_findpred(12, tree));
	printf("pred(15): %d\n", veb_findpred(15, tree));
	printf("pred(17): %d\n", veb_findpred(17, tree));
	printf("pred(18): %d\n", veb_findpred(18, tree));
	printf("pred(20): %d\n\n", veb_findpred(20, tree));
	fflush(stdout); 
}

void printinfo(vebtree *tree){
	if (tree->n == 0)
		return;
	printf("Size %d, using %d bits. Currently containing %d elements\n", tree->size, tree->w, tree->n);
	if (tree->size > tree->threshold){
		printf("Recursive with:\n");
		printf("min: %d\nmax: %d\n", tree->min.value, tree->max.value);
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
	fflush(stdout); 
}
