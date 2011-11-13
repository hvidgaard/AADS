#include <stdio.h>
#include <vebtrees.h>
#include <math.h>

void printinfo(int in, vebtree *tree);
void printtest(vebtree *tree);
void indent(int in);

int main(int argc, char **argv)
{
	printf("\nmaking vEB tree\n");
	vebtree *tree = veb_initialize(26, 16);
	/*printinfo(0, tree);
	printf("inserting 10\n");
	veb_insert(10, NULL, tree);
	printinfo(0, tree);
	printf("inserting 5\n");
	veb_insert(5, NULL, tree);
	printinfo(0, tree);
	printf("inserting 18\n");
	veb_insert(18, NULL, tree);
	printinfo(0, tree);
	printf("inserting 15\n");
	veb_insert(15, NULL, tree);
	printinfo(0, tree);
	printf("\n5, 10, 15, 18");
	printtest(tree);
	printf("deleting 5\n");
	veb_delete(5, tree);
	printinfo(0, tree);
	printf("\n10, 15, 18");
	printtest(tree);
	printf("inserting 5\n");
	veb_insert(5, NULL, tree);
	printinfo(0, tree);
	printf("\n5, 10, 15, 18");
	printtest(tree);
	printf("deleting 10\n");
	veb_delete(10, tree);
	printinfo(0, tree);
	printf("\n5, 15, 18");
	printtest(tree);
	printf("inserting 10\n");
	veb_insert(10, NULL, tree);
	printinfo(0, tree);
	printf("\n5, 10, 15, 18");
	printtest(tree);
	printf("deleting 15\n");
	veb_delete(15, tree);
	printinfo(0, tree);
	printf("\n5, 10, 18");
	printtest(tree);
	printf("inserting 15\n");
	veb_insert(15, NULL, tree);
	printinfo(0, tree);
	printf("\n5, 10, 15, 18");
	printtest(tree);
	printf("deleting 18\n");
	veb_delete(18, tree);
	printinfo(0, tree);
	printf("\n5, 10, 15");
	printtest(tree);
	printf("inserting 18\n");
	veb_insert(18, NULL, tree);
	printf("\n5, 10, 15, 18");
	printtest(tree);
	printf("deleting 5\n");
	veb_delete(5, tree);
	printf("deleting 18\n");
	veb_delete(18, tree);
	printf("\n10, 15");
	printtest(tree);*/
	int i;
	for (i = 0; i < tree->size; i++)
		veb_insert(i, NULL, tree);
	for (i = 0; i < tree->size; i++)
		veb_delete(i,tree);
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

void printinfo(int in, vebtree *tree){
	//if (tree->n == 0)
	//	return;
	/*indent(in); printf("Size %d, using %d bits. Currently containing %d elements\n", tree->size, tree->w, tree->n);
	if (tree->size > tree->threshold){
		indent(in);printf("Recursive with:\n");
		indent(in);printf("min: %d\n", tree->min.value);
		indent(in);printf("max: %d\n", tree->max.value);
		indent(in);printf("with TOP structure: \n");
		printinfo(in+4, tree->top);
		indent(in);printf("with BOTTOM structures: \n");
		int i;
		for (i = 0; i < pow(2, (tree->w)/2); i++){
			printinfo(in+4, (tree->bottom)[i]);
		}
	}
	else {
		int i;
		indent(in);printf("Leaf with elements: \n");
		indent(in);
		for (i = 0; i < tree->size; i++){
			printf("index %d: %d ;", i, (tree->arr)[i].value);
		}
		indent(in);printf("\n\n");
	}
	fflush(stdout); */
}
void indent(int in){
	int i;
	for (i = 0; i < in; i++){
		printf(" ");
	}
}
