#include <stdio.h>
#include "vebtrees.h"
#include <math.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

void printinfo(int in, vebtree *tree);
void printtest(vebtree *tree);
void indent(int in);

int main(int argc, char **argv)
{
	if (argc != 3){
		printf("Please use 2 input values - first for the bitsize of the tree, and the second for the threashold. You can use -1 to let the system determine the best one\n");
		return 1;
	}
	int thres = 4;
	int cont = 1;

	/*int i, index;
	vebtree *tree = veb_initialize((unsigned int)strtol(argv[1], NULL, 10), (unsigned int)strtol(argv[2], NULL, 10));
	srand(100);
				for (i = 0; i < (tree->size)/10; i++){
					index = rand() % tree->size;
					veb_insert(index, NULL, tree);
				}
				for (i = 0; i < tree->size; i++){
					index = rand() % tree->size;
					veb_findsucc(index, NULL, tree);
				}
				for (i = 0; i < tree->size; i++){
					index = rand() % tree->size;
					veb_findpred(index, NULL, tree);
				}
				while (tree->n){
					veb_delete_min(tree);
				}*/
	while(cont){
		pid_t pid = fork();
		if (pid){
			if (thres > 16000)
				cont = 0;
			else if (thres == -1)
				thres = 2;
			else
				thres = thres * 2;
			if (waitpid(pid, NULL, 0) == -1)
				printf("erm, something gik galt");
		}
		else{
			printf("Using %10d for threshold - ", thres);
			int i, j;
			srand(100);
			uint32_t index;
			vebtree *tree = veb_initialize((unsigned int)strtol(argv[1], NULL, 10), thres);
			for (i = 0; i < (tree->size)/10; i++){
				index = rand() % tree->size;
				veb_insert(index, NULL, tree);
			}
			for (i = 0; i < tree->size; i++){
				index = rand() % tree->size;
				veb_findsucc(index, NULL, tree);
			}
			for (i = 0; i < tree->size; i++){
				//printf("ERROR %d\n", i);
				index = rand() % tree->size;
				veb_findpred(index, NULL, tree);
			}
			fflush(stdout);
			while (tree->n)
				veb_delete_min(tree, NULL);
			fflush(stdout);
			clock_t a, t;
			a = t = 0;
			for (j = 0; j < 10; j++){
				srand(100);
				for (i = 0; i < (tree->size)/10; i++){
					index = rand() % tree->size;
					a = clock();
					veb_insert(index, NULL, tree);
					t += clock() - a;
				}
				for (i = 0; i < tree->size; i++){
					index = rand() % tree->size;
					a = clock();
					veb_findsucc(index, NULL, tree);
					t += clock() - a;
				}
				for (i = 0; i < tree->size; i++){
					index = rand() % tree->size;
					a = clock();
					veb_findpred(index, NULL, tree);
					t += clock() - a;
				}
				while (tree->n){
					a = clock();
					veb_delete_min(tree, NULL);
					t += clock() - a;
				}
			}
			double running_time = (double) (t) / (double) CLOCKS_PER_SEC *1000 / 10;
			printf("Time: %10gms\n", running_time);
			fflush(stdout);
			cont = 0;
		}
	}
}
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


void printtest(vebtree *tree){
	printf("\nsucc(2): %d\n", veb_findsucc(2, NULL, tree));
	printf("succ(5): %d\n", veb_findsucc(5, NULL, tree));
	printf("succ(7): %d\n", veb_findsucc(7, NULL, tree));
	printf("succ(10): %d\n", veb_findsucc(10, NULL, tree));
	printf("succ(12): %d\n", veb_findsucc(12, NULL, tree));
	printf("succ(15): %d\n", veb_findsucc(15, NULL, tree));
	printf("succ(17): %d\n", veb_findsucc(17, NULL, tree));
	printf("succ(18): %d\n", veb_findsucc(18, NULL, tree));
	printf("succ(20): %d\n", veb_findsucc(20, NULL, tree));
	printf("pred(2): %d\n", veb_findpred(2, NULL, tree));
	printf("pred(5): %d\n", veb_findpred(5, NULL, tree));
	printf("pred(7): %d\n", veb_findpred(7, NULL, tree));
	printf("pred(10): %d\n", veb_findpred(10, NULL, tree));
	printf("pred(12): %d\n", veb_findpred(12, NULL, tree));
	printf("pred(15): %d\n", veb_findpred(15, NULL, tree));
	printf("pred(17): %d\n", veb_findpred(17, NULL, tree));
	printf("pred(18): %d\n", veb_findpred(18, NULL, tree));
	printf("pred(20): %d\n\n", veb_findpred(20, NULL, tree));
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
