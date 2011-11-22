#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include "BinaryHeap.h"
#include "FibonacciHeap.h"
#include "vebtrees.h"
#include "veb_pq.h"
#include "rb_tree.h"

#define ITR 10000000

void printinfo(int in, vebtree *tree);
void printtest(vebtree *tree);
void indent(int in);
void simpletest(vebtree *tree);
int main2(int argc, char **argv);
void testleafsize(int argc, char **argv);
void testcorrectness();

int main(int argc, char **argv){
	//vebtree *tree = veb_initialize(5, 4);
	//simpletest(tree);
	/*uint32_t * list = malloc(30 * sizeof(uint32_t));
	int i;
	for (i = 0; i < 30; i++)
		list[i] = 1;
	sort_rb(30, list);*/
	int i;
	for (i = 1024; i < 15000000; i *= 2){
		printf("\n\ntesting %d iterations\n",i);
		testcorrectness(i);
	}
	return 0;
}

void testcorrectness(int itr){
	int MAX = pow(2, 24);
	double
		vinit, binit, rinit,
		vins, bins, rins,
		vdm, bdm, rdm;
	clock_t start = clock();
	vebtree * vebt = veb_initialize(24, 64);
	clock_t end = clock();
	vinit = ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
	
	start = clock();
	binary_heap * bheap = bh_init_heap(MAX);
	end = clock();
	binit = ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
	//FibHeap * fheap = fib_make_heap();
	start = clock();
	rb_tree* rbt = rb_init(24, 64);
	end = clock();
	rinit = ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
	
	
	printf("vEB init: %f ms - bhinit: %f ms - rinit: %f ms\n", vinit, binit, rinit);
	int i;
	uint8_t * arr = calloc(MAX, sizeof(uint8_t));
	if (arr == NULL){
		printf("dang...\n");
		exit(1);
	}
	bins = 0;
	vins = 0;
	rins = 0;
	
	bdm = 0;
	vdm = 0;
	rdm = 0;
	for (i = 0; i < itr; i++){
		uint32_t s = random() % MAX;
		while(arr[s])
			s = random() % MAX;
		arr[s] = 1;
		start = clock();
		bh_insert(s, NULL, bheap);
		end = clock();
		bins += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
		start = clock();
		veb_insert(s, NULL, vebt);
		end = clock();
		vins += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
		//fib_insert(s, NULL, fheap);
		start = clock();
		rb_insert(s, rbt);
		end = clock();
		rins += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
	}
	printf("spend time inserting: vEB: %f ms - BH: %f ms - RB: %f ms\n", vins, bins, rins);
	printf("avg: vEB %f ms - BH: %f ms - RB: %f ms\n", vins/itr, bins/itr, rins/itr);
	uint32_t v, b, f, r;
	
	start = clock();
	rb_node* node = rbt->root;
	while(!is_leaf(node->left))
			node = node->left;
	rb_node* successor = node;
	end = clock();
	rdm += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
	
	for (i = 0; i < itr; i++){
		start = clock();
		b = bh_delete_min(bheap)->key;
		end = clock();
		bdm += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
		
		start = clock();
		v = vebt->min->value;
		veb_delete_min(vebt);
		end = clock();
		vdm += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
		
		start = clock();
		r = node->key;
		successor = rb_succ(node);
		rb_delete(node);
		node = successor;
		end = clock();
		rdm += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
		//v = fheap->min->key;
		//fib_delete_min(fheap);
		if (b != v)
			exit(-1);
		//printf("vEB: %d, bin: %d\n", v, b);
	}
	printf("spend time delete min: vEB: %f ms - BH: %f ms - RB: %f ms\n", vdm, bdm, rdm);
	printf("avg: vEB %f ms - BH: %f ms - RB: %f ms\n", vdm/itr, bdm/itr, rdm/itr);
	free(arr);
	bh_destruct(bheap);
	veb_destruct(vebt);
	rb_destruct(rbt);
}

void testleafsize(int argc, char **argv){
	if (argc != 3){
		printf("Please use 2 input values - first for the bitsize of the tree, and the second for the threashold. You can use -1 to let the system determine the best one\n");
		return;
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
				veb_delete_min(tree);
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
					veb_delete_min(tree);
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

void simpletest(vebtree *tree){
		printinfo(0, tree);
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
	printtest(tree);
}

void printtest(vebtree *tree){
	int32_t r;
	veb_findsucc(2, &r, tree);
	printf("\nsucc(2): %d\n", r);
	veb_findsucc(5, &r, tree);
	printf("succ(5): %d\n", r);
	veb_findsucc(7, &r, tree);
	printf("succ(7): %d\n", r);
	veb_findsucc(10, &r, tree);
	printf("succ(10): %d\n", r);
	veb_findsucc(12, &r, tree);
	printf("succ(12): %d\n", r);
	veb_findsucc(15, &r, tree);
	printf("succ(15): %d\n", r);
	veb_findsucc(17, &r, tree);
	printf("succ(17): %d\n", r);
	veb_findsucc(18, &r, tree);
	printf("succ(18): %d\n", r);
	veb_findsucc(20, &r, tree);
	printf("succ(20): %d\n", r);
	veb_findpred(2, &r, tree);
	printf("pred(2): %d\n", r);
	veb_findpred(5, &r, tree);
	printf("pred(5): %d\n", r);
	veb_findpred(7, &r, tree);
	printf("pred(7): %d\n", r);
	veb_findpred(10, &r, tree);
	printf("pred(10): %d\n", r);
	veb_findpred(12, &r, tree);
	printf("pred(12): %d\n", r);
	veb_findpred(15, &r, tree);
	printf("pred(15): %d\n", r);
	veb_findpred(17, &r, tree);
	printf("pred(17): %d\n", r);
	veb_findpred(18, &r, tree);
	printf("pred(18): %d\n", r);
	veb_findpred(20, &r, tree);
	printf("pred(20): %d\n\n", r);
	fflush(stdout); 
}

void printinfo(int in, vebtree *tree){
	//if (tree->n == 0)
	//	return;
	/*indent(in); printf("Size %d, using %d bits. Currently containing %d elements\n", tree->size, tree->w, tree->n);
	if (tree->size > 4){
		indent(in);printf("Recursive with:\n");
		indent(in);printf("min: %d\n", tree->min->value);
		indent(in);printf("max: %d\n", tree->max->value);
		indent(in);printf("sqrt: %d\n", tree->sqrtsize);
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
		indent(in);printf("min: %d\n", tree->min->value);
		indent(in);printf("max: %d\n", tree->max->value);
		indent(in);printf("sqrt: %d\n", tree->sqrtsize);
		indent(in);
		for (i = 0; i < tree->size; i++){
			printf("index %d: %d ;", i, (tree->arr)[i].value);
		}
		indent(in);printf("\n\n");
	}
	fflush(stdout);*/
}
void indent(int in){
	int i;
	for (i = 0; i < in; i++){
		printf(" ");
	}
}
