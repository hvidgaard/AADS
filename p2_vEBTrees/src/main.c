#include <stdio.h>
#include "vebtrees.h"
#include <math.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include "veb_pq.h"
#include "FibonacciHeap.h"
#include "BinaryHeap.h"

int main(int argc, char **argv);
void testcorrectnessveb();
void testcorrectnessvebpq();
void testVEBperformance_random_sort(int itr, int thres);
void testPQperformance_random(int itr);
void testperformancePQdijkstra();

int main(int argc, char **argv){
	if (argc < 2){
		printf("Start the program with one of the following commands:\n");
		printf(" 0: Test correctness of vEB\n\n");
		printf(" 1: Test correctness of vEB priority queue\n\n");
		printf(" 2: Test performance of vEB for sorting random elements,\n");
		printf("    compare with binary heap, fibonacci heap and Red Black trees.\n");
		printf("    NOTE: this use the bare vEB tree, and does not allow duplicated elements\n\n");
		printf(" 3: Test performance of vEB as a priority queue, with random elements,\n");
		printf("    compare with binary heap and fibonacci heap.\n");
		printf("    NOTE: this use the vEB_pq construction. It's slower than bare vEB,\n");
		printf("    but allow any number of duplicated elements.\n\n");
		printf(" 4: Test performance of vEB using Dijkstras algorithm,\n");
		printf("    compared with binary heap and fibonacci heap,\n");
		printf("    with test graph maximizing the decrease key stress on binary heap\n\n");
		printf(" 5: ALL OF THEM!\n\n");
		exit (0);
	}
	int testcase = atoi(argv[1]);
	int i, br;
	br = 1;
	switch (testcase){
		case 5:
			br = 0;
		case 0:
			printf("Testing correctness of vEB\n");
			testcorrectnessveb();
			if (br)
				break;
		case 1:
			printf("Testing correctness of vEB priority queue\n");
			testcorrectnessvebpq();
			if (br)
				break;
		case 2:
			printf("\nTesting VEB performance by sorting random elements\n");
			for (i = 4096; i < 10000000; i *= 2){
				printf("\nTesting with %d elements;\n",i);
				testVEBperformance_random_sort(i, 64);
			}
			if (br)
				break;
		case 3:
			printf("\nTesting vEB priority queue performance with random elements\n");
			for (i = 4096; i < 10000000; i *= 2){
				printf("\nTesting with %d elements;\n",i);
				testPQperformance_random(i);
			}
			if (br)
				break;
		case 4:
			printf("\nTesting vEB priority queue performance with Dijkstra\n NOT IMPLEMENTED!\n");
			//testperformancePQdijkstra();
			if (br)
				break;
		case 6:
			printf("Testing different leaf sizes\n");
			for (i = 8; i <= 4096; i *= 2){
				printf("\nTesting with leafsize %d ;\n",i);
				testVEBperformance_random_sort(10000000, i);
			}
			break;
		default:
			printf("Please provide an option between 0 and 5\n");
	}
	return 0;
}
void testcorrectnessveb(){
	int itr = 1000000;
	int MAX = pow(2, 24);
	vebtree * vebt = veb_initialize(24, 64);
	binary_heap * bheap = bh_init_heap(MAX);
	//FibHeap * fheap = fib_make_heap();
	
	uint8_t * arr = calloc(MAX, sizeof(uint8_t));
	int i;
	if (arr == NULL){
		printf("dang... could not allocate enough memory\n");
		exit(1);
	}
	for (i = 0; i < itr; i++){
		uint32_t s = random() % MAX;
		while(arr[s])
			s = random() % MAX;
		arr[s] = 1;
		veb_insert(s, NULL, vebt);
		bh_insert(s, NULL, bheap);
		//fib_insert(s, NULL, fheap);
	}
	uint32_t v, b; //f;
	bh_element * bnode;
	for (i = 0; i < itr; i++){
		v = vebt->min->value;
		veb_delete_min(vebt);
		bnode = bh_delete_min(bheap);
		b = bnode->key;
		free(b);
		//v = fheap->min->key;
		//fib_delete_min(fheap);
		if (b != v){
			printf("one of the datastructures was not correct\n");
			printf("vEB: %d, bin: %d\n", v, b);
			exit(-1);
		}
	}
	printf("all data structures agree, so they can be assumed correct\n");
	free(arr);
	veb_destruct(vebt);
	bh_destruct(bheap);
}
void testcorrectnessvebpq(){
	int MAX = pow(2, 24);
	vebtree * vebt = veb_pq_init(24);
	binary_heap * bheap = bh_init_heap(MAX);
	//FibHeap * fheap = fib_make_heap();
	
	int i;
	veb_pq_node * n;
	bh_element * nn;
	for (i = 0; i < 10000000; i++){
		uint32_t s = random() % MAX;
		veb_pq_node * n = malloc(sizeof(veb_pq_node));
		n->node_prio = s;
		veb_pq_insert(n, vebt);
		bh_insert(s, NULL, bheap);
		//fib_insert(s, NULL, fheap);
	}
	uint32_t v, b; //f;
	for (i = 0; i < 10000000; i++){
		v = vebt->min->value;
		n = veb_pq_deletemin(vebt);;
		free(n);
		nn = bh_delete_min(bheap);
		b = nn->key;
		free(nn);
		//v = fheap->min->key;
		//fib_delete_min(fheap);
		if (b != v){
			printf("one of the datastructures was not correct\n");
			printf("vEB: %d, bin: %d\n", v, b);
			exit(-1);
		}
	}
	printf("all data structures agree, so they can be assumed correct\n");
	veb_destruct(vebt);
	bh_destruct(bheap);
}
void testPQperformance_random(int itr){
	int MAX = pow(2, 24);
	double vinit, binit, vins, bins, vdm, bdm;
	clock_t start = clock();
	
	vebtree * vebt = veb_pq_init(24);
	clock_t end = clock();
	vinit = ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
	
	start = clock();
	binary_heap * bheap = bh_init_heap(itr);
	end = clock();
	binit = ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
	//FibHeap * fheap = fib_make_heap();
	
	
	printf("vEB init: %f ms - bhinit: %f ms\n", vinit, binit);
	int i;
	vins = 0;
	bins = 0;
	vdm = 0;
	bdm = 0;
	veb_pq_node * n;
	bh_element * nn;
	for (i = 0; i < itr; i++){
		uint32_t s = random() % MAX;
		veb_pq_node * n = malloc(sizeof(veb_pq_node));
		n->node_prio = s;
		n->node_nr = i;
		start = clock();
		veb_pq_insert(n, vebt);
		end = clock();
		vins += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
		start = clock();
		bh_insert(s, NULL, bheap);
		end = clock();
		bins += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
		//fib_insert(s, NULL, fheap);
	}
	printf("spend time inserting: vEB: %f ms - BH: %f ms\n", vins, bins);
	printf("avg: vEB %f ms - BH: %f ms\n", vins/itr, bins/itr);
	uint32_t v, b; //f;
	for (i = 0; i < itr; i++){
		v = vebt->min->value;
		start = clock();
		n = veb_pq_deletemin(vebt);;
		end = clock();
		free(n);
		vdm += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
		start = clock();
		nn = bh_delete_min(bheap);
		end = clock();
		b = nn->key;
		free(nn);
		bdm += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
		//v = fheap->min->key;
		//fib_delete_min(fheap);
		if (b != v){
			printf("vEB: %d, bin: %d\n", v, b);
			exit(-1);
		}
			
		
	}
	printf("spend time delete min: vEB: %f ms - BH: %f ms\n", vdm, bdm);
	printf("avg: vEB %f ms - BH: %f ms\n", vdm/itr, bdm/itr);
	veb_destruct(vebt);
	bh_destruct(bheap);
}
void testVEBperformance_random_sort(int itr, int thres){
	int MAX = pow(2, 24);
	double vinit, binit, vins, bins, vdm, bdm;
	clock_t start = clock();
	vebtree * vebt = veb_initialize(24, thres);
	clock_t end = clock();
	vinit = ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
	
	start = clock();
	binary_heap * bheap = bh_init_heap(MAX);
	end = clock();
	binit = ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
	//FibHeap * fheap = fib_make_heap();
	
	
	printf("vEB init: %f ms - bhinit: %f ms\n", vinit, binit);
	int i;
	uint8_t * arr = calloc(MAX, sizeof(uint8_t));
	if (arr == NULL){
		printf("dang...could not allocate enough memory\n");
		exit(1);
	}
	vins = 0;
	bins = 0;
	vdm = 0;
	bdm = 0;
	for (i = 0; i < itr; i++){
		uint32_t s = random() % MAX;
		while(arr[s])
			s = random() % MAX;
		arr[s] = 1;
		start = clock();
		veb_insert(s, NULL, vebt);
		end = clock();
		vins += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
		start = clock();
		bh_insert(s, NULL, bheap);
		end = clock();
		bins += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
		//fib_insert(s, NULL, fheap);
	}
	printf("spend time inserting: vEB: %f ms - BH: %f ms\n", vins, bins);
	printf("avg: vEB %f ms - BH: %f ms\n", vins/itr, bins/itr);
	uint32_t v, b; //f;
	bh_element * bnode;
	for (i = 0; i < itr; i++){
		start = clock();
		v = vebt->min->value;
		veb_delete_min(vebt);
		end = clock();
		vdm += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
		start = clock();
		bnode = bh_delete_min(bheap);
		end = clock();
		b = bnode->key;
		free(b);
		bdm += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
		//v = fheap->min->key;
		//fib_delete_min(fheap);
		if (b != v)
			exit(-1);
		//printf("vEB: %d, bin: %d\n", v, b);
	}
	printf("spend time delete min: vEB: %f ms - BH: %f ms\n", vdm, bdm);
	printf("avg: vEB %f ms - BH: %f ms\n", vdm/itr, bdm/itr);
	free(arr);
	veb_destruct(vebt);
	bh_destruct(bheap);
}



















/*
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
	/*while(cont){
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
//}
/*void indent(int in){
	int i;
	for (i = 0; i < in; i++){
		printf(" ");
	}
}*/
