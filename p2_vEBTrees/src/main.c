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
#include "graph_generators.h"

#ifndef UINT_MAX
#define UINT_MAX 65535 //2^16-1
#endif

int main(int argc, char **argv);
void testcorrectnessveb();
void testcorrectnessvebpq();
void testVEBperformance_random_sort(int itr, int thres);
void testVEBperformance_leaf(int itr, int thres);
void testPQperformance_random(int itr);
void testperformancePQdijkstra(int itr);
void time_veb_dijkstra(uint32_t num_vertices, uint32_t source, uint32_t * weights, uint32_t ** edges);
void time_bin_dijkstra(uint32_t num_vertices, uint32_t source, uint32_t * weights, uint32_t ** edges);
void time_fib_dijkstra(uint32_t num_vertices, uint32_t source, uint32_t * weights, uint32_t ** edges);

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
		printf(" 5: Test performance of vEB with different leaf sizes\n\n");
		printf(" 6: ALL OF THEM!\n\n");
		exit (0);
	}
	int testcase = atoi(argv[1]);
	int i, br;
	br = 1;
	switch (testcase){
		case 6:
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
			for (i = 4096; i < 20000000; i *= 2){
				printf("\nTesting with %d elements;\n",i);
				testVEBperformance_random_sort(i, 64);
			}
			if (br)
				break;
		case 3:
			printf("\nTesting vEB priority queue performance with random elements\n");
			for (i = 4096; i < 20000000; i *= 2){
				printf("\nTesting with %d elements;\n",i);
				testPQperformance_random(i);
			}
			if (br)
				break;
		case 4:
			printf("\nTesting vEB priority queue performance with Dijkstra\n");
			for (i = 1000; i < 24000; i += 1000){
				testperformancePQdijkstra(i);
			}
			if (br)
				break;
		case 5:
			printf("Testing different leaf sizes\n");
			for (i = 8; i <= 4096; i *= 2){
				printf("\nTesting with leafsize %d ;\n",i);
				testVEBperformance_leaf(10000000, i);
				//testVEBperformance_random_sort(10000000, i);
			}
			break;
		default:
			printf("Please provide an option between 0 and 5\n");
	}
	return 0;
}
void testcorrectnessveb(){
	int itr = 10;
	int MAX = pow(2, 24);
	vebtree * vebt = veb_initialize(24, 64);
	binary_heap * bheap = bh_init_heap(MAX);
	FibHeap * fheap = fib_make_heap();
	
	uint8_t * arr = calloc(MAX, sizeof(uint8_t));
	int i;
	if (arr == NULL){	
		printf("dang... could not allocate enough memory\n");
		exit(1);
	}
	bh_element * e;
	for (i = 0; i < itr; i++){
		uint32_t s = random() % MAX;
		while(arr[s])
			s = random() % MAX;
		arr[s] = 1;
		veb_insert(s, NULL, vebt);
		bh_insert(s, NULL, bheap);
		fib_insert(s, NULL, fheap);
	}
	uint32_t v, b, f;
	FibNode * fn;
	for (i = 0; i < itr; i++){
		v = vebt->min->value;
		veb_delete_min(vebt);
		e = bh_delete_min(bheap);
		b = e->key;
		free(e);
		fn = fib_find_min(fheap);
		f = fn->key;
		fib_delete_min(fheap);
		free(fn);
		if (b != v || b != f || v !=f){
			printf("one of the datastructures was not correct\n");
			printf("vEB: %d, bin: %d, fib: %d\n", v, b, f);
			exit(-1);
		}
	}
	printf("all data structures agree, so they can be assumed correct - %d -\n", bheap->size);
	free(arr);
	veb_destruct(vebt);
	bh_destruct(bheap);
	free(fheap);
}
void testcorrectnessvebpq(){
	int itr = 10000000;
	int MAX = pow(2, 24);
	vebtree * vebt = veb_pq_init(24);
	binary_heap * bheap = bh_init_heap(MAX);
	FibHeap * fheap = fib_make_heap();
	
	int i;
	veb_pq_node * n;
	bh_element * e;
	for (i = 0; i < itr; i++){
		uint32_t s = random() % MAX;
		veb_pq_node * n = malloc(sizeof(veb_pq_node));
		n->node_prio = s;
		veb_pq_insert(n, vebt);
		bh_insert(s, NULL, bheap);
		fib_insert(s, NULL, fheap);
	}
	uint32_t v, b, f;
	FibNode * fn;
	for (i = 0; i < itr; i++){
		v = vebt->min->value;
		n = veb_pq_deletemin(vebt);;
		free(n);
		e = bh_delete_min(bheap);
		b = e->key;
		free(e);
		fn = fib_find_min(fheap);
		f = fn->key;
		fib_delete_min(fheap);
		free(fn);
		if (b != v || b != f || v !=f){
			printf("one of the datastructures was not correct\n");
			printf("vEB: %d, bin: %d\n", v, b);
			exit(-1);
		}
	}
	printf("all data structures agree, so they can be assumed correct\n");
	veb_destruct(vebt);
	bh_destruct(bheap);
	free(fheap);
}
void testPQperformance_random(int itr){
	int MAX = pow(2, 24);
	double vinit, binit, finit, vins, bins, fins, vdm, bdm, fdm;
	
	clock_t start = clock();
	vebtree * vebt = veb_pq_init(24);
	clock_t end = clock();
	vinit = ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
	
	start = clock();
	binary_heap * bheap = bh_init_heap(itr);
	end = clock();
	binit = ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
	
	start = clock();
	FibHeap * fheap = fib_make_heap();
	end = clock();
	finit = ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
	
	printf("vEB init: %f ms - bhinit: %f ms - fibinit: %f ms\n", vinit, binit, finit);
	int i;
	vins = 0;
	bins = 0;
	fins = 0;
	vdm = 0;
	bdm = 0;
	fdm = 0;
	veb_pq_node * n;
	bh_element * e;
	FibNode * fn;
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
		
		start = clock();
		fib_insert(s, NULL, fheap);
		end = clock();
		fins += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
	}
	printf("spend time inserting: vEB: %f ms (avg %f) - BH: %f ms (avg %f) - fib: %f ms (avg %f)\n", vins, vins/itr, bins, bins/itr, fins, fins/itr);
	//printf("avg: vEB %f ms - BH: %f ms\n", vins/itr, bins/itr);
	uint32_t v, b, f;
	for (i = 0; i < itr; i++){
		v = vebt->min->value;
		
		start = clock();
		n = veb_pq_deletemin(vebt);;
		end = clock();
		free(n);
		vdm += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
		
		start = clock();
		e = bh_delete_min(bheap);
		end = clock();
		b = e->key;
		free(e);
		bdm += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
		fn = fib_find_min(fheap);
		f = fn->key;
		
		start = clock();
		fib_delete_min(fheap);
		end = clock();
		fdm += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
		free(fn);
		if (b != v || b != f || v != f){
			printf("vEB: %d, bin: %d\n", v, b);
			exit(-1);
		}
	}
	printf("spend time deletemin: vEB: %f ms (avg %f) - BH: %f ms (avg %f) - fib: %f ms (avg %f)\n", vdm, vdm/itr, bdm, bdm/itr, fdm, fdm/itr);
	//printf("avg: vEB %f ms - BH: %f ms\n", vdm/itr, bdm/itr);
	veb_destruct(vebt);
	bh_destruct(bheap);
	free(fheap);
}
void testVEBperformance_random_sort(int itr, int thres){
	int MAX = pow(2, 24);
	double vinit, binit, finit, vins, bins, fins, vdm, bdm, fdm;
	
	clock_t start = clock();
	vebtree * vebt = veb_initialize(24, thres);
	clock_t end = clock();
	vinit = ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
	
	start = clock();
	binary_heap * bheap = bh_init_heap(MAX);
	end = clock();
	binit = ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
	
	start = clock();
	FibHeap * fheap = fib_make_heap();
	end = clock();
	finit = ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
	
	printf("vEB init: %f ms - bhinit: %f ms - fibinit: %f ms\n", vinit, binit, finit);
	int i;
	uint8_t * arr = calloc(MAX, sizeof(uint8_t));
	if (arr == NULL){
		printf("dang...could not allocate enough memory\n");
		exit(1);
	}
	vins = 0;
	bins = 0;
	fins = 0;
	vdm = 0;
	bdm = 0;
	fdm = 0;
	bh_element *e;
	FibNode * fn;
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
		
		start = clock();
		fib_insert(s, NULL, fheap);
		end = clock();
		fins += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
	}
	printf("spend time inserting: vEB: %f ms (avg %f) - BH: %f ms (avg %f) - fib: %f ms (avg %f)\n", vins, vins/itr, bins, bins/itr, fins, fins/itr);
	//printf("avg: vEB %f ms - BH: %f ms\n", vins/itr, bins/itr);
	uint32_t v, b, f;
	for (i = 0; i < itr; i++){
		
		start = clock();
		v = vebt->min->value;
		veb_delete_min(vebt);
		end = clock();
		vdm += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
		
		start = clock();
		e= bh_delete_min(bheap);
		end = clock();
		b = e->key;
		free(e);
		bdm += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
		fn = fib_find_min(fheap);
		f = fn->key;
		
		start = clock();
		fib_delete_min(fheap);
		end = clock();
		fdm += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
		free(fn);
		if (b != v)
			exit(-1);
		//printf("vEB: %d, bin: %d\n", v, b);
	}
	printf("spend time deletemin: vEB: %f ms (avg %f) - BH: %f ms (avg %f) - fib: %f ms (avg %f)\n", vdm, vdm/itr, bdm, bdm/itr, fdm, fdm/itr);
	//printf("avg: vEB %f ms - BH: %f ms\n", vdm/itr, bdm/itr);
	free(arr);
	veb_destruct(vebt);
	bh_destruct(bheap);
	free(fheap);
}
void testVEBperformance_leaf(int itr, int thres){
	int MAX = pow(2, 24);
	double vinit, vins, vdm;
	
	clock_t start = clock();
	vebtree * vebt = veb_initialize(24, thres);
	clock_t end = clock();
	vinit = ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
	
	printf("vEB init: %f ms\n", vinit);
	int i;
	uint8_t * arr = calloc(MAX, sizeof(uint8_t));
	if (arr == NULL){
		printf("dang...could not allocate enough memory\n");
		exit(1);
	}
	vins = 0;
	vdm = 0;
	for (i = 0; i < itr; i++){
		uint32_t s = random() % MAX;
		while(arr[s])
			s = random() % MAX;
		arr[s] = 1;
		
		start = clock();
		veb_insert(s, NULL, vebt);
		end = clock();
		vins += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
		
	}
	printf("spend time inserting: vEB: %f ms (avg %f)\n", vins, vins/itr);
	//printf("avg: vEB %f ms - BH: %f ms\n", vins/itr, bins/itr);
	uint32_t v;
	for (i = 0; i < itr; i++){
		
		start = clock();
		v = vebt->min->value;
		veb_delete_min(vebt);
		end = clock();
		vdm += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
	}
	printf("spend time deletemin: vEB: %f ms (avg %f)", vdm, vdm/itr);
	//printf("avg: vEB %f ms - BH: %f ms\n", vdm/itr, bdm/itr);
	free(arr);
	veb_destruct(vebt);
}
void testperformancePQdijkstra(int size){
	int seed = 1234;
	uint32_t* weights = generate_decrease_key_max_graph_2(size, 1000, seed);
	uint32_t** edges = malloc((size+1) * sizeof(uint32_t *));
	uint32_t *t_edges = malloc(size * sizeof(uint32_t));
	uint32_t i, j;
	for (i = 0; i < size; i++) {
		uint32_t count = 0;
		for (j = 0; j < size; j++){
			if (weights[(i * size) + j])
				t_edges[++count] = j;
		}
		edges[i] = malloc((count+1) * sizeof(uint32_t));
		edges[i][0] = count;
		for (j = 1; j <= count; j++)
			edges[i][j] = t_edges[j];
	}
	free(t_edges);
	
	printf("now testing vEB PQ: size = %d\n", size);
	printf("-------------------\n");
	time_veb_dijkstra(size, 0, weights, edges);
	printf("now testing binary PQ: size = %d\n", size);
	printf("----------------------\n");
	time_bin_dijkstra(size, 0, weights, edges);
	printf("now testing fibonacci PQ: size = %d\n", size);
	printf("-------------------------\n");
	time_fib_dijkstra(size, 0, weights, edges);
	for (i = 0; i < size; i++)
		free(edges[i]);
	free(edges);
	free(weights);
}

void time_bin_dijkstra(uint32_t num_vertices, uint32_t source, uint32_t * weights, uint32_t ** edges){
	clock_t start, end;
	double binit = 0;
	double bdm = 0;
	double bdk = 0;
	double bins = 0;
	
	start = clock();
	binary_heap * heap = bh_init_heap(num_vertices);
	end = clock();
	binit = ((double) (end-start) / CLOCKS_PER_SEC) * 1000;

	
	uint32_t *distances = malloc(num_vertices * sizeof(uint32_t));
	bh_element ** vertices = malloc(num_vertices * sizeof(bh_element *));
	
	uint32_t distance;
	uint32_t *data;
	uint32_t i;
	for (i = 0; i < num_vertices; i++) {
		if(i == source)
			distance = 0;
		else
			distance = UINT_MAX;
		distances[i] = distance;
		data = malloc(sizeof(uint32_t));
		*data = i;
		start = clock();
		vertices[i] = bh_insert(distance, data, heap);
		end = clock();
		bins += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
		
	}
	bh_element *node;
	uint32_t decrease_key_calls = 0;
	
	start = clock();
	node = bh_delete_min(heap);
	end = clock();
	bdm += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
	while (node) {
		uint32_t u = *((uint32_t *)node->data);
		for (i = 1; i <= edges[u][0]; i++) {
			uint32_t v = edges[u][i];
			uint32_t alt = distances[u] + weights[u * num_vertices + v];
			if (alt < distances[v]) {
				start = clock();
				bh_decrease_key(distances[v] - alt, vertices[v], heap);
				end = clock();
				bdk += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
				distances[v] = alt;
				decrease_key_calls++;
			}
		}
		free(node->data);
		free(node);
		start = clock();
		node = bh_delete_min(heap);
		end = clock();
		bdm += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
	}
	bh_destruct(heap);
	free(vertices);
	free(distances);
	printf("bin: init: %f - total time: %f\n", binit, binit+bdm+bins+bdk);
	printf("     insert: %f (avg: %f)\n", bins, bins/num_vertices);
	printf("     delmin: %f (avg: %f)\n", bdm, bdm/num_vertices);
	printf("     dec.ke: %f (avg: %f)\n\n", bdk, bdk/decrease_key_calls);
}
void time_veb_dijkstra(uint32_t num_vertices, uint32_t source, uint32_t * weights, uint32_t ** edges){
	clock_t start, end;
	double vinit = 0;
	double vdm = 0;
	double vdk = 0;
	double vins = 0;
	
	start = clock();
	vebtree * heap = veb_pq_init(24);
	end = clock();
	vinit = ((double) (end-start) / CLOCKS_PER_SEC) * 1000;

	
	uint32_t *distances = malloc(num_vertices * sizeof(uint32_t));
	veb_pq_node ** vertices = malloc(num_vertices * sizeof(veb_pq_node *));
	
	uint32_t distance;
	uint32_t i;
	veb_pq_node * n;
	for (i = 0; i < num_vertices; i++) {
		if(i == source)
			distance = 0;
		else
			distance = UINT_MAX;
		distances[i] = distance;
		n = malloc(sizeof(veb_pq_node));
		n->node_prio = distance;
		n->node_nr = i;
		vertices[i] = n;
		start = clock();
		veb_pq_insert(n, heap);
		end = clock();
		vins += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
		
	}
	uint32_t decrease_key_calls = 0;
	
	start = clock();
	n = veb_pq_deletemin(heap);
	end = clock();
	vdm += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
	while (n) {
		uint32_t u = n->node_nr;
		for (i = 1; i <= edges[u][0]; i++) {
			uint32_t v = edges[u][i];
			uint32_t alt = distances[u] + weights[u * num_vertices + v];
			if (alt < distances[v]) {
				start = clock();
				veb_pq_decrease_key(heap, vertices[v], distances[v] - alt);
				end = clock();
				vdk += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
				distances[v] = alt;
				decrease_key_calls++;
			}
		}
		start = clock();
		n = veb_pq_deletemin(heap);
		end = clock();
		vdm += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
	}
	for (i = 0; i < num_vertices; i++)
		free(vertices[i]);
	veb_destruct(heap);
	free(vertices);
	free(distances);
	printf("veb: init: %f - total time: %f\n", vinit, vinit+vdm+vins+vdk);
	printf("     insert: %f (avg: %f)\n", vins, vins/num_vertices);
	printf("     delmin: %f (avg: %f)\n", vdm, vdm/num_vertices);
	printf("     dec.ke: %f (avg: %f)\n\n", vdk, vdk/decrease_key_calls);
	
}
void time_fib_dijkstra(uint32_t num_vertices, uint32_t source, uint32_t * weights, uint32_t ** edges){
	clock_t start, end;
	double finit = 0;
	double fdm = 0;
	double fdk = 0;
	double fins = 0;
	
	start = clock();
	FibHeap * heap = fib_make_heap();
	end = clock();
	finit = ((double) (end-start) / CLOCKS_PER_SEC) * 1000;

	
	uint32_t *distances = malloc(num_vertices * sizeof(uint32_t));
	FibNode ** vertices = malloc(num_vertices * sizeof(FibNode *));
	
	uint32_t distance;
	uint32_t *data;
	uint32_t i;
	FibNode * n;
	for (i = 0; i < num_vertices; i++) {
		if(i == source)
			distance = 0;
		else
			distance = UINT_MAX;
		distances[i] = distance;
		data = malloc(sizeof(uint32_t));
		*data = i;
		start = clock();
		vertices[i] = fib_insert(distance, data, heap);
		end = clock();
		fins += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
		
	}
	uint32_t decrease_key_calls = 0;
	n = fib_find_min(heap);
	start = clock();
	fib_delete_min(heap);
	end = clock();
	fdm += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
	while (n) {
		uint32_t u = *((uint32_t *)n->data);
		for (i = 1; i <= edges[u][0]; i++) {
			uint32_t v = edges[u][i];
			uint32_t alt = distances[u] + weights[u * num_vertices + v];
			if (alt < distances[v]) {
				start = clock();
				fib_decrease_key(distances[v] - alt, vertices[v], heap);
				end = clock();
				fdk += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
				distances[v] = alt;
				decrease_key_calls++;
			}
		}
		n = fib_find_min(heap);
		start = clock();
		fib_delete_min(heap);
		end = clock();
		fdm += ((double) (end-start) / CLOCKS_PER_SEC) * 1000;
	}
	for (i = 0; i < num_vertices; i++){
		free(vertices[i]->data);
		free(vertices[i]);
	}
	free(heap);
	free(vertices);
	free(distances);
	printf("fib: init: %f - total time: %f\n", finit, finit+fdm+fins+fdk);
	printf("     insert: %f (avg: %f)\n", fins, fins/num_vertices);
	printf("     delmin: %f (avg: %f)\n", fdm, fdm/num_vertices);
	printf("     dec.ke: %f (avg: %f)\n\n", fdk, fdk/decrease_key_calls);	
}