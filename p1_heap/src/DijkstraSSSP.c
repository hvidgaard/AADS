#include <stdlib.h>
#include <stdio.h>
#include <BinaryHeap.h>
#include <FibonacciHeap.h>
#include <BinaryHeap.h>

void dijkstra(unsigned int num_vertices, unsigned int source, unsigned int * w, unsigned int ** edges){
	unsigned int * dist = malloc(num_vertices * sizeof(unsigned int));
	
	binary_heap * h = bh_init_heap(num_vertices);
	bh_element ** elements = malloc(num_vertices * sizeof(bh_element *));
	bh_element * v;
	
	dist[source] = 0;
	unsigned int i;
	unsigned int * v_data;
	v_data = malloc(sizeof(unsigned int));
	*v_data = source;
	
	elements[source] = bh_insert(0, v_data, h);
	//printf("inserted source, got index %d\n", elements[source]->index);	
	for (i = 0; i < num_vertices; i++){
		if (i != source) {
			v_data = malloc(sizeof(unsigned int));
			*v_data = i;
			elements[*v_data] = bh_insert(UINT_MAX, v_data, h);
			//printf("inserted %d with key: %d, got index %d\n", *v_data, UINT_MAX, elements[*v_data]->index);	
			dist[i] = UINT_MAX;
		}
	}
	unsigned int v_num;
	unsigned int u_num;
	while (h->size > 0) {
		v = bh_delete_min(h);
		v_num = *(unsigned int *)v->data;
		//printf("del_min : %d\n", v_num); 
		for (i = 1; i <= edges[v_num][0]; i++){
			 u_num = edges[v_num][i];
			 //printf("u: %d - dist[v]: %d, dist[u]: %d, w(u,v): %d\n", u_num, dist[v_num], dist[u_num], w[v_num * num_vertices + u_num]);
			 if (dist[v_num] + w[v_num * num_vertices + u_num] < dist[u_num] ){
				 dist[u_num] = dist[v_num] + w[v_num * num_vertices + u_num];
				 bh_decrease_key(dist[u_num], elements[u_num], h);
			 }
		 }
	}
	for (i = 0; i < num_vertices; i++)
		printf("distance from %d to %d: %d\n", source, i, dist[i]);
}