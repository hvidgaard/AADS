#ifndef SSSP_H
#define SSSP_H

#define UINT_MAX 429496729

unsigned int *generate_graph(unsigned int vertices, unsigned int edge_chance, unsigned int max_weight, unsigned int seed);
unsigned int *generate_decrease_key_max(unsigned int vertices);
unsigned int *generate_decrease_key_max2(unsigned int vertices);
unsigned int dijkstra_bin(unsigned int num_vertices, unsigned int source, unsigned int * w, unsigned int ** edges, unsigned int *bops);
unsigned int dijkstra_fib(unsigned int num_vertices, unsigned int source, unsigned int * w, unsigned int ** edges, unsigned int *bops);
unsigned int dijkstra_pq(unsigned int num_vertices, unsigned int source, unsigned int * w, unsigned int ** edges, unsigned int *bops);

#endif