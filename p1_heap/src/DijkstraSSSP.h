#ifndef SSSP_H
#define SSSP_H

#define UINT_MAX 429496729

unsigned int *dijkstra_bin(unsigned int num_vertices, unsigned int source, unsigned int * w, unsigned int ** edges);
unsigned int *dijkstra_fib(unsigned int num_vertices, unsigned int source, unsigned int * w, unsigned int ** edges);

#endif