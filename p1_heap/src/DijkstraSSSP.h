#ifndef SSSP_H
#define SSSP_H

#define UINT_MAX 429496729

void dijkstra_bin(unsigned int num_vertices, unsigned int source, unsigned int * w, unsigned int ** edges);
void dijkstra_fib(unsigned int num_vertices, unsigned int source, unsigned int * w, unsigned int ** edges);

#endif