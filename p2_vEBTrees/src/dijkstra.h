#ifndef DIJKSTRA_H
#define DIJKSTRA_H

#ifndef uint
typedef unsigned int uint;
#endif
#ifndef UINT_MAX
#define UINT_MAX 16777215
#endif

uint dijkstra_bin(uint num_vertices, uint source, uint* weights, uint** edges);
uint dijkstra_fib(uint num_vertices, uint source, uint* weights, uint** edges);
uint dijkstra_veb(uint num_vertices, uint source, uint* weights, uint** edges);

#endif