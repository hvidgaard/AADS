#ifndef GRAPH_GEN_H_
#define GRAPH_GEN_H_

#ifndef uint
typedef unsigned int uint;
#endif

uint *generate_random_graph(uint vertices, uint max_weight, uint seed);
uint *generate_decrease_key_max_graph(uint vertices, uint max_weight, uint seed);
uint *generate_decrease_key_max_graph_2(uint vertices, uint max_weight, uint seed);

#endif