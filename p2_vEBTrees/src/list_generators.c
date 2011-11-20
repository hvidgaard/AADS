#include <stdlib.h>
#include "list_generators.h"

uint *generate_random_list(uint size, uint max, uint seed) {
	srandom(seed);
	uint *list = malloc(size*sizeof(uint));
	uint i;
	for(i = 0; i < size; i++) {
		list[i] = random()%max+1;
	}
	return list;
}