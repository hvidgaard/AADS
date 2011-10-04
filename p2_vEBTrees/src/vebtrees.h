#include <stdint.h>

typedef struct vebtree {
	uint32_t n;
	uint32_t size;
	uint32_t min;
	uint32_t max;
	uint32_t sqroot;
	int threshold;
	struct vebtree *top;
	struct vebtree **bottom;
	struct vebelement *arr;
} vebtree;

typedef struct vebelement {
	void *data;
} vebelement;

/* this are the basic operations, other operations are combinations
 * hereof.
 */
vebtree * veb_init_tree(uint32_t size, int threshold);
vebtree * veb_init_leaf(int size, int threshold);
uint32_t veb_delete(uint32_t index, vebtree * tree);
uint32_t veb_insert(uint32_t index, void * data, vebtree * tree);
uint32_t veb_findsucc(uint32_t index, vebtree * tree);
uint32_t veb_findpred(uint32_t index, vebtree * tree);
/********************************************************************/
vebtree * veb_initialize(uint32_t size, int threshold);
vebelement * veb_deletemin(vebtree * tree);
/********************************************************************/
void vebfactor(uint32_t i, uint32_t * a, uint32_t * b);