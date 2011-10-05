#include <stdint.h>

typedef struct vebtree {
	uint32_t n;
	uint32_t size;
	//vebelement * min_data;
	void * min_data;
	uint32_t min;
	//vebelement * max_data;
	void * max_data;
	uint32_t max;
	uint32_t w;
	int threshold;
	struct vebtree *top;
	struct vebtree **bottom;
	void ** arr;
	//struct vebelement *arr;
} vebtree;

typedef struct vebelement {
	void *data;
} vebelement;

/* this are the basic operations, other operations are combinations
 * hereof.
 */
vebtree * veb_init_tree(int size, int threshold);
vebtree * veb_init_leaf(int size, int threshold);
uint32_t veb_delete(uint32_t index, vebtree * tree);
uint32_t veb_insert(uint32_t index, void * data, vebtree * tree);
uint32_t veb_findsucc(uint32_t index, vebtree * tree);
uint32_t veb_findpred(uint32_t index, vebtree * tree);
/********************************************************************/
vebtree * veb_initialize(int size, int threshold);
vebelement * veb_deletemin(vebtree * tree);
/********************************************************************/
inline void vebfactor(int w, uint32_t i, uint32_t * a, uint32_t * b);
inline void vebswap(void * d1, void * d2, uint32_t * i1, uint32_t * i2);