#ifndef H_VEB_T
#define H_VEB_T

#include <stdint.h>

typedef struct vebelement {
	int32_t value;
	void *data;
} vebelement;

typedef struct vebtree {
	int leaf;
	struct vebelement *max;
	struct vebelement *min;
	struct vebtree *top;
	struct vebtree **bottom;
	uint32_t w;
	uint32_t size;
	uint32_t sqrtsize;
	uint32_t n;
	struct vebelement *arr;
} vebtree;



/* this are the basic operations, other operations are combinations
 * hereof.
 */
void veb_delete(uint32_t index, void * data, vebtree * tree);
uint32_t veb_insert(uint32_t index, void * data, vebtree * tree);
int32_t veb_findsucc(uint32_t index, void * data, vebtree *tree);
int32_t veb_findpred(uint32_t index, void * data, vebtree *tree);
void veb_extract_min(vebtree * tree, void *);
void veb_delete_min(vebtree * tree, void * data);
void veb_decrease_key(uint32_t index, uint32_t delta, vebtree * tree);
void veb_destruct(vebtree *tree);
/********************************************************************/
vebtree * veb_initialize(int, int);
/********************************************************************/

#endif