#ifndef H_VEB_T
#define H_VEB_T

#include <stdint.h>
#include "linked_list.h"

typedef struct vebelement {
	uint32_t value;
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
vebtree * veb_initialize(int w, int threshold);
int32_t veb_insert(uint32_t index, void * data, vebtree * tree);
void * veb_delete(uint32_t index, vebtree * tree);
void * veb_findsucc(uint32_t index, int32_t * succ, vebtree * tree);
void * veb_findpred(uint32_t index, int32_t * pred, vebtree * tree);
void * veb_delete_min(vebtree * tree);
void veb_destruct(vebtree *tree);
linked_list *veb_prio_walk(vebtree *t);



#endif