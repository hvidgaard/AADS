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
	uint32_t threshold;
	struct vebelement *arr;
} vebtree;



/* this are the basic operations, other operations are combinations
 * hereof.
 */
void veb_delete(uint32_t index, vebtree * tree);
uint32_t veb_insert(uint32_t index, void * data, vebtree * tree);
int32_t veb_findsucc(uint32_t index, void * data, vebtree *tree);
int32_t veb_findpred(uint32_t index, void * data, vebtree *tree);
void veb_extract_min(vebtree *, void *);
void veb_delete_min(vebtree *);
/********************************************************************/
vebtree * veb_initialize(int, int);
/********************************************************************/