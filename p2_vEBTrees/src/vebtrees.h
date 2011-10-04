#include <stdint.h>

typedef struct vebtree {
	uint32_t n;
	uint32_t size;
	uint32_t min;
	uint32_t max;
	struct vebtree *top;
	struct vebtree **bottom;
} vebtree;

typedef struct vebelement {
	uint32_t value;
	void * data;
} vebelement;

/* this are the basic operations, other operations are a combinations
 * hereof.
 */
 vebtree * initialize(uint32_t size);
uint32_t delete_index(uint32_t index, vebtree * tree);
uint32_t findsucc_index(uint32_t index, vebtree * tree);
uint32_t findpred_index(uint32_t index, vebtree * tree);
/********************************************************************/

void insert(vebelement * element, vebtree * tree);
vebelement * delete(vebelement * element, vebtree * tree);
vebelement * findsucc(vebelement * element, vebtree * tree);
vebelement * deletemin(vebtree * tree);
vebelement * findpred(vebelement element, vebtree * tree);