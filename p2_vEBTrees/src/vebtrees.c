#include <vebtrees.h>
#include <stdlib.h>
#include <math.h>
#include <stdio.h>

/*size is number of bits to use for the elements. Note
 *that the size of the entire datastructure will be 
 *2^size...
 *threshold is the amount of bits from where to degrade
 *to a linear search through an array.
 * 
 * w is the number of bits to use
 * threshold is the number of elements a tree must
 * at least contain. If the size is less than this
 * degrade to a simple array for storing the element
 */

/* declerations of internal functions not intented for
 * external use.
 */
uint32_t veb_insert_leaf(uint32_t index, void * data, vebtree * tree);
uint32_t veb_insert_node_trivial(uint32_t index, void * data, vebtree * tree);
vebtree * veb_init_tree(int size, int threshold);
vebtree * veb_init_leaf(int size, int threshold);
void vebfactor(int w, uint32_t i, uint32_t * a, uint32_t * b);
void vebswap(uint32_t *index, void **data, vebelement * e);

vebtree * veb_initialize(int w, int threshold){
	//anything bigger will make the structure larger than 2 GiB
	if (w > 28)
		return NULL;
	if (pow(2, w) > threshold){
		//printf("recursive structure: ");
		return veb_init_tree(w, threshold);
	}
	else {
		//printf("leaf structure: ");
		return veb_init_leaf(w, threshold);
		
	}
}
vebtree * veb_init_tree(int w, int threshold){
	vebtree * tree = malloc(sizeof(struct vebtree));
	//tree->max = malloc(sizeof(struct vebelement));
	tree->max.value = 0;
	tree->max.data = NULL;
	//tree->min = malloc(sizeof(struct vebelement));
	tree->min.value = 0;
	tree->min.data = NULL;
	tree->w = w;
	tree->size = pow(2,w);
	tree->n = 0;
	tree->threshold = threshold;
	//w/2 is rounded down, which is what we want if w is odd.
	int halfofw = w/2;
	int squarerootofsize = pow(2, w/2);
	/*printf("\n\
                w := %d\n\
        threshold := %d\n\
          halfofw := %d\n\
       sqrtofsize := %d\n", w, threshold, halfofw, squarerootofsize);*/
	//sorry about the two above names - I had no idea what to
	//call them :(
	//printf("Create TOP: ");
	tree->top = veb_initialize(halfofw, threshold);
	/*printf("\n");
	printf("Create BOTTOMS: ");
	printf("\n");*/
	tree->bottom = calloc(squarerootofsize, sizeof(struct vebtree *));
	int i;
	for (i = 0; i < squarerootofsize; i++)
		tree->bottom[i] = veb_initialize(w-halfofw, threshold);
	return tree;
}
vebtree * veb_init_leaf(int w, int threshold){
	vebtree * tree = malloc(sizeof(struct vebtree));
	//tree->max = calloc(1, sizeof(struct vebelement));
	//tree->max->value = 0;
	//tree->max->data = NULL;
	//tree->min = calloc(1, sizeof(struct vebelement));
	//tree->min->value = 0;
	//tree->min->data = NULL;
	tree->w = w;
	tree->size = pow(2,w);
	tree->n = 0;
	tree->threshold = threshold;
	tree->bottom = NULL;
	tree->top = NULL;
	//printf("size: %d", tree->size);
	tree->arr = calloc(tree->size, sizeof(struct vebelement));
	/*printf("\n\
                w := %d\n\
        threshold := %d\n", w, threshold);*/
	return tree;
}
uint32_t veb_delete(uint32_t index, vebtree * tree){	
	return 0;
}
uint32_t veb_findsucc(uint32_t index, vebtree * tree){
	return 0;
}
uint32_t veb_findpred(uint32_t index, vebtree * tree){
	return 0;
}
/* return 0 if the insert succeded. Any other value 
 * indicated an error
 */
uint32_t veb_insert(uint32_t index, void * data, vebtree * tree){
	//the element shouldn't be stored as the index is too large
	if (index > tree->size)
		return 1;
	//the tree is at the threshold, thus it's just a simple array
	if (tree->size < tree->threshold)
		return veb_insert_leaf(index, data, tree);
	//the insert is a trivial case with no recursion
	else if (tree->n < 2)
		return veb_insert_node_trivial(index, data, tree);
	//the insert is a recursive case.
	//check if the new insert falls outside the current min/max span
	if (index < tree->min.value) {
		vebswap(&index, &data, &(tree->min));
		/*void *tmp_data;
		uint32_t tmp_idx;
		tmp_data = tree->min.data;
		tmp_idx = tree->min.value;
		tree->min.data = data;
		tree->min.value = index;
		data = tmp_data;
		index = tmp_idx;*/
	}
	else if (index > tree->max.value) {
		vebswap(&index, &data, &(tree->max));
		/*void *tmp_data;
		uint32_t tmp_idx;
		tmp_data = tree->max.data;
		tmp_idx = tree->max.value;
		tree->max.data = data;
		tree->max.value = index;
		data = tmp_data;
		index = tmp_idx;*/
	}
	uint32_t * a = malloc(sizeof(uint32_t));
	uint32_t * b = malloc(sizeof(uint32_t));
	vebfactor(tree->w, index, a, b);
	//printf("w: %d - i: %d - a: %d - b: %d\n", tree->w, index, *a, *b);
	int result;
	if (tree->bottom[*a]->n == 0) {
		result = veb_insert(*a, NULL, tree->top);
		if (result != 0) {
			//cleanup in case of failure
			free(a);
			free(b);
			return result;
		}
	}
	result = veb_insert(*b, data, tree->bottom[*a]);
	if (result != 0 && tree->bottom[*a]->n == 0)
		veb_delete(*a, tree->top);
	free(a);
	free(b);
	return result;
}
uint32_t veb_insert_leaf(uint32_t index, void * data, vebtree * tree){
	if ((tree->arr)[index].value)
		return 3;
	(tree->arr)[index].value = 1;
	(tree->arr)[index].data = data;
	tree->n++;
	return 0;
}
uint32_t veb_insert_node_trivial(uint32_t index, void * data, vebtree * tree){
	if (tree->n == 0){
		tree->n = 1;
		tree->max.value = index;
		tree->max.data = data;
		return 0;
	}
	else if (index > tree->max.value)
			vebswap(&index, &data, &(tree->max));
	tree->n = 2;
	tree->min.value = index;
	tree->min.data = data;
	return 0;
}
void vebfactor(int w, uint32_t i, uint32_t * a, uint32_t * b){
	*a = i >> (w-(w/2));
	//we want to clear the upper bits.
	*b = i << (32-(w-(w/2)));
	//printf("what: %d", *b);
	*b = *b >> (32-(w-(w/2)));
}
void vebswap(uint32_t *index, void **data, vebelement *e){
	void *tmp_data = e->data;
	uint32_t tmp_idx = e->value;
	e->data = *data;
	e->value = *index;
	*data = tmp_data;
	*index = tmp_idx;
}