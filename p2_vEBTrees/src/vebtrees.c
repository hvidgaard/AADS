#include <vebtrees.h>
#include <stdlib.h>
#include <math.h>

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

vebtree * veb_initialize(int w, int threshold){
	//anything bigger will make the structure larger than 2 GiB
	if (w > 28)
		return NULL;
	if (w > threshold)
		return veb_init_tree(w, threshold);
	else
		return veb_init_leaf(w, threshold);
}
vebtree * veb_init_tree(int w, int threshold){
	vebtree * tree = malloc(sizeof(struct vebtree));
	tree->max = malloc(sizeof(struct vebelement));
	tree->max->value = 0;
	tree->max->data = NULL;
	tree->min = malloc(sizeof(struct vebelement));
	tree->min->value = 0;
	tree->min->data = NULL;
	tree->w = w;
	tree->size = pow(2,w);
	tree->n = 0;
	tree->threshold = threshold;
	//w/2 is rounded down, which is what we want if w is odd.
	int halfofw = w/2;
	int squarerootofsize = pow(2, w/2);
	//sorry about the two above names - I had no idea what to
	//call them :(
	tree->top = veb_initialize(halfofw, threshold);
	tree->bottom = calloc(squarerootofsize, sizeof(struct vebtree *));
	int i;
	for (i = 0; i < squarerootofsize; i++)
		tree->bottom[i] = veb_initialize(w-halfofw, threshold);
	return tree;
}
vebtree * veb_init_leaf(int w, int threshold){
	vebtree * tree = malloc(sizeof(struct vebtree));
	tree->max = malloc(sizeof(struct vebelement));
	tree->max->value = 0;
	tree->max->data = NULL;
	tree->min = malloc(sizeof(struct vebelement));
	tree->min->value = 0;
	tree->min->data = NULL;
	tree->w = w;
	tree->size = pow(2,w);
	tree->n = 0;
	tree->threshold = threshold;
	tree->bottom = NULL;
	tree->top = NULL;
	tree->arr = calloc(tree->size, sizeof(struct vebelement *));
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
	vebelement* new_ele = malloc(sizeof(struct vebelement));
	vebelement* tmp_ele;
	if (index < tree->min->value) {
		tmp_ele = tree->min;
		tree->min = new_ele;
		new_ele = tmp_ele;
	}
	else if (index > tree->max->value) {
		tmp_ele = tree->max;
		tree->max = new_ele;
		new_ele = tmp_ele;
	}
	uint32_t * a = malloc(sizeof(uint32_t));
	uint32_t * b = malloc(sizeof(uint32_t));
	vebfactor(tree->w, index, a, b);
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
	
	/*if (tree->n == 0) {
		tree->max->value = index;
		tree->max->data = data;
		return 0;
	}
	else if (tree->n == 1){
		if (index == tree->max->value)
			return 2;
		tree->min->value = index;
		tree->min->value = data;
		if (tree->min->value > tree->max->value) {
			vebelement * tmp_ele = tree->min;
			tree->min = tree->max
			tree->max = tmp_ele;
		}
		return 0;
	}
	else if (tree->n == 2){
		vebelement * new_ele = malloc(sizeof(struct vebelement));
		new_ele->data = data;
		new_ele->value = index;
		if ()
		//2 elements ( in min and max)
	}
	else{
	if (tree->arr[index]->value)
		return 0; //the element already exists, so return 0 to indicate error.
	tree->arr[index] = data;
	tree->n++;
	return index;
	}*/
}
uint32_t veb_insert_node_trivial(uint32_t index, void * data, vebtree * tree){
	if (tree->n == 0){
		tree->n = 1;
		tree->max->value = index;
		tree->max->data = data;
		return 0;
	}
	else{
		tree->n = 2;
		tree->min = malloc(sizeof(struct vebelement *));
		tree->min->value = index;
		tree->min->data = data;
		if (index > tree->max->value){
			vebelement * tmp_ele = tree->min;
			tree->min = tree->max;
			tree->max = tmp_ele;
			
		}
		return 0;
	}
}
void vebfactor(int w, uint32_t i, uint32_t * a, uint32_t * b){
	*a = i >> (w-(w/2));
	//we want to clear the upper bits.
	*b = i << (w/2);
	*b = *b >> (w/2);
}