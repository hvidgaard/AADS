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
	if (w > 24)
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
	tree->sqrtsize = pow(2, halfofw);
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
	tree->bottom = malloc(tree->sqrtsize * sizeof(struct vebtree *));
	int i;
	for (i = 0; i < tree->sqrtsize; i++)
		tree->bottom[i] = veb_initialize(w-halfofw, threshold);
	return tree;
}
vebtree * veb_init_leaf(int w, int threshold){
	vebtree * tree = malloc(sizeof(struct vebtree));
	/*tree->max = calloc(1, sizeof(struct vebelement));
	tree->max->value = 0;
	tree->max->data = NULL;
	tree->min = calloc(1, sizeof(struct vebelement));
	tree->min->value = 0;
	tree->min->data = NULL;*/
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
void veb_delete(uint32_t index, vebtree * tree){	
	printf("\ndeleting %d in tree size %d\n", index, tree->size);
	if (tree->size <= tree->threshold){
		if ((tree->arr)[index].value && tree->n > 0) {
			tree->n--;
			(tree->arr)[index].value = 0;
		}
		return;
	}
	else {
		switch (tree->n){
			case 0:
				break;
			case 1:
				if (tree->max.value == index)
					tree->n--;
				break;
			case 2:
				if (tree->min.value == index){
					tree->min.value = tree->max.value;
					tree->min.data = tree->max.data;
					tree->n--;
				}
				else if (tree->max.value == index){
					tree->max.value = tree->min.value;
					tree->max.data = tree->min.data;
					tree->n--;
				}
				break;
			default:
				if (tree->min.value == index){
					index = tree->min.value = tree->top->min.value * tree->sqrtsize + (tree->bottom)[tree->top->min.value]->min.value;
					tree->min.data = (tree->bottom)[tree->top->min.value]->min.data;
					
					/*a = tree->top->min.value;
					b = (tree->bottom)[tree->top->min.value]->min.value;*/
				}
				else if (tree->max.value == index){
					index = tree->max.value = tree->top->max.value * tree->sqrtsize + (tree->bottom)[tree->top->max.value]->max.value;
					tree->max.data = (tree->bottom)[tree->top->max.value]->max.data;
				}
				uint32_t * t_a = malloc(sizeof(uint32_t));
				uint32_t * t_b = malloc(sizeof(uint32_t));
				vebfactor (tree->w, index, t_a, t_b);
				uint32_t a = *t_a;
				uint32_t b = *t_b;
				free(t_a);
				free(t_b);
				veb_delete(b, (tree->bottom)[a]);
				if ((tree->bottom)[a]->size == 0)
					veb_delete(a, tree->top);
				tree->n--;
				break;
		}
		return;
	}
	
}
int32_t veb_findsucc(uint32_t index, vebtree * tree){
	if (tree->size > tree->threshold) {
		if (index > tree->max.value && tree->n > 0)
			return -1;
		else if (index <= tree->min.value)
			return tree->min.value;
		else if (tree->n <= 2)
			return tree->max.value;
		uint32_t * t_a = malloc(sizeof(uint32_t));
		uint32_t * t_b = malloc(sizeof(uint32_t));
		vebfactor (tree->w, index, t_a, t_b);
		uint32_t a = *t_a;
		uint32_t b = *t_b;
		free(t_a);
		free(t_b);
		if ((tree->bottom)[a]->n > 0 && (tree->bottom)[a]->max.value >= b)
			return a * (tree->sqrtsize) + veb_findsucc(b, (tree->bottom)[a]);
		else if (tree->top->max.value <= a)
			return tree->max.value;
		uint32_t c = veb_findsucc(a + 1, tree->top);
		return c * (tree->sqrtsize) + (tree->bottom)[c]->min.value;
	}
	else{
		int i;
		for (i = 0; i < tree->size; i++){
			if (tree->arr[i].value)
				return i;
		}
		return -1;
	}
}
int32_t veb_findpred(uint32_t index, vebtree * tree){
	if (tree->size < tree->threshold){
		int i;
		for (i = 0; i < tree->size; i++){
			if (tree->arr[i].value){
				if (tree->arr[i].value > index)
					return -1;
				else
					return i;
			}
		}
		return -1;
	}
	else {
		if (index < tree->min.value)
			return -1;
		else if (index >= tree->max.value && tree->n > 0)
			return tree->max.value;
		else if (tree->n <= 2)
			return tree->min.value;
		uint32_t * t_a = malloc(sizeof(uint32_t));
		uint32_t * t_b = malloc(sizeof(uint32_t));
		vebfactor (tree->w, index, t_a, t_b);
		uint32_t a = *t_a;
		uint32_t b = *t_b;
		free(t_a);
		free(t_b);
		if ((tree->bottom)[a]->n > 0 && (tree->bottom)[a]->min.value <= b)
			return a * (tree->sqrtsize) + veb_findpred(b, (tree->bottom)[a]);
		else if (tree->top->min.value >= a)
			return tree->min.value;
		int32_t c = veb_findpred(a + 1, tree->top);
		return c * (tree->sqrtsize) + (tree->bottom[c]->max.value);
	}
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
	}
	else if (index > tree->max.value) {
		vebswap(&index, &data, &(tree->max));
	}
	uint32_t * a = malloc(sizeof(uint32_t));
	uint32_t * b = malloc(sizeof(uint32_t));
	vebfactor(tree->w, index, a, b);
	if (index == 10) printf("w: %d - i: %d - a: %d - b: %d\n", tree->w, index, *a, *b);
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
	tree->n++;
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
		tree->max.value = index;
		tree->max.data = data;
		tree->min.value = index;
		tree->min.data = data;
		tree->n++;
		return 0;
	}
	else if (index > tree->max.value)
			vebswap(&index, &data, &(tree->max));
	tree->min.value = index;
	tree->min.data = data;
	tree->n++;
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