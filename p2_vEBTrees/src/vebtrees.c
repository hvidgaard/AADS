#include <vebtrees.h>
#include <stdlib.h>
#include <math.h>
#include <stdio.h>
#include <unistd.h>

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
int32_t veb_insert_leaf(uint32_t index, void * data, vebtree * tree);
int32_t veb_insert_node_trivial(uint32_t index, void * data, vebtree * tree);
vebtree * veb_init_tree(int size, int threshold);
vebtree * veb_init_leaf(int size, int threshold);
void vebfactor(int w, uint32_t i, uint32_t * a, uint32_t * b);
void vebswap(uint32_t *, void **, vebelement *);
void veb_delete_tree(uint32_t, vebtree *);

int elementsInAPage = 0;

vebtree * veb_initialize(int w){
	//anything bigger will make the structure larger than 2 GiB
	
	//the threashold is designed such than a leaf tree fits entirely in a single
	//page.
	if (!elementsInAPage){
		elementsInAPage = (sysconf(_SC_PAGESIZE) - sizeof(struct vebtree)) / sizeof(struct vebelement);
		//elementsInAPage = 28;
		/*printf("size of tree is %d\n", sizeof(struct vebtree));
		printf("size of element is %d\n", sizeof(struct vebelement));
		printf("size of element pointer is %d\n", sizeof(struct vebelement *));
		printf("size of tree pointer is %d\n", sizeof(struct vebtree *));
		printf("size of element pointer pointer is %d\n", sizeof(struct vebelement **));
		printf("threshold is %d\n", elementsInAPage);*/
	}
	if (w > 26)
		return NULL;
	if (pow(2, w) > elementsInAPage)
		return veb_init_tree(w, elementsInAPage);
	else
		return veb_init_leaf(w, elementsInAPage);
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
	tree->sqrtsize = pow(2, halfofw);
	
	tree->top = veb_initialize(halfofw);
	
	tree->bottom = malloc(tree->sqrtsize * sizeof(struct vebtree *));
	int i;
	for (i = 0; i < tree->sqrtsize; i++)
		tree->bottom[i] = veb_initialize(w-halfofw);
	
	return tree;
}
vebtree * veb_init_leaf(int w, int threshold){
	//no need to initialize the top/bottom
	//since it's a leaf and will never be accessed.
	vebtree * tree = malloc(sizeof(struct vebtree));
//	tree->min = malloc(sizeof(struct vebelement));
//	tree->max = malloc(sizeof(struct vebelement));
	tree->w = w;
	tree->size = pow(2,w);
	tree->n = 0;
	tree->threshold = threshold;
	tree->arr = calloc(tree->size, sizeof(struct vebelement));

	return tree;
}
void veb_delete(uint32_t index, vebtree * tree){	
	//delete in leaf, not large enough to warrant it's own function.
	if (tree->size <= tree->threshold){
		if (tree->n > 0 && (tree->arr)[index].value) {
			tree->n--;
			(tree->arr)[index].value = 0;
		}
	}
	else
		veb_delete_tree(index, tree);
}
void veb_delete_tree(uint32_t index, vebtree * tree){
	switch (tree->n){
		case 0:
			break;
		case 1:
			tree->n--;
			break;
		case 2:
			if (tree->max->value == index){
				tree->max->value = tree->min->value;
				tree->max->data = tree->min->data;
				tree->n--;
			}
			else if (tree->min->value == index){
				tree->min->value = tree->max->value;
				tree->min->data = tree->max->data;
				tree->n--;
			}
			break;
		default:
			if (tree->min->value == index){
				index = tree->min->value = tree->top->min->value * tree->sqrtsize + (tree->bottom)[tree->top->min->value]->min->value;
				tree->min->data = (tree->bottom)[tree->top->min->value]->min->data;
				}
			else if (tree->max->value == index){
				index = tree->max->value = tree->top->max->value * tree->sqrtsize + (tree->bottom)[tree->top->max->value]->max->value;
				tree->max->data = (tree->bottom)[tree->top->max->value]->max->data;
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
} 
int32_t veb_findsucc(uint32_t index, vebtree * tree){
	//in a leaf
	if (tree->size <= tree->threshold) {
		int i;
		for (i = index; i < tree->size; i++){
			if (tree->arr[i].value)
				return i;
		}
		return -1;
	}
	//in a recursive tree
	else {
		if (index > tree->max->value && tree->n > 0)
			return -1;
		else if (index <= tree->min->value)
			return tree->min->value;
		else if (tree->n <= 2)
			return tree->max->value;
		uint32_t * t_a = malloc(sizeof(uint32_t));
		uint32_t * t_b = malloc(sizeof(uint32_t));
		vebfactor (tree->w, index, t_a, t_b);
		uint32_t a = *t_a;
		uint32_t b = *t_b;
		free(t_a);
		free(t_b);
		if ((tree->bottom)[a]->n > 0 && (tree->bottom)[a]->max->value >= b)
			return a * (tree->sqrtsize) + veb_findsucc(b, (tree->bottom)[a]);
		else if (tree->top->max->value <= a)
			return tree->max->value;
		uint32_t c = veb_findsucc(a + 1, tree->top);
		return c * (tree->sqrtsize) + (tree->bottom)[c]->min->value;
	}
}
int32_t veb_findpred(uint32_t index, vebtree * tree){
	//in a leaf
	if (tree->size <= tree->threshold){
		int i;
		for (i = index; i >= 0; i--){
			if (tree->arr[i].value){
				return i;
			}
		}
		return -1;
	}
	//in a recursive tree
	else {
		if (index < tree->min->value)
			return -1;
		else if (index >= tree->max->value && tree->n > 0)
			return tree->max->value;
		else if (tree->n <= 2)
			return tree->min->value;
		uint32_t * t_a = malloc(sizeof(uint32_t));
		uint32_t * t_b = malloc(sizeof(uint32_t));
		vebfactor (tree->w, index, t_a, t_b);
		uint32_t a = *t_a;
		uint32_t b = *t_b;
		free(t_a);
		free(t_b);
		if ((tree->bottom)[a]->n > 0 && (tree->bottom)[a]->min->value <= b)
			return a * (tree->sqrtsize) + veb_findpred(b, (tree->bottom)[a]);
		else if (tree->top->min->value >= a)
			return tree->min->value;
		int32_t c = veb_findpred(a, tree->top);
		if (c == -1)
			return tree->min->value;
		return c * (tree->sqrtsize) + (tree->bottom[c]->max->value);
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
	if (tree->size <= tree->threshold)
		return veb_insert_leaf(index, data, tree);
	//the insert is a trivial case with no recursion
	else if (tree->n < 2)
		return veb_insert_node_trivial(index, data, tree);
	//the insert is a recursive case.
	//check if the new insert falls outside the current min/max span
	//and swap accordingly
	if (index < tree->min->value) {
		vebswap(&index, &data, tree->min);
	}
	else if (index > tree->max->value) {
		vebswap(&index, &data, tree->max);
	}
	uint32_t * a = malloc(sizeof(uint32_t));
	uint32_t * b = malloc(sizeof(uint32_t));
	vebfactor(tree->w, index, a, b);
	if (tree->bottom[*a]->n == 0 && veb_insert(*a, NULL, tree->top)) {
		//cleanup in case of failure
		free(a);
		free(b);
		return 1;
	}
	if (veb_insert(*b, data, tree->bottom[*a]) && tree->bottom[*a]->n == 0) {
		veb_delete(*a, tree->top);
		free(a);
		free(b);
		return -1;
	}
	free(a);
	free(b);
	tree->n++;
	return 0;
}
int32_t veb_insert_leaf(uint32_t index, void * data, vebtree * tree){
	if ((tree->arr)[index].value)
		return 1;
	(tree->arr)[index].value = index;
	(tree->arr)[index].data = data;
	if (tree->n){
		if (index > tree->max->value){
			tree->max = &(tree->arr)[index];
		}
		else if (index < tree->min->value){
			tree->min = &(tree->arr)[index];
		}
	}
	else
		tree->min = tree->max = &(tree->arr)[index];
	tree->n++;
	return 0;
}
int32_t veb_insert_node_trivial(uint32_t index, void * data, vebtree * tree){
	if (tree->n == 0){
		tree->max->value = tree->min->value = index;
		tree->max->data = tree->min->data = data;
		tree->n++;
		return 0;
	}
	else if (index > tree->max->value)
			vebswap(&index, &data, tree->max);
	tree->min->value = index;
	tree->min->data = data;
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