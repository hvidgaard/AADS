#include "vebtrees.h"
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
vebtree * veb_init_tree(int size);
vebtree * veb_init_leaf(int size);
void vebfactor(int w, uint32_t i, uint32_t * a, uint32_t * b);
void vebswap(uint32_t *, void **, vebelement *);
void * veb_delete_tree(uint32_t index, vebtree * tree);

int elementsInAPage = 0;
uint32_t threshold = 0;

vebtree * veb_initialize(int w, int thres){
	if (!elementsInAPage){
		if (thres == -1){
			//the threashold is designed such than a leaf tree fits entirely in a single
			//page.
	
			elementsInAPage = sysconf(_SC_PAGESIZE) / sizeof(struct vebelement);
			printf("  threshold = %i  ", elementsInAPage);
			fflush(stdout);
			//elementsInAPage = 28;
			/*printf("size of tree is %d\n", sizeof(struct vebtree));
			printf("size of element is %d\n", sizeof(struct vebelement));
			printf("size of element pointer is %d\n", sizeof(struct vebelement *));
			printf("size of tree pointer is %d\n", sizeof(struct vebtree *));
			printf("size of element pointer pointer is %d\n", sizeof(struct vebelement **));
			printf("threshold is %d\n", elementsInAPage);*/
		}
		else {
			elementsInAPage = thres;
		}
		threshold = thres;
	}
	//anything bigger will make the structure larger than 2 GiB
	if (w > 24)
		return NULL;
	if (pow(2, w) > elementsInAPage)
		return veb_init_tree(w);
	else
		return veb_init_leaf(w);
}
vebtree * veb_init_tree(int w){
	vebtree * tree = malloc(sizeof(struct vebtree));
	
	tree->leaf = 0;

	tree->max = malloc(sizeof(struct vebelement));
	tree->max->value = 0;
	tree->max->data = NULL;
	
	tree->min = malloc(sizeof(struct vebelement));
	tree->min->value = 0;
	tree->min->data = NULL;
	
	tree->w = w;
	tree->size = pow(2,w);
	tree->n = 0;
	//w/2 is rounded down, which is what we want if w is odd.
	int halfofw = w/2;
	tree->sqrtsize = pow(2, halfofw);
	
	tree->top = veb_initialize(halfofw, 0);
	
	tree->bottom = malloc(tree->sqrtsize * sizeof(struct vebtree *));
	int i;
	for (i = 0; i < tree->sqrtsize; i++)
		tree->bottom[i] = veb_initialize(w-halfofw, 0);
	
	return tree;
}
vebtree * veb_init_leaf(int w){
	//no need to initialize the top/bottom
	//since it's a leaf and will never be accessed.
	vebtree * tree = malloc(sizeof(struct vebtree));

	tree->leaf = 1;
//	tree->min = malloc(sizeof(struct vebelement));
//	tree->max = malloc(sizeof(struct vebelement));
	tree->w = w;
	tree->size = pow(2,w);
	tree->n = 0;
	tree->arr = calloc(tree->size, sizeof(struct vebelement));

	return tree;
}
void * veb_delete(uint32_t index, vebtree * tree){	
	//delete in leaf, not large enough to warrant it's own function.
	if (tree->size <= threshold){
		void * result = NULL;
		if (tree->n > 0 && (tree->arr)[index].value) {
			tree->n--;
			(tree->arr)[index].value = 0;
			result = (tree->arr)[index].data;
		}
		return result;
	}
	else
		return veb_delete_tree(index, tree);
}
void * veb_delete_min(vebtree * tree){
	int32_t r;
	veb_findsucc(0, &r,tree);
	return veb_delete(r, tree);
}
void * veb_delete_tree(uint32_t index, vebtree * tree){
	void * result = NULL;
	switch (tree->n){
		case 0:
			return NULL;
		case 1:
			tree->n--;
			return tree->max->data;
		case 2:
			if (tree->max->value == index){
				result = tree->max->data;
				tree->max->value = tree->min->value;
				tree->max->data = tree->min->data;
				tree->n--;
			}
			else{
				result = tree->min->data;
				tree->min->value = tree->max->value;
				tree->min->data = tree->max->data;
				tree->n--;
			}
			return result;
		default:
			if (tree->min->value == index){
				result = tree->min->data;
				index = tree->min->value = tree->top->min->value * tree->sqrtsize + (tree->bottom)[tree->top->min->value]->min->value;
				tree->min->data = (tree->bottom)[tree->top->min->value]->min->data;
				}
			else if (tree->max->value == index){
				result = tree->max->data;
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
			if (result)
				veb_delete(b, (tree->bottom)[a]);
			else
				result = veb_delete(b, (tree->bottom)[a]);
				
			if ((tree->bottom)[a]->size == 0)
				veb_delete(a, tree->top);
			tree->n--;
			return result;
	}
}
void * veb_findsucc(uint32_t index, int32_t * succ, vebtree * tree){
	if (tree->n == 0){
		*succ = -1;
		return NULL;
	}
	//in a leaf
	if (tree->size <= threshold) {
		int i;
		for (i = index; i < tree->size; i++){
			if (tree->arr[i].value){
				*succ = i;
				return tree->arr[i].data;
			}
		}
		*succ = -1;
		return NULL;
	}
	//in a recursive tree
	else {
		if (index > tree->max->value && tree->n > 0){
			*succ = -1;
			return NULL;
		}
		else if (index <= tree->min->value){
			*succ = tree->min->value;
			return tree->min->data;
		}
		else if (tree->n <= 2){
			*succ = tree->max->value;
			return tree->max->data;
		}
		uint32_t * t_a = malloc(sizeof(uint32_t));
		uint32_t * t_b = malloc(sizeof(uint32_t));
		vebfactor (tree->w, index, t_a, t_b);
		uint32_t a = *t_a;
		uint32_t b = *t_b;
		free(t_a);
		free(t_b);
		if ((tree->bottom)[a]->n > 0 && (tree->bottom)[a]->max->value >= b){
			int32_t r;
			void * data = veb_findsucc(b, &r, (tree->bottom)[a]);
			*succ = a * tree->sqrtsize + r;
			return data;
		}
		else if (tree->top->max->value <= a){
			*succ = tree->max->value; 
			return tree->max->data;
		}
		int32_t c;
		veb_findsucc(a + 1, &c, tree->top);
		*succ = c * (tree->sqrtsize) + (tree->bottom)[c]->min->value;
		return (tree->bottom)[c]->min->data;
	}
}
void * veb_findpred(uint32_t index, int32_t * pred, vebtree * tree){
	if (tree->n == 0){
		*pred = -1;
		return NULL;
	}
	//in a leaf
	if (tree->size <= threshold){
		void * result = NULL;
		int i;
		int32_t idx = -1;
		for (i = 0; i <= index; i++){
			if (tree->arr[i].value){
				result = tree->arr[i].data;
				idx = i;
			}
		}
		*pred = idx;
		return result;
	}
	//in a recursive tree
	else {
		if (index < tree->min->value){
			*pred = -1;
			return NULL;
		}
		else if (index >= tree->max->value && tree->n > 0){
			*pred = tree->max->value;
			return tree->max->data;
		}
		else if (tree->n <= 2){
			*pred = tree->min->value;
			return tree->min->data;
		}
		uint32_t * t_a = malloc(sizeof(uint32_t));
		uint32_t * t_b = malloc(sizeof(uint32_t));
		vebfactor (tree->w, index, t_a, t_b);
		uint32_t a = *t_a;
		uint32_t b = *t_b;
		free(t_a);
		free(t_b);
		if ((tree->bottom)[a]->n > 0 && (tree->bottom)[a]->min->value <= b){
			int32_t r;
			void * data = veb_findpred(b, &r, (tree->bottom)[a]);
			*pred = a * (tree->sqrtsize) + r;
			return data;
		}
		else if (tree->top->min->value >= a){
			*pred = tree->min->value;
			return tree->min->data;
		}
		int32_t c;
		veb_findpred(a, &c, tree->top);
		if (c == -1){
			*pred = tree->min->value;
			return tree->min->data;
		}
		*pred = c * (tree->sqrtsize) + (tree->bottom[c]->max->value);
		return tree->bottom[c]->max->data;
	}
}
void * veb_extract_min(vebtree * tree, int32_t * index){
	return veb_findsucc(0, index, tree);
}
/*void veb_decrease_key(uint32_t delta, uint32_t index, vebtree * tree){
	void * d = malloc(sizeof(void *));
	veb_delete(index, d, tree);
	veb_insert(index-delta, d, tree);
}*/

/* return 0 if the insert succeded. Any other value 
 * indicated an error
 */
int32_t veb_insert(uint32_t index, void * data, vebtree * tree){
	//the element shouldn't be stored as the index is too large
	if (index > tree->size)
		return 1;
	//the tree is at the threshold, thus it's just a simple array
	if (tree->size <= threshold)
		return veb_insert_leaf(index, data, tree);
	//the insert is a trivial case with no recursion
	else if (tree->n < 2)
		return veb_insert_node_trivial(index, data, tree);
	//the insert is a recursive case.
	//check if the new insert falls outside the current min/max span
	//and swap accordingly
	uint32_t * a = malloc(sizeof(uint32_t));
	uint32_t * b = malloc(sizeof(uint32_t));
	if (index < tree->min->value) {
		vebfactor(tree->w, tree->min->value, a, b);
		veb_insert(*b, tree->min->data, tree->bottom[*a]);
		tree->min->data = data;
		tree->min->value = index;
		if (tree->bottom[*a]->n == 1)
			veb_insert(*a, NULL, tree->top);
		//vebswap(&index, &data, tree->min);
	}
	else if (index > tree->max->value) {
		vebfactor(tree->w, tree->max->value, a, b);
		veb_insert(*b, tree->max->data, tree->bottom[*a]);
		tree->max->data = data;
		tree->max->value = index;
		if (tree->bottom[*a]->n == 1)
			veb_insert(*a, NULL, tree->top);
		//vebswap(&index, &data, tree->max);
	}
	else {
		vebfactor(tree->w, index, a, b);
		veb_insert(*b, data, tree->bottom[*a]);
		if (tree->bottom[*a]->n == 1)
			veb_insert(*a, NULL, tree->top);
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
void veb_destruct(vebtree *tree){
	if (tree->size < threshold)
		free(tree->arr);
	else {
		free(tree->max);
		free(tree->min);
		veb_destruct(tree->top);
		int i;
		for (i = 0; i < tree->sqrtsize; i++)
			veb_destruct(tree->bottom[i]);
	}
	free(tree);
}

/*
vebtree * veb_init_tree(int w){

	tree->top = veb_initialize(halfofw, 0);
	
	tree->bottom = malloc(tree->sqrtsize * sizeof(struct vebtree *));
	int i;
	for (i = 0; i < tree->sqrtsize; i++)
		tree->bottom[i] = veb_initialize(w-halfofw, 0);
	
	return tree;
}*/