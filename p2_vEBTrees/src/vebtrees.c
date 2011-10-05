#include <vebtrees.h>
#include <stdlib.h>
#include <math.h>

//size is number of bits to use for the elements. Note
//that the size of the entire datastructure will be 
//2^size...
//threshold is the amount of bits from where to degrade
//to a linear search through an array.
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
	tree->size = pow(2, w);
	tree->threshold = threshold;
	tree->n = 0;
	tree->max = 0;
	tree->max_data = malloc(sizeof(void *));
	tree->min = 0;
	tree->min_data = malloc(sizeof(void *));
	//note, the size here, is in the amount of bits needed, same for sqrt.
	tree->w = w;
	tree->top = veb_initialize(w/2, threshold);
	tree->bottom = calloc(pow(2, w/2), sizeof(struct vebtree *));
	int i = 0;
	for (i = 0; i < pow(2, w/2); i++)
		tree->bottom[i] = veb_initialize(w/2, threshold);
	return tree;
}
vebtree * veb_init_leaf(int w, int threshold){
	vebtree * tree = malloc(sizeof(struct vebtree));
	tree->size = pow(2, w);
	tree->threshold = threshold;
	tree->n = 0;
	tree->max = 0;
	tree->min = 0;
	tree->size = 0;
	tree->top = NULL;
	tree->bottom = NULL;
	tree->arr = calloc(tree->size, sizeof(void **));
	int i = 0;
	while (i < tree->size){
		tree->arr[i] = 0;
		i++;
	}
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
uint32_t veb_insert(uint32_t index, void * data, vebtree * tree){
	//this first block is handling the leaf trees
	//***********************************************************
	//the element shouldn't be stored as the index is too large
	if (index > tree->size)
		return 0;
	tree->n++;
	//the tree is at the threshold, thus it's just a simple array
	if (tree->size < tree->threshold) {
		if (tree->arr[index])
			return 0; //the element already exists, so return 0 to indicate error.
		tree->arr[index] = data;
		return index;
	}
	//***********************************************************
	else{
		uint32_t * a;
		uint32_t * b;
		vebfactor(tree->w, index, a, b);
		if (tree->n == 0){
			tree->n = 1;
			tree->max = index;
			tree->max_data = data;
			return index;
		}
		else if(tree->n == 1){
			tree->n = 2;
			if (index > tree->max){
				tree->min = tree->max;
				tree->min_data = tree->max_data;
				tree->max = index;
				tree->max_data = data;
			}
			else{
				tree->min = index;
				tree->min_data = data;
			}
			return index;
		}
		else {
			if (index < tree->min)
				vebswap(tree->min_data, data, &(tree->min), &index);
			else if (index > tree->max)
				vebswap(tree->max_data, data, &(tree->max), &index);
			uint32_t * a = malloc(sizeof(uint32_t));
			uint32_t * b = malloc(sizeof(uint32_t));	
			vebfactor(tree->w, index, a, b);
			if (tree->bottom[*a]->size == 0)
				veb_insert(*a, NULL, tree->top);
			veb_insert(*b, data, tree->bottom[*a]);
			free(a);
			free(b);
			return index;
		}
	}
}

void vebfactor(int w, uint32_t i, uint32_t * a, uint32_t * b){
	*a = i >> w-(w/2);
	//we want to clear the upper bits.
	*b = i << (w/2);
	*b = *b >> (w/2);
}
void vebswap(void * d1, void * d2, uint32_t * i1, uint32_t * i2) {
	void * dt = d1;
	d1 = d2;
	d2 = dt;
	uint32_t it = i1;
	i1 = i2;
	i2 = it;
}