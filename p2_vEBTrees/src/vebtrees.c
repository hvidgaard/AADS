#include <vebtrees.h>
#include <stdlib.h>
#include <math.h>

//to make the recursive definition more simple
//and because powers of 2 are generally easier
//to deal with, make the size be the nearst
//power of 2.
vebtree * veb_initialize(uint32_t size, int threshold){
	if (size != threshold) {
		uint32_t realsize = 2;
		while (realsize < size)
			realsize = realsize*realsize;
		return veb_init_tree(realsize, threshold);
	}
	else {
		//Not checking if size is a power of 2, but it must be
		return veb_init_leaf(size, threshold);
	}
}

vebtree * veb_init_tree(uint32_t size, int threshold){
	vebtree * tree = malloc(sizeof(struct vebtree));
	tree->size = size;
	tree->threshold = threshold;
	tree->n = 0;
	tree->max = 0;
	tree->min = 0;
	tree->sqroot = sqrt(size);
	tree->top = veb_initialize(sqrt(size), threshold);
	tree->bottom = malloc(sqrt(size) * sizeof(struct vebtree *));
	int i = 0;
	while (i < sqrt(size)) {
		tree->bottom[i] = veb_initialize(sqrt(size), threshold);
		i++;
	}
	return tree;
}
vebtree * veb_init_leaf(int size, int threshold){
	vebtree * tree = malloc(sizeof(struct vebtree));
	tree->size = size;
	tree->threshold = threshold;
	tree->n = 0;
	tree->max = 0;
	tree->min = 0;
	tree->size = 0;
	tree->top = NULL;
	tree->bottom = NULL;
	tree->arr = malloc(size * sizeof(struct vebelement *));
	int i = 0;
	while (i < size){
		tree->arr[i].data = 0;
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
	vebelement * element = malloc(sizeof(struct vebelement *));
	element->data = data;
	//the tree is at the threshold, thus it's just a simple array
	if (tree->size == tree->threshold) {
		if (tree->arr[index].data)
			return 0; //the element already exists, so return 0 to indicate error.
		tree->arr[index].data = element;
		return index;
	}
	else{
		uint32_t * a;
		uint32_t * b;
		vebfactor(tree->sqroot, index, a, b);
		if (tree->n == 0){
			tree->n = 1;
			tree->min = index;
			tree->max = index;
			veb_insert(*a, data, tree->top);
			veb_insert(*b, data, tree->bottom[*b]);
		}
		else {
			
			if (index < tree->min)
				
		}
		return index;
	}
}

void vebfactor(uint32_t sqroot, uint32_t i, uint32_t * a, uint32_t * b){
	*a = 1;
	while ((*a) * sqroot< i)
		*a++;
	*a = i - 1;
	*b = i % sqroot;
}