#include <stdlib.h>
#include <math.h>
#include <stdio.h>
#include <unistd.h>
#include "debug.h"
#include "vebtrees.h"


/* declerations of internal functions not intented for
 * external use.
 */
void veb_init_tree(vebtree * t, int threshold);
void veb_init_leaf(vebtree * t);
int32_t veb_insert_leaf(uint32_t index, void * data, vebtree * tree);
int32_t veb_insert_tree(uint32_t index, void * data, vebtree * tree);
void * veb_delete_tree(uint32_t index, vebtree * tree);
void * veb_delete_leaf(uint32_t index, vebtree * tree);
void vebfactor(int w, uint32_t i, uint32_t * a, uint32_t * b);
void veb_destruct(vebtree *tree);

/*w is number of bits to use for the elements. Note
 *that the size of the entire datastructure will be 
 *2^size...
 *threshold is the amount of bits from where to degrade
 *to a linear search through an array.
 */
vebtree * veb_initialize(int w, int threshold){
	vebtree * t = malloc(sizeof(vebtree));
	t->n = 0;
	t->size = pow(2, w);
	t->sqrtsize = pow(2, w-(w/2));
	t->w = w;
	if (t->size <= threshold)
		veb_init_leaf(t);
	else
		veb_init_tree(t, threshold);
	return t;
}
void veb_init_leaf(vebtree * t){
	t->leaf = 1;
	t->arr = calloc(t->size, sizeof(vebelement));
}
void veb_init_tree(vebtree * t, int threshold){
	uint32_t w = t->w;
	t->leaf = 0;
	t->min = malloc(sizeof(vebelement));
	t->max = malloc(sizeof(vebelement));
	t->top = veb_initialize(w/2, threshold);
	t->bottom = malloc(t->top->size * sizeof(vebtree *));
	int i;
	for (i = 0; i < t->top->size; i++)
		t->bottom[i] = veb_initialize(w-(w/2), threshold);
}
int32_t veb_insert(uint32_t index, void * data, vebtree * tree){
	#ifdef DEBUG
	if (index > tree->size){
		printf("cannot insert element larger than tree size\n");
		exit(1);
	}
	#endif
	if(tree->leaf)
		return veb_insert_leaf(index, data, tree);
	else
		return veb_insert_tree(index, data, tree);
}
int32_t veb_insert_leaf(uint32_t index, void * data, vebtree * t){
	#ifdef DEBUG
	if (t->arr[index].value || (t->n > 0 && index == 0 && t->min->value == 0)){
		printf("element already present in leaf\n");
		exit(1);
	}
	#endif
	t->arr[index].value = index;
	t->arr[index].data = data;
	if (t->n){
		if (index > t->max->value)
			t->max = &t->arr[index];
		else if (index < t->min->value)
			t->min = &t->arr[index];
	}
	else {
		t->min = &t->arr[index];
		t->max = &t->arr[index];
	}
	t->n++;
	#ifdef DEBUG
	if (t->n == 1 && (t->min != t->max)){
		printf("element min and max should be identical when n = 1\n");
		exit(1);
	}
	if (t->n == 2 && (t->min == t->max)){
		printf("element min and max should not the identical when n = 2\n");
		exit(1);
	}
	#endif
	return 0;
}
int32_t veb_insert_tree(uint32_t index, void * data, vebtree * t){
	if(t->n == 0){
		t->min->value = index;
		t->min->data = data;
		t->max->value = index;
		t->max->data = data;
		t->n++;
		return 0;
	}
	else if(t->n == 1) {
		#ifdef DEBUG
		if (t->min->value != t->max->value){
			printf("min and max should be the same when n = 1\n");
			exit(1);
		}
		if (t->min->value == index){
			printf("value already present in tree (as min-max)\n");
			exit(1);
		}
		#endif
		if (index < t->min->value){
			t->min->value = index;
			t->min->data = data;
		}
		else{
			t->max->value = index;
			t->max->data = data;
		}
		t->n++;
		return 0;
	}
	else {
		#ifdef DEBUG
		if (t->min->value == index){
			printf("value already present in tree (as min)\n");
			exit(1);
		}
		if (t->max->value == index){
			printf("value already present in tree (as max)\n");
			exit(1);
		}
		#endif
		if (index < t->min->value){
			uint32_t minval = t->min->value;
			void * mindat = t->min->data;
			t->min->value = index;
			t->min->data = data;
			index = minval;
			data = mindat;
			//return veb_insert(minval, mindat, t);
		}
		else if (index > t->max->value){
			uint32_t maxval = t->max->value;
			void * maxdat = t->max->data;
			t->max->value = index;
			t->max->data = data;
			index = maxval;
			data = maxdat;
			//return veb_insert(maxval, maxdat, t);
		}
		uint32_t a, b;
		vebfactor(t->w, index, &a, &b);
		if (t->bottom[a]->n == 0)
			veb_insert(a, NULL, t->top);
		veb_insert(b, data, t->bottom[a]);
		t->n++;
		return 0;
	}
}
void * veb_delete(uint32_t index, vebtree * tree){
	#ifdef DEBUG
	if (tree->n == 0){
		printf("cannot delete from empty tree\n");
		exit(2);
	}
	if (index > tree->size){
		printf("cannot delete element larger than the tree\n");
		exit(2);
	}
	#endif
	if (tree->leaf)
		return veb_delete_leaf(index, tree);
	else
		return veb_delete_tree(index, tree);
}
void * veb_delete_leaf(uint32_t index, vebtree * tree){
	#ifdef DEBUG
	if ((index != 0 && tree->arr[index].value == 0) || (tree->n > 0 && index == 0 && tree->min->value != 0)){
		printf("element not present in leaf\n");
		exit(2);
	}
	if (tree->n == 1 && tree->min != tree->max){
		printf("min and max should be the same element because n = 1, they're not\n");
		exit(2);
	}
	if (tree->n == 2 && tree->min == tree->max){
		printf("min and max should not be the same element because n = 2, they are\n");
		exit(2);
	}
	if (tree->n == 2 && index != tree->min->value && index != tree->max->value){
		printf("index is expected to be min or max since n = 2\n");
		exit(2);
	}
	#endif
	if (tree->n > 1){
		int32_t t;
		if (tree->min->value == index){
			veb_findsucc(index+1, &t, tree);
			#ifdef PRINT
			int i;
			printf("succ(%d) = %d in |", index, t);
			for (i=0; i < tree->size; i++)
			printf("%d|", tree->arr[i].value);
			printf("\n");
			#endif
			tree->min = &tree->arr[t];
		}
		else if (tree->max->value == index){
			veb_findpred(index-1, &t, tree);
			#ifdef PRINT
			int j;
			printf("pred(%d) = %d in |", index, t);
			for (j=0; j < tree->size; j++)
				printf("%d|", tree->arr[j].value);
			printf("\n");
			#endif
			tree->max = &tree->arr[t];
		}
	}
	tree->arr[index].value = 0;
	tree->n--;
	#ifdef DEBUG
	int j;
	if (tree->n == 1 && (tree->min != tree->max)){
		printf("element min and max should be identical when n = 1\n");
		#ifdef PRINT
		printf("min is %d and max is %d in |", tree->min->value, tree->max->value);
		for (j=0; j < tree->size; j++)
				printf("%d|", tree->arr[j].value);
			printf("\n");
		#endif
		exit(2);
	}
	if (tree->n == 2 && (tree->min == tree->max)){
		printf("element min and max should not the identical when n = 2\n");
		#ifdef PRINT
		printf("min is %d and max is %d in |", tree->min->value, tree->max->value);
		for (j=0; j < tree->size; j++)
				printf("%d|", tree->arr[j].value);
			printf("\n");
		#endif
		exit(2);
	}
	#endif
	return tree->arr[index].data;
}
void * veb_delete_tree(uint32_t index, vebtree * tree){
	#ifdef DEBUG
	if (tree->n == 1 && tree->min->value != tree->max->value){
		printf("min and max are expected to be the same since n = 1\n");
		exit(2);
	}
	if (tree->n == 1 && index != tree->min->value){
		printf("there's only one element, and index is not it\n");
		exit(2);
	}
	if (tree->n == 2 && index != tree->min->value && index != tree->max->value){
		printf("index is not min or max, and that's the only 2 elements\n");
		exit(2);
	}
	#endif
	void * result = NULL;
	int set = 0;
	if (tree->n == 1){
		result = tree->min->data;
	}
	else if (tree->n == 2){
		if (tree->max->value == index){
			result = tree->max->data;
			tree->max->value = tree->min->value;
			tree->max->data = tree->min->data;
		}
		else{
			result = tree->min->data;
			tree->min->value = tree->max->value;
			tree->min->data = tree->max->data;
		}
	}
	else {
		#ifdef DEBUG
		uint32_t aa, bb;
		uint32_t s = 0;
		#endif
		if (tree->min->value == index){
			result = tree->min->data;
			set = 1;
			#ifdef DEBUG
				s = 1;
				aa = tree->top->min->value;
				bb = tree->bottom[tree->top->min->value]->min->value;
			#endif
			tree->min->value = tree->top->min->value * tree->sqrtsize + tree->bottom[tree->top->min->value]->min->value;
			tree->min->data = tree->bottom[tree->top->min->value]->min->data;
			index = tree->min->value;
			
		}
		else if (tree->max->value == index){
			result = tree->max->data;
			set = 1;
			#ifdef DEBUG
				s = 1;
				aa = tree->top->max->value;
				bb = tree->bottom[tree->top->max->value]->max->value;
			#endif
			tree->max->value = tree->top->max->value * tree->sqrtsize + tree->bottom[tree->top->max->value]->max->value;
			tree->max->data = tree->bottom[tree->top->max->value]->max->data;
			index = tree->max->value;
		}
		uint32_t a, b;
		vebfactor(tree->w, index, &a, &b);
		#ifdef DEBUG
		if (s && a != aa){
			printf("factoring was wrong - a - %d != %d\n", a, aa);
			exit (2);
		}
		if (s && b != bb){
			printf("factoring was wrong - b - %d != %d\n", b, bb);
			exit (2);
		}
		#endif
		if (set)
			veb_delete(b, tree->bottom[a]);
		else
			result = veb_delete(b, tree->bottom[a]);
		if (tree->bottom[a]->n == 0)
			veb_delete(a, tree->top);
	}
	tree->n--;
	return result;
}
void * veb_findsucc(uint32_t index, int32_t * succ, vebtree * tree){
	void * result = NULL;
	if (tree->n == 0){
		*succ = -1;
		return NULL;
	}
	else if (index > tree->max->value && tree->n > 0){
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
	else if (tree->leaf){
		int i;
		for (i = index; i < tree->size; i ++){
			if (tree->arr[i].value){
				*succ = i;
				return tree->arr[i].data;
			}
		}
		*succ = -1;
		return NULL;
	}
	else {
		uint32_t a, b;
		vebfactor(tree->w, index, &a, &b);
		if (tree->bottom[a]->n > 0 && tree->bottom[a]->max->value >= b){
			*succ = a * tree->sqrtsize;
			int32_t r;
			result = veb_findsucc(b, &r, tree->bottom[a]);
			*succ += r;
			return result;
		}
		else if (tree->top->max->value <= a){
			*succ = tree->max->value;
			return tree->max->data;
		}
		else {
			int32_t c;
			veb_findsucc(a+1, &c, tree->top);
			*succ = c * tree->sqrtsize + tree->bottom[c]->min->value;
			return tree->bottom[c]->min->data;
		}
	}
}
void * veb_findpred(uint32_t index, int32_t * pred, vebtree * tree){
	void * result = NULL;
	if (tree->n == 0){
		*pred = -1;
		return NULL;
	}
	else if (index < tree->min->value && tree->n > 0){
		*pred = -1;
		return NULL;
	}
	else if (index >= tree->max->value){
		*pred = tree->max->value;
		return tree->max->data;
	}
	else if (tree->n <= 2){
		*pred = tree->min->value;
		return tree->min->data;
	}
	else if (tree->leaf){
		int i;
		for (i = index; i >= 0; i --){
			if (tree->arr[i].value){
				*pred = i;
				return tree->arr[i].data;
			}
		}
		*pred = -1;
		return NULL;
	}
	else {
		uint32_t a, b;
		vebfactor(tree->w, index, &a, &b);
		if (tree->bottom[a]->n > 0 && tree->bottom[a]->min->value <= b){
			*pred = a * tree->sqrtsize;
			int32_t r;
			result = veb_findpred(b, &r, tree->bottom[a]);
			*pred += r;
			return result;
		}
		else if (tree->top->min->value >= a){
			*pred = tree->min->value;
			return tree->min->data;
		}
		else {
			int32_t c;
			veb_findpred(a-1, &c, tree->top);
			*pred = c * tree->sqrtsize + tree->bottom[c]->max->value;
			return tree->bottom[c]->max->data;
		}
	}
}


/*
void * veb_delete_min(vebtree * tree);

void * veb_extract_min(vebtree * tree, int32_t * index);*/

/* Given a bitlength w, integer i to factor and pointer a, b
 * to where the upper and lower half should be - it will factor
 * i by simple bitshifting.
 */
void vebfactor(int w, uint32_t i, uint32_t * a, uint32_t * b){
	*a = i >> (w-(w/2));
	//we want to clear the upper bits.
	*b = i << (32-(w-(w/2)));
	//printf("what: %d", *b);
	*b = *b >> (32-(w-(w/2)));
}
/* given a pointer to a tree, it will free all memory allocated to
 * that tree. Any data that was still in the tree, will not be freed
 */
void veb_destruct(vebtree *tree){
	if (tree->leaf)
		free(tree->arr);
	else {
		free(tree->max);
		free(tree->min);
		veb_destruct(tree->top);
		int i;
		for (i = 0; i < tree->sqrtsize; i++)
			veb_destruct(tree->bottom[i]);
		free(tree->bottom);
	}
	free(tree);
}










void * veb_delete_min(vebtree * tree){
	int32_t r;
	veb_findsucc(0, &r,tree);
	return veb_delete(r, tree);
}

/*void * veb_findsucc(uint32_t index, int32_t * succ, vebtree * tree){
	//in a recursive tree
	else {
		uint32_t * t_a = malloc(sizeof(uint32_t));
		uint32_t * t_b = malloc(sizeof(uint32_t));
		vebfactor (tree->w, index, t_a, t_b);
		int32_t a = *t_a;
		int32_t b = *t_b;
		free(t_a);
		free(t_b);
		int32_t n_a, n_b;
		veb_findsucc(a, &n_a, tree->top);
		if (n_a == -1){
			*succ = tree->max->value;
			return tree->max->data;
		}
		else if (a == n_a){
			void * d = veb_findsucc(b, &n_b, tree->bottom[a]);
			if (n_b < b){
				*succ = tree->max->value;
				return tree->max->data;
			}
			else {
				*succ = a * tree->sqrtsize + n_b;
				return d;
			}
		}
		else {
			*succ = n_a * tree->sqrtsize + tree->bottom[n_a]->min->value;
			return tree->bottom[n_a]->min->data;
		}
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
}*/
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
/*int32_t veb_insert(uint32_t index, void * data, vebtree * tree){
	//the element shouldn't be stored as the index is too large
	if (index > tree->size){
		#ifdef DEBUG
		printf("For stort element\n");
		exit(24);
		#endif
		return 1;
	}
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
	#ifdef DEBUG
	if (index == tree->min->value){
		printf("igen noget der allerede er der....\n");
		exit(40);
	}
	else if (index == tree->max->value){
		printf("igen noget der allerede er der----\n");
		exit(41);
	}
	#endif
	if (index < tree->min->value) {
		vebfactor(tree->w, tree->min->value, a, b);
		tree->min->data = data;
		tree->min->value = index;
		veb_insert(*b, tree->min->data, tree->bottom[*a]);
		//vebswap(&index, &data, tree->min);
	}
	else if (index > tree->max->value) {
		vebfactor(tree->w, tree->max->value, a, b);
		tree->max->data = data;
		tree->max->value = index;
		veb_insert(*b, tree->max->data, tree->bottom[*a]);
		//vebswap(&index, &data, tree->max);
	}
	else {
		vebfactor(tree->w, index, a, b);
		veb_insert(*b, data, tree->bottom[*a]);
	}
	if (tree->bottom[*a]->n == 1)
		veb_insert(*a, NULL, tree->top);
	free(a);
	free(b);
	tree->n++;
	return 0;
}
int32_t veb_insert_leaf(uint32_t index, void * data, vebtree * tree){
	if ((tree->arr)[index].value){
		#ifdef DEBUG
		printf("trying to insert where there already is an element\n");
		exit(14);
		#endif
		return 1;
	}
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
	else{
		tree->min = &(tree->arr)[index];
		tree->max = &(tree->arr)[index];
	}
	tree->n++;
	return 0;
}
int32_t veb_insert_node_trivial(uint32_t index, void * data, vebtree * tree){
	#ifdef DEBUG
	if (tree->n > 1){
		printf("oups, skulle ikke kunne komme her...\n");
		exit(25);
	}
	#endif
	if (tree->n == 0){
		tree->max->value = index;
		tree->min->value = index;
		tree->max->data = data;
		tree->min->data = data;
	}
	#ifdef DEBUG
	else if (index == tree->max->value){
		printf("ups, inserting something that is there\n");
		exit(30);
	}
	else if (index == tree->min->value){
		printf("ups, inserting something that is there---2\n");
		exit(31);
	}
	#endif
	else if (index > tree->max->value){
		tree->min->value = tree->max->value;
		tree->min->data = tree->max->data;
		tree->max->value = index;
		tree->max->data = data;
	}
	else {
		tree->min->value = index;
		tree->min->data = data;
	}
	tree->n++;
	return 0;
}
*/
void vebswap(uint32_t *index, void **data, vebelement *e){
	void *tmp_data = e->data;
	uint32_t tmp_idx = e->value;
	e->data = *data;
	e->value = *index;
	*data = tmp_data;
	*index = tmp_idx;
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