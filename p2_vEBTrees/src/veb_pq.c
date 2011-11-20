#include "veb_pq.h"
#include <stdlib.h>

vebtree * veb_pq_init(int w){
	return veb_initialize(w, 64);
}
void veb_pq_insert(veb_pq_node * node, vebtree * tree){
	int32_t t;
	veb_pq_data * data = (veb_pq_data *)veb_findsucc(node->node_prio, &t, tree);

	if (t == -1 || t != node->node_prio){
		veb_pq_data * data = malloc(sizeof(veb_pq_data));
		data->n = 1;
		data->first = node;
		node->parent = data;
		veb_insert(node->node_prio, data, tree);
	}
	else{
		node->next = data->first;
		node->next->prev = node;
		node->prev = NULL;
		node->parent = data;
		data->first = node;
		data->n++;
	}
}
int veb_pq_delete(vebtree * tree, veb_pq_node * node){
	if (node->parent->n > 1){
		if (node->next)
			node->next->prev = node->prev;
		if (node->prev)
			node->prev->next = node->next;
		node->parent->n--;
		node->next = NULL;
		node->prev = NULL;
		node->parent = NULL;
	}
	else{
		//veb_pq_data * p = (veb_pq_data *)
		veb_delete(node->node_prio, tree);
		free(node->parent);
		node->next = NULL;
		node->prev = NULL;
		node->parent = NULL;
	}
	return 0;
}
veb_pq_node * veb_pq_deletemin(vebtree * tree){
	if (tree->n == 0)
		return NULL;
	veb_pq_node * n = ((veb_pq_data *)tree->min->data)->first;
	if (veb_pq_delete(tree, n))
		return NULL;
	else
		return n;
}
int veb_pq_decrease_key(vebtree * tree, veb_pq_node * node, uint32_t delta){
	veb_pq_delete(tree, node);
	node->node_prio -= delta;
	veb_pq_insert(node, tree);
	return 0;
}
void veb_pq_destruct(vebtree * tree){
	while(tree->n)
		veb_pq_deletemin(tree);
	veb_destruct(tree);
}

/*
typedef struct veb_pq_node {
	uint32_t node_nr;
	uint32_t node_prio;
	data * parent;
	uint32_t parent_index;
} veb_pq_node;

typedef struct veb_pq_data {
	uint32_t max_size;
	uint32_t size;
	node ** nodes;
} veb_pq_data;
*/