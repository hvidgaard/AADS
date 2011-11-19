#include "veb_pq.h"
#include <stdlib.h>

vebtree * veb_pq_init(int w){
	return veb_initialize(w, 64);
}
veb_pq_node * veb_pq_insert(vebtree * tree, uint32_t node_nr, uint32_t node_prio){
	veb_pq_node * node = malloc(sizeof(veb_pq_node));
	node->node_nr = node_nr;
	node->node_prio = node_prio;
	
	veb_pq_data * data = malloc(sizeof(veb_pq_data));
	
	int32_t t = veb_findsucc(node_prio, data, tree);
	if (t == -1)
		return NULL;
	else if (t != node_prio){
		data->max_size = 2;
		data->size = 1;
		data->nodes = malloc(2 * sizeof(veb_pq_node *));
		data->nodes[0] = node;
		node->parent = data;
		node->parent_index = 0;
		veb_insert(node_prio, data, tree);
	}
	else{
		if (data->max_size == data->size) {
			veb_pq_node ** arr = malloc((data->max_size) * 2 * sizeof(veb_pq_node *));
			int i;
			for (i = 0; i < data->max_size; i++)
				arr[i] = data->nodes[i];
			data->max_size = data->max_size * 2;
			data->size++;
			free(data->nodes);
			data->nodes = arr;
		}
		data->nodes[data->size + 1] = node;
		node->parent_index = data->size + 1;
		node->parent = data;
		data->size++;
	}
	return node;
}
veb_pq_node * veb_pq_deletemin(vebtree * tree){
	if (tree->n == 0)
		return NULL;
	veb_pq_node * n = ((veb_pq_node **)(tree->min->data))[0];
	if (veb_pq_delete(tree, ((veb_pq_node **)(tree->min->data))[0]))
		return NULL;
	else
		return n;
}
int veb_pq_decrease_key(vebtree * tree, veb_pq_node * node, uint32_t delta){
	veb_pq_delete(tree, node);
	veb_pq_insert(tree, node->node_nr, node->node_prio - delta);
	free(node);
	return 0;
}
int veb_pq_delete(vebtree * tree, veb_pq_node * node){
	veb_pq_data * parent = node->parent;
	parent->size--;
	if (parent->size > 0)
		parent->nodes[node->parent_index] = parent->nodes[parent->size];
	else {
		veb_delete(node->node_prio, NULL, tree);
		free(parent->nodes);
		free(parent);
	}
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