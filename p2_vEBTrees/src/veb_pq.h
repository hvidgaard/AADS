#ifndef H_VEB_PQ
#define H_VEB_PQ

#include <stdint.h>
#include "vebtrees.h"

//struct veb_pq_node;
//struct veb_pq_data;
typedef struct veb_pq_node veb_pq_node;
typedef struct veb_pq_data veb_pq_data;

struct veb_pq_node {
	uint32_t node_nr;
	uint32_t node_prio;
	veb_pq_data *parent;
	uint32_t parent_index;
};
struct veb_pq_data {
	uint32_t max_size;
	uint32_t size;
	veb_pq_node ** nodes;
};

vebtree * veb_pq_init(int w);
veb_pq_node * veb_pq_insert(vebtree * tree, uint32_t node_nr, uint32_t node_prio);
veb_pq_node * veb_pq_deletemin(vebtree * tree);
int veb_pq_decrease_key(vebtree * tree, veb_pq_node * node, uint32_t delta);
int veb_pq_delete(vebtree * tree, veb_pq_node * node);

#endif