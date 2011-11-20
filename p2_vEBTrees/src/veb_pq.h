#ifndef H_VEB_PQ
#define H_VEB_PQ

#include <stdint.h>
#include "vebtrees.h"

typedef struct veb_pq_node veb_pq_node;
typedef struct veb_pq_data veb_pq_data;

struct veb_pq_node {
	uint32_t node_nr;
	uint32_t node_prio;
	veb_pq_data *parent;
	veb_pq_node *next;
	veb_pq_node *prev;
};
struct veb_pq_data {
	uint32_t n;
	veb_pq_node * first;
};

vebtree * veb_pq_init(int w);
void veb_pq_insert(veb_pq_node * n, vebtree * tree);
veb_pq_node * veb_pq_deletemin(vebtree * tree);
veb_pq_node * veb_pq_findmin(vebtree * tree);
int veb_pq_decrease_key(vebtree * tree, veb_pq_node * node, uint32_t delta);
int veb_pq_delete(vebtree * tree, veb_pq_node * node);
void veb_pq_destruct(vebtree * tree);

#endif