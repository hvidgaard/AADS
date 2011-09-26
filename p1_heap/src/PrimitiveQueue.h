#ifndef PRIMQ_H
#define PRIMQ_H

typedef struct PrimitiveNode {
	unsigned int key;
	void *data;
	struct PrimitiveNode* next;
} PrimitiveNode;

typedef struct PrimitiveQueue {
	PrimitiveNode *node;
} PrimitiveQueue;

PrimitiveQueue *pq_make_heap();
PrimitiveNode *pq_find_min(PrimitiveQueue *queue);
PrimitiveNode *pq_insert(unsigned int key, void *data, PrimitiveQueue *queue);
void pq_delete_min(PrimitiveQueue *queue);
void pq_decrease_key(unsigned int delta, PrimitiveNode *node, PrimitiveQueue *queue);

#endif