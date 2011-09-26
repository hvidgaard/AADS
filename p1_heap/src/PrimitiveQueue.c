#include <stdlib.h>
#include <PrimitiveQueue.h>

PrimitiveQueue *pq_make_heap()
{
	struct PrimitiveQueue *queue = malloc(sizeof *queue);
	queue->node = NULL;
	return queue;
}

PrimitiveNode *pq_find_min(PrimitiveQueue *queue) {
	if(!queue->node)
		return NULL;
	PrimitiveNode *min;
	PrimitiveNode *current;
	min = current = queue->node;
	do {
		if(current->key < min->key)
			min = current;
	} while(current = current->next);
	return min;
}

PrimitiveNode *pq_insert(unsigned int key, void *data, PrimitiveQueue *queue)
{
	PrimitiveNode *node = malloc(sizeof(struct PrimitiveNode));
	node->key = key;
	node->data = data;
	node->next = queue->node;
	queue->node = node;
	return node;
}

void pq_delete_min(PrimitiveQueue *queue) {
	PrimitiveNode *min = pq_find_min(queue);
	if(min == queue->node) {
		queue->node = min->next;
		return;
	}
	PrimitiveNode *current = queue->node;
	do {
		if(current->next == min) {
			current->next = min->next;
			return;
		}
	} while(current = current->next);
	abort();
}

void pq_decrease_key(unsigned int delta, PrimitiveNode *node, PrimitiveQueue *queue) {
	node->key -= delta;
}