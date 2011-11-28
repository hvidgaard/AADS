#include "linked_list.h"

linked_list * linked_list_init(){
	linked_list *ll = malloc(sizeof(linked_list));
	ll->first = NULL;
	ll->last = NULL;
	ll->n = 0;
	return ll;
}

int linked_list_add_tail(uint32_t d, linked_list *ll){
	linked_list_node *node = malloc(sizeof(linked_list_node));
	node->data = d;
	if (ll->n == 0) {
		ll->first = node;
		ll->last = node;
	}
	else {
		ll->last->next = node;
		node->prev = ll->last;
		ll->last = node;
	}
	ll->n++;
	return 0;
}
int linked_list_merge(linked_list *l1, linked_list *l2){
	l1->last->next = l2->first;
	l2->first->prev = l1->last;
	l1->last = l2->last;
	free(l2);
	return 0;
}
void linked_list_destruct(linked_list *l){
	while (l->n > 1){
		linked_list_node *n = l->first;
		l->first = l->first->next;
		free(n);
		l->n--;
	}
	free(l->first);
	free(l);
}