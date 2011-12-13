#ifndef LINKED_LIST
#define LINKED_LIST

#include <stdlib.h>
#include <stdint.h>

typedef struct linked_list_node linked_list_node;
typedef struct linked_list linked_list;

struct linked_list_node {
	uint32_t data;
	linked_list_node *next;
	linked_list_node *prev;
};

struct linked_list {
	int n;
	linked_list_node *first;
	linked_list_node *last;
};

linked_list * linked_list_init();
int linked_list_add_tail(uint32_t d, linked_list *ll);
int linked_list_merge(linked_list *l1, linked_list *l2);
void linked_list_destruct(linked_list *l);

#endif