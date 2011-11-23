#ifndef H_RB_TREE
#define H_RB_TREE

#include <stdint.h>

#define BLACK 1
#define RED 0


typedef struct rb_node rb_node;
typedef struct rb_tree rb_tree;

struct rb_node {
	uint32_t key;
	int color;
	rb_node* parent;
	rb_node* right;
	rb_node* left;
	rb_tree* tree;
	void* data;
};

struct rb_tree {
	rb_node* root;
	rb_node* nil;
	uint32_t n;
};

rb_tree* rb_init();
void rb_destruct(rb_tree* tree);
rb_node* rb_insert(uint32_t key, rb_tree* tree);
void rb_delete(rb_node* n, rb_tree* tree);
rb_node* rb_pred(rb_node* n, rb_tree* tree);
rb_node* rb_succ(rb_node* n, rb_tree* tree);
rb_node * rb_find_min(rb_tree * tree);

void is_correct(rb_tree* tree);
void test_down(rb_node* n, rb_tree* tree);

void tree_insert(rb_node* n, rb_node* leaf, rb_tree* tree);
void delete_one_child(rb_node* n, rb_tree* tree);
int is_leaf(rb_node* n, rb_tree* tree);
void replace_node(rb_node* n, rb_node* child, rb_tree* tree);

void rotate_right(rb_node* pivot, rb_tree* tree);
void rotate_left(rb_node* pivot, rb_tree* tree);
rb_node* sibling(rb_node* n);
rb_node* grandparent(rb_node* n);
rb_node* uncle(rb_node* n);

void insert_case1(rb_node* n, rb_tree* tree);
void insert_case2(rb_node* n, rb_tree* tree);
void insert_case3(rb_node* n, rb_tree* tree);
void insert_case4(rb_node* n, rb_tree* tree);
void insert_case5(rb_node* n, rb_tree* tree);
void delete_case1(rb_node* n, rb_tree* tree);
void delete_case2(rb_node* n, rb_tree* tree);
void delete_case3(rb_node* n, rb_tree* tree);
void delete_case4(rb_node* n, rb_tree* tree);
void delete_case5(rb_node* n, rb_tree* tree);
void delete_case6(rb_node* n, rb_tree* tree);

#endif