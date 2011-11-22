#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "rbtree.h"

void is_correct(rb_tree* tree) {
	if(!tree->root)
		return;
	rb_node* n = tree->root;
	assert(n->left);
	assert(n->right);
	test_down(n->left);
	test_down(n->right);
	assert(n->left != n->right);
}

void test_down(rb_node* n) {
	assert(n->parent);
	assert(n == n->parent->left || n == n->parent->right);
	if(!n->left) {
		assert(!n->right);
		assert(!n->key);
		return;
	}
	assert(n->right);
	if(n->left->left)
		assert(n->key > n->left->key);
	if(n->right->right)
		assert(n->key <= n->right->key);
	assert(n->left != n->right);
	test_down(n->left);
	test_down(n->right);
}

rb_node* rb_insert(uint32_t key, rb_tree* tree) {
	rb_node* n = calloc(1,sizeof(rb_node));
	n->key = key;
	n->color = RED;
	n->tree = tree;
	n->left = calloc(1,sizeof(rb_node));
	n->left->color = BLACK;
	n->left->parent = n;
	n->left->tree = tree;
	n->right = calloc(1,sizeof(rb_node));
	n->right->color = BLACK;
	n->right->parent = n;
	n->right->tree = tree;
	is_correct(tree);
	if(tree->root)
		tree_insert(n, tree->root);
	else
		tree->root = n;
	is_correct(tree);
	tree->n++;
	insert_case1(n);
	is_correct(tree);
	return n;
}

void rb_delete(rb_node* n) {
	assert(is_leaf(n->left) || is_leaf(n->right));
	delete_one_child(n);
}

rb_node* rb_pred(rb_node* n) {
	if(!is_leaf(n->left))
		return n->left;
	while(n->parent && n->parent->right != n)
		n = n->parent;
	return n->parent;
}

rb_node* rb_succ(rb_node* n) {
	if(!is_leaf(n->right))
		return n->right;
	while(n->parent && n->parent->left != n)
		n = n->parent;
	return n->parent;
}

void tree_insert(rb_node* n, rb_node* leaf) {
	assert(leaf);
	if(is_leaf(leaf)) {
		assert(leaf->parent);
		if(leaf->parent->left == leaf)
			leaf->parent->left = n;
		else
			leaf->parent->right = n;
		n->parent = leaf->parent;
		free(leaf);
	} else {
		if(n->key < leaf->key) {
			tree_insert(n, leaf->left);
		} else {
			tree_insert(n, leaf->right);
		}
	}
}

void delete_one_child(rb_node* n) {
	/*
	* Precondition: n has at most one non-null child.
	*/
	rb_node* child = is_leaf(n->right) ? n->left : n->right;
	
	replace_node(n, child);
	if (n->color == BLACK) {
		if (child->color == RED)
			child->color = BLACK;
		else
			delete_case1(child);
	}
	free(n);
}

void rotate_right(rb_node* pivot) {
	printf("rotate right\n");
	is_correct(pivot->tree);
	rb_node* root = pivot->left;
	rb_node* move = root->right;
	
	root->parent = pivot->parent;
	
	pivot->left = move;
	move->parent = pivot;
	
	root->right = pivot;
	pivot->parent = root;
	
	if(root->parent) {
		if (pivot == root->parent->left)
			root->parent->left = root;
		else
			root->parent->right = root;
	} else {
		printf("New root\n");
		root->tree->root = root;
	}
	is_correct(root->tree);
}

void rotate_left(rb_node* pivot) {
	printf("rotate left\n");
	is_correct(pivot->tree);
	rb_node* root = pivot->right;
	rb_node* move = root->left;
	
	root->parent = pivot->parent;
	
	pivot->right = move;
	move->parent = pivot;
	
	root->left = pivot;
	pivot->parent = root;
	
	if(root->parent) {
		if (pivot == root->parent->left)
			root->parent->left = root;
		else
			root->parent->right = root;
	} else {
		printf("New root\n");
		root->tree->root = root;
	}
	is_correct(root->tree);
}

rb_node* sibling(rb_node* n) {
	if (n == n->parent->left)
		return n->parent->right;
	else
		return n->parent->left;
}

rb_node* grandparent(rb_node* n) {
	if ((n != NULL) && (n->parent != NULL))
		return n->parent->parent;
	else
		return NULL;
}

rb_node* uncle(rb_node* n) {
	rb_node* g = grandparent(n);
	if (g == NULL)
		return NULL; // No grandparent means no uncle
	if (n->parent == g->left)
		return g->right;
	else
		return g->left;
}

int is_leaf(rb_node* n) {
	return n->left == NULL;
}

void replace_node(rb_node* n, rb_node* child) {
	if(n->parent) {
		if(n->parent->left == n)
			n->parent->left = child;
		else
			n->parent->right = child;
		child->parent = n->parent;
	}
}

void insert_case1(rb_node* n) {
	if (n->parent == NULL)
		n->color = BLACK;
	else
		insert_case2(n);
}

void insert_case2(rb_node* n) {
	if (n->parent->color == BLACK)
		return; /* Tree is still valid */
	else
		insert_case3(n);
}

void insert_case3(rb_node* n) {
	rb_node* u = uncle(n), *g; 

	if ((u != NULL) && (u->color == RED)) {
		n->parent->color = BLACK;
		u->color = BLACK;
		g = grandparent(n);
		g->color = RED;
		insert_case1(g);
	}
	else {
		insert_case4(n);
	}
}

void insert_case4(rb_node* n) {
	rb_node* g = grandparent(n);
	
	if ((n == n->parent->right) && (n->parent == g->left)) {
		rotate_left(n->parent);
		n = n->left;
	}
	else if ((n == n->parent->left) && (n->parent == g->right)) {
		rotate_right(n->parent);
		n = n->right;
	}
	insert_case5(n);
}

void insert_case5(rb_node* n) {
	rb_node* g = grandparent(n);
	
	n->parent->color = BLACK;
	g->color = RED;
	if ((n == n->parent->left) && (n->parent == g->left)) {
		rotate_right(g);
	} else if ((n == n->parent->right) && (n->parent == g->right)) {
		rotate_left(g);
	}
}

void delete_case1(rb_node* n) {
	if (n->parent != NULL)
		delete_case2(n);
}

void delete_case2(rb_node* n) {
	rb_node* s = sibling(n);

	if (s->color == RED) {
		n->parent->color = RED;
		s->color = BLACK;
		if (n == n->parent->left)
			rotate_left(n->parent);
		else
			rotate_right(n->parent);
	} 
	delete_case3(n);
}

void delete_case3(rb_node* n) {
	rb_node* s = sibling(n);

	if ((n->parent->color == BLACK) &&
	(s->color == BLACK) &&
	(s->left->color == BLACK) &&
	(s->right->color == BLACK)) {
		s->color = RED;
		delete_case1(n->parent);
	} else {
		delete_case4(n);
	}
}

void delete_case4(rb_node* n) {
	rb_node* s = sibling(n);

	if ((n->parent->color == RED) &&
	(s->color == BLACK) &&
	(s->left->color == BLACK) &&
	(s->right->color == BLACK)) {
		s->color = RED;
		n->parent->color = BLACK;
	} else {
		delete_case5(n);
	}
}

void delete_case5(rb_node* n) {
	rb_node* s = sibling(n);
	
	/* this if statement is trivial, 
	due to Case 2 (even though Case two changed the sibling to a sibling's child, 
	the sibling's child can't be red, since no red parent can have a red child). */
	/* the following statements just force the red to be on the left of the left of the parent, 
	or right of the right, so case six will rotate correctly. */
	if	(s->color == BLACK) { 
		if ((n == n->parent->left) &&
		(s->right->color == BLACK) &&
		(s->left->color == RED)) { /* this last test is trivial too due to cases 2-4. */
			s->color = RED;
			s->left->color = BLACK;
			rotate_right(s);
		} else if ((n == n->parent->right) &&
		(s->left->color == BLACK) &&
		(s->right->color == RED)) {/* this last test is trivial too due to cases 2-4. */
			s->color = RED;
			s->right->color = BLACK;
			rotate_left(s);
		}
	}
	delete_case6(n);
}

void delete_case6(rb_node* n) {
	rb_node* s = sibling(n);

	s->color = n->parent->color;
	n->parent->color = BLACK;

	if (n == n->parent->left) {
		s->right->color = BLACK;
		rotate_left(n->parent);
	} else {
		s->left->color = BLACK;
		rotate_right(n->parent);
	}
}
