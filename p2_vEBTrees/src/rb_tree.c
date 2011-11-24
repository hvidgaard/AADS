#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "debug.h"
#include "rb_tree.h"

rb_tree* rb_init() {
	rb_tree* tree = calloc(1, sizeof(rb_tree));
	tree->nil = calloc(1,sizeof(rb_node));
	tree->nil->color = BLACK;
	return tree;
}
int rb_search(int key, rb_tree * rbt){
	rb_node *current = rbt->root;
	while (current->key != key){
		if (key > current->key)
			current = current->right;
		else
			current = current->left;
	}
	return current->key;
}

void rb_destruct(rb_tree* tree) {
	rb_destruct_node(tree->root, tree);
	free(tree->nil);
	free(tree);
}
void rb_destruct_node(rb_node *n, rb_tree* tree) {
	if (n->left != tree->nil)
		rb_destruct_node(n->left, tree);
	if (n->right != tree->nil)
		rb_destruct_node(n->right, tree);
	free (n);
}

rb_node * rb_find_min(rb_tree * tree){
	rb_node *n = tree->root;
	while (n->left != tree->nil)
		n = n->left;
	return n;
}

rb_node* rb_insert(uint32_t key, rb_tree* tree) {
	rb_node* n = calloc(1,sizeof(rb_node));
	n->key = key;
	n->color = RED;
	n->left = tree->nil;
	n->right = tree->nil;
	
	#ifdef DEBUG
	is_correct(tree);
	#endif
	
	if(tree->root)
		tree_insert(n, tree->root, tree);
	else
		tree->root = n;
	
	#ifdef DEBUG
	is_correct(tree);
	#endif
	
	insert_case1(n, tree);
	
	#ifdef DEBUG
	is_correct(tree);
	#endif
	
	tree->n++;
	return n;
}

void rb_delete(rb_node* n, rb_tree* tree) {
	#ifdef DEBUG
	if(!is_leaf(n->left, tree))
		assert(is_leaf(n->right, tree));
	if(!is_leaf(n->right, tree))
		assert(is_leaf(n->left, tree));
	#endif
	
	delete_one_child(n, tree);
	tree->n--;
}

rb_node* rb_pred(rb_node* n, rb_tree* tree) {
	if(!is_leaf(n->left, tree))
		return n->left;
	while(n->parent && n->parent->right != n)
		n = n->parent;
	return n->parent;
}

rb_node* rb_succ(rb_node* n, rb_tree* tree) {
	if(!is_leaf(n->right, tree))
		return n->right;
	while(n->parent && n->parent->left != n)
		n = n->parent;
	return n->parent;
}



void tree_insert(rb_node* n, rb_node* parent, rb_tree* tree) {
	#ifdef DEBUG
	assert(parent);
	#endif
	
	if(n->key < parent->key) {
		if(is_leaf(parent->left, tree)) {
			parent->left = n;
			n->parent = parent;
		} else {
			tree_insert(n, parent->left, tree);
		}
	} else {
		if(is_leaf(parent->right, tree)) {
			parent->right = n;
			n->parent = parent;
		} else {
			tree_insert(n, parent->right, tree);
		}
	}
}

void delete_one_child(rb_node* n, rb_tree* tree) {
	/*
	* Precondition: n has at most one non-null child.
	*/
	rb_node* child = is_leaf(n->right, tree) ? n->left : n->right;
	
	replace_node(n, child, tree);
	if (n->color == BLACK) {
		if (child->color == RED)
			child->color = BLACK;
		else
			delete_case1(child, tree);
	}
	free(n);
}

void rotate_right(rb_node* pivot, rb_tree* tree) {
	#ifdef DEBUG
	is_correct(tree);
	assert(!is_leaf(pivot->left, tree));
	#endif
	
	rb_node* root = pivot->left;
	rb_node* move = root->right;
	
	root->parent = pivot->parent;
	
	pivot->left = move;
	if(!is_leaf(move, tree))
		move->parent = pivot;
	
	root->right = pivot;
	pivot->parent = root;
	
	if(root->parent) {
		if (pivot == root->parent->left)
			root->parent->left = root;
		else
			root->parent->right = root;
	} else {
		tree->root = root;
	}
	
	#ifdef DEBUG
	is_correct(tree);
	#endif
}

void rotate_left(rb_node* pivot, rb_tree* tree) {
	#ifdef DEBUG
	is_correct(tree);
	assert(!is_leaf(pivot->right, tree));
	#endif
	
	rb_node* root = pivot->right;
	rb_node* move = root->left;
	
	root->parent = pivot->parent;
	
	pivot->right = move;
	if(!is_leaf(move, tree))
		move->parent = pivot;
	
	root->left = pivot;
	pivot->parent = root;
	
	if(root->parent) {
		if (pivot == root->parent->left)
			root->parent->left = root;
		else
			root->parent->right = root;
	} else {
		tree->root = root;
	}
	
	#ifdef DEBUG
	is_correct(tree);
	#endif
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

int is_leaf(rb_node* n, rb_tree* tree) {
	#ifdef DEBUG
	assert(n != NULL);
	#endif
	
	return n == tree->nil;
}

void replace_node(rb_node* n, rb_node* child, rb_tree* tree) {
	if(n->parent) {
		if(n->parent->left == n)
			n->parent->left = child;
		else
			n->parent->right = child;
	}
	if(!is_leaf(child, tree))
		child->parent = n->parent;
}

void insert_case1(rb_node* n, rb_tree* tree) {
	if (n->parent == NULL)
		n->color = BLACK;
	else
		insert_case2(n, tree);
}

void insert_case2(rb_node* n, rb_tree* tree) {
	if (n->parent->color == BLACK)
		return; /* Tree is still valid */
	else
		insert_case3(n, tree);
}

void insert_case3(rb_node* n, rb_tree* tree) {
	rb_node* u = uncle(n), *g; 

	if ((u != NULL) && (u->color == RED)) {
		n->parent->color = BLACK;
		u->color = BLACK;
		g = grandparent(n);
		g->color = RED;
		insert_case1(g, tree);
	}
	else {
		insert_case4(n, tree);
	}
}

void insert_case4(rb_node* n, rb_tree* tree) {
	rb_node* g = grandparent(n);
	
	if ((n == n->parent->right) && (n->parent == g->left)) {
		rotate_left(n->parent, tree);
		n = n->left;
	}
	else if ((n == n->parent->left) && (n->parent == g->right)) {
		rotate_right(n->parent, tree);
		n = n->right;
	}
	insert_case5(n, tree);
}

void insert_case5(rb_node* n, rb_tree* tree) {
	rb_node* g = grandparent(n);
	
	n->parent->color = BLACK;
	g->color = RED;
	if ((n == n->parent->left) && (n->parent == g->left)) {
		rotate_right(g, tree);
	} else if ((n == n->parent->right) && (n->parent == g->right)) {
		rotate_left(g, tree);
	}
}

void delete_case1(rb_node* n, rb_tree* tree) {
	if (n->parent != NULL)
		delete_case2(n, tree);
}

void delete_case2(rb_node* n, rb_tree* tree) {
	rb_node* s = sibling(n);
	if (s->color == RED) {
		n->parent->color = RED;
		s->color = BLACK;
		if (n == n->parent->left)
			rotate_left(n->parent, tree);
		else
			rotate_right(n->parent, tree);
	} 
	delete_case3(n, tree);
}

void delete_case3(rb_node* n, rb_tree* tree) {
	rb_node* s = sibling(n);

	if ((n->parent->color == BLACK) &&
	(s->color == BLACK) &&
	(s->left->color == BLACK) &&
	(s->right->color == BLACK)) {
		s->color = RED;
		delete_case1(n->parent, tree);
	} else {
		delete_case4(n, tree);
	}
}

void delete_case4(rb_node* n, rb_tree* tree) {
	rb_node* s = sibling(n);

	if ((n->parent->color == RED) &&
	(s->color == BLACK) &&
	(s->left->color == BLACK) &&
	(s->right->color == BLACK)) {
		s->color = RED;
		n->parent->color = BLACK;
	} else {
		delete_case5(n, tree);
	}
}

void delete_case5(rb_node* n, rb_tree* tree) {
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
			rotate_right(s, tree);
		} else if ((n == n->parent->right) &&
		(s->left->color == BLACK) &&
		(s->right->color == RED)) {/* this last test is trivial too due to cases 2-4. */
			s->color = RED;
			s->right->color = BLACK;
			rotate_left(s, tree);
		}
	}
	delete_case6(n, tree);
}

void delete_case6(rb_node* n, rb_tree* tree) {
	rb_node* s = sibling(n);

	s->color = n->parent->color;
	n->parent->color = BLACK;

	if (n == n->parent->left) {
		s->right->color = BLACK;
		rotate_left(n->parent, tree);
	} else {
		s->left->color = BLACK;
		rotate_right(n->parent, tree);
	}
}


void is_correct(rb_tree* tree) {
	if(!tree->root)
		return;
	assert(tree->nil->parent == NULL);
	assert(tree->nil->left == NULL);
	assert(tree->nil->right == NULL);
	rb_node* n = tree->root;
	assert(n->left);
	assert(n->right);
	if(!is_leaf(n->left, tree)) {
		test_down(n->left, tree);
		assert(n->left != n->right);
	}
	if(!is_leaf(n->right, tree)) {
		test_down(n->right, tree);
		assert(n->left != n->right);
	}
}

void test_down(rb_node* n, rb_tree* tree) {
	assert(n->parent);
	assert(n == n->parent->left || n == n->parent->right);
	assert(n->left);
	assert(n->right);
	if(!is_leaf(n->left, tree)) {
		assert(n->key >= n->left->key);
		test_down(n->left, tree);
		assert(n->left != n->right);
	}
	if(!is_leaf(n->right, tree)) {
		assert(n->key <= n->right->key);
		test_down(n->right, tree);
		assert(n->left != n->right);
	}
}