#include "rbtree.h"


void rotate_right(rb_node *q){
	rb_node * p = n->left;
	rb_node * b = p->right;
	
	p->parent = q->parent;
	
	q->left = b;
	b->parent = q;
	
	p->right = q;
	q->parent = p;
	
	if (q == p->parent->left)
		 p->parent->left = p;
	else
		p->parent->right = p;
}
void rotate_left(rb_node *p){
	rb_node * q = p->right;
	rb_node * b = q->left;
	
	q->parent = p->parent;
	
	p->right = b;
	b->parent = p;
	
	q->left = p;
	p->parent = q;
	
	if (p == q->parent->left)
		q->parent->left = q;
	else
		q->parent->right = q;
	
}
rb_node *sibling(rb_node *n)
{
	if (n == n->parent->left)
		return n->parent->right;
	else
		return n->parent->left;
}
 rb_node *grandparent(rb_node *n)
{
	if ((n != NULL) && (n->parent != NULL))
		return n->parent->parent;
	else
		return NULL;
}
rb_node *uncle(rb_node *n)
{
	rb_node *g = grandparent(n);
	if (g == NULL)
		return NULL; // No grandparent means no uncle
	if (n->parent == g->left)
		return g->right;
	else
		return g->left;
}
void insert_case1(rb_node *n)
{
	if (n->parent == NULL)
		n->color = BLACK;
	else
		insert_case2(n);
}
void insert_case2(rb_node *n)
{
	if (n->parent->color == BLACK)
		return; /* Tree is still valid */
	else
		insert_case3(n);
}
void insert_case3(rb_node *n)
{
	rb_node *u = uncle(n), *g; 

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
void insert_case4(rb_node *n)
{
	rb_node *g = grandparent(n);
 
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
void insert_case5(rb_node *n)
{
        rb_node *g = grandparent(n);
 
        n->parent->color = BLACK;
        g->color = RED;
        if ((n == n->parent->left) && (n->parent == g->left)) {
                rotate_right(g);
        } else if ((n == n->parent->right) && (n->parent == g->right)) {
                rotate_left(g);
        }
}
void delete_one_child(rb_node *n)
{
        /*
         * Precondition: n has at most one non-null child.
         */
        rb_node *child = is_leaf(n->right) ? n->left : n->right;
 
        replace_node(n, child);
        if (n->color == BLACK) {
                if (child->color == RED)
                        child->color = BLACK;
                else
                        delete_case1(child);
        }
        free(n);
}
void delete_case1(rb_node *n)
{
        if (n->parent != NULL)
                delete_case2(n);
}
void delete_case2(rb_node *n)
{
        rb_node *s = sibling(n);
 
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
void delete_case3(rb_node *n)
{
        rb_node *s = sibling(n);
 
        if ((n->parent->color == BLACK) &&
            (s->color == BLACK) &&
            (s->left->color == BLACK) &&
            (s->right->color == BLACK)) {
                s->color = RED;
                delete_case1(n->parent);
        } else
                delete_case4(n);
}
void delete_case4(rb_node *n)
{
        rb_node *s = sibling(n);
 
        if ((n->parent->color == RED) &&
            (s->color == BLACK) &&
            (s->left->color == BLACK) &&
            (s->right->color == BLACK)) {
                s->color = RED;
                n->parent->color = BLACK;
        } else
                delete_case5(n);
}
void delete_case5(rb_node *n)
{
        rb_node *s = sibling(n);
 
        if  (s->color == BLACK) { /* this if statement is trivial, 
due to Case 2 (even though Case two changed the sibling to a sibling's child, 
the sibling's child can't be red, since no red parent can have a red child). */
/* the following statements just force the red to be on the left of the left of the parent, 
   or right of the right, so case six will rotate correctly. */
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
void delete_case6(rb_node *n)
{
        rb_node *s = sibling(n);
 
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
