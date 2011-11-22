#define BLACK 1
#define RED 0

typedef struct rb_node rb_node;
 typedef struct rb_tree rb_tree;
 
 struct rb_node {
	uint32_t key;
	int color;
	rb_data *parent;
	rb_node *right;
	rb_node *left;
	void * data;
};

struct rb_tree {
	rb_node * root;
	uint32_t n;
};