void balanceR(link& h)
  {
    if ((h == 0) || (h->N == 1)) return;
    partR(h, h->N/2);
    balanceR(h->l);
    balanceR(h->r);
  }

link joinLR(link a, link b)
  { 
    if (a == 0) return b;
    if (b == 0) return a;
    if (rand()/(RAND_MAX/(a->N+b->N)+1) < a->N)
         { a->r = joinLR(a->r, b); return a; }
    else { b->l = joinLR(a, b->l); return b; }
  }

private:
  void insertR(link& h, Item x)
    { if (h == 0) { h = new node(x); return; }
      if (rand() < RAND_MAX/(h->N+1))
        { insertT(h, x); return; }
      if (x.key() < h->item.key()) 
           insertR(h->l, x);
      else insertR(h->r, x);
      h->N++; 
    }

  link joinR(link a, link b)
    { 
      if (a == 0) return b;
      if (b == 0) return a;
      insertR(b, a->item); 
      b->l = joinR(a->l, b->l); 
      b->r = joinR(a->r, b->r); 
      delete a; fixN(b); return b;
    }

  void splay(link& h, Item x)
    { 
      if (h == 0) 
        { h = new node(x, 0, 0, 1); return; }
      if (x.key() < h->item.key())
        { link& hl = h->l; int N = h->N;
          if (hl == 0) 
            { h = new node(x, 0, h, N+1); return; }
          if (x.key() <  hl->item.key()) 
               { splay(hl->l, x); rotR(h); }
          else { splay(hl->r, x); rotL(hl); }
          rotR(h);
        }
      else
        { link &hr = h->r; int N = h->N;
          if (hr == 0) 
            { h = new node(x, h, 0, N+1); return; }
          if (hr->item.key() < x.key()) 
               { splay(hr->r, x); rotL(h); }
          else { splay(hr->l, x); rotR(hr); }
          rotL(h);
        }
    }

  int red(link x) 
    { if (x == 0) return 0; return x->red; }
  void RBinsert(link& h, Item x, int sw)
    { 
      if (h == 0) { h = new node(x); return; }
      if (red(h->l) && red(h->r)) 
      { h->red = 1; h->l->red = 0; h->r->red = 0; }
      if (x.key() < h->item.key())
        {
          RBinsert(h->l, x, 0); 
          if (red(h) && red(h->l) && sw) rotR(h); 
          if (red(h->l) && red(h->l->l)) 
            { rotR(h); h->red = 0; h->r->red = 1; }
        }
      else
        { 
          RBinsert(h->r, x, 1); 
          if (red(h) && red(h->r) && !sw) rotL(h); 
          if (red(h->r) && red(h->r->r)) 
            { rotL(h); h->red = 0; h->l->red = 1; }
        }
    }

  Item searchR(link t, Key v, int k)
    { if (t == 0) return nullItem;
      if (v == t->item.key()) return t->item;
      link x = t->next[k];
      if ((x == 0) || (v < x->item.key())) 
        { 
          if (k == 0) return nullItem;
          return searchR(t, v, k-1); 
        }
      return searchR(x, v, k);
    }

  struct node 
    { Item item; node **next; int sz; 
      node(Item x, int k)
        { item = x; sz = k; next = new node*[k]; 
          for (int i = 0; i < k; i++) next[i] = 0; } 
    }; 
  typedef node *link;
  link head;
  Item nullItem;
  int lgN;

  int randX()
    { int i, j, t = rand();
      for (i = 1, j = 2; i < lgNmax; i++, j += j)
        if (t > RAND_MAX/j) break;
      if (i > lgN) lgN = i;
      return i;
    }

  void insertR(link t, link x, int k)
    { Key v = x->item.key(); link tk = t->next[k];
      if ((tk == 0) || (v < tk->item.key())) 
        { 
          if (k < x->sz)
            { x->next[k] = tk; t->next[k] = x; }
          if (k == 0) return;
          insertR(t, x, k-1); return;
        }
      insertR(tk, x, k);
    }

  void removeR(link t, Key v, int k)
    { link x = t->next[k];
      if (!(x->item.key() < v)) 
        { 
          if (v == x->item.key())
            { t->next[k] = x->next[k]; }
          if (k == 0) { delete x; return; }
          removeR(t, v, k-1); return;
        }
      removeR(t->next[k], v, k);
    }

public:
  void insert(Item x)
    { insertR(head, x); }
  void join(ST<Item, Key>& b)
    { int N = head->N;
      if (rand()/(RAND_MAX/(N+b.head->N)+1) < N)
           head = joinR(head, b.head);
      else head = joinR(b.head, head); }
  void insert(Item item)
    { splay(head, item); }
  void insert(Item x)
    { RBinsert(head, x, 0); head->red = 0; }
  Item search(Key v)
    { return searchR(head, v, lgN); }
  ST(int)
    { head = new node(nullItem, lgNmax); lgN = 0; }
  void insert(Item v)
    { insertR(head, new node(v, randX()), lgN); }
  void remove(Item x)
    { removeR(head, x.key(), lgN); }

