#include <iostream>

using namespace std;

struct tree_node {
  int key;
  struct tree_node* parent;
  struct tree_node* left;
  struct tree_node* right;
};

struct tree_node* tree_root = 0;

struct tree_node* tree_search(struct tree_node* x, int key)
{
  if (x == 0 || x->key == key)
  {
    return x;
  }
  else if (key < x->key)
  {
    return tree_search(x->left, key);
  }
  else
  {
    return tree_search(x->right, key);
  }
}

struct tree_node* iterative_tree_search(struct tree_node* x, int key)
{
  while (x != 0 && key != x->key)
  {
    if (key < x->key)
    {
      x = x->left;
    }
    else
    {
      x = x->right;
    }
  }
  return x;
}

struct tree_node* tree_minimum(struct tree_node* x)
{
  while (x->left != 0)
  {
    x = x->left;
  }
  return x;
}

struct tree_node* tree_maximum(struct tree_node* x)
{
  while (x->right != 0)
  {
    x = x->right;
  }
  return x;
}

struct tree_node* tree_successor(struct tree_node* x)
{
  if (x->right != 0)
  {
    return tree_minimum(x->right);
  }
  else
  {
    struct tree_node* y = x->parent;
    while (y != 0 && x == y->right)
    {
      x = y;
      y = y->parent;
    }
    return y;
  }
}

struct tree_node* tree_insert(int key)
{
  struct tree_node* y = 0;
  struct tree_node* x = tree_root;
  while (x != 0)
  {
    y = x;
    if (key < x->key)
    {
      x = x->left;
    }
    else
    {
      x = x->right;
    }
  }

  struct tree_node* z = new struct tree_node;
  z->key = key;
  z->parent = y;
  z->left = 0;
  z->right = 0;
  if (y == 0)
  {
    tree_root = z;
  }
  else if (z->key < y->key)
  {
    y->left = z;
  }
  else
  {
    y->right = z;
  }

  return z;
}

void print_node(struct tree_node* x)
{
  cout << "x==" << x 
       << " x->key=" << x->key 
       << " x->parent=" << x->parent 
       << " x->left" << x->left 
       << " x->right" << x->right 
       << endl;
}

int main()
{
  tree_insert(2);
  tree_insert(252);
  tree_insert(401);
  tree_insert(398);
  tree_insert(330);
  tree_insert(344);
  tree_insert(397);
  tree_insert(363);

  struct tree_node* x = tree_search(tree_root, 401);
  print_node(x);

  x = tree_minimum(tree_root);
  cout << "minimum:" << endl;
  print_node(x);

  x = tree_successor(x);
  cout << "successor:" << endl;
  print_node(x);

  x = tree_maximum(tree_root);
  cout << "maximum:" << endl;
  print_node(x);

  return 0;
}

