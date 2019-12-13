#include <iostream>
#include <string>
#include <queue>

using namespace std;

struct node {
  string data;
  node* parent;
  node* left;
  node* right;
};
  
void traverse_inorder(node* tree, int level, void visit(node*, int level)) {
  if (tree != 0) {
    traverse_inorder(tree->left, level + 1, visit);
    visit(tree, level);
    traverse_inorder(tree->right, level + 1, visit);
  }
}

void traverse_preorder(node* tree, int level, void visit(node*, int level)) {
  if (tree != 0) {
    visit(tree, level);
    traverse_preorder(tree->left, level + 1, visit);
    traverse_preorder(tree->right, level + 1, visit);
  }
}

void traverse_postorder(node* tree, int level, void visit(node*, int level)) {
  if (tree != 0) {
    traverse_postorder(tree->left, level + 1, visit);
    traverse_postorder(tree->right, level + 1, visit);
    visit(tree, level);
  }
}

void insert(node* root, string data) {
  if (root->data <= data) {
    if (root->left == 0) {
      root->left = new node;
      root->left->left = 0;
      root->left->parent = root;
      root->left->data = data;
      root->left->left = 0;
      root->left->right = 0;
    } else {
      insert(root->left, data);
    }
  } else {
    if (root->right == 0) {
      root->right = new node;
      root->right->parent = root;
      root->right->data = data;
      root->right->left = 0;
      root->right->right = 0;
    } else {
      insert(root->right, data);
    }
  }
}

void traverse_levelorder(node* tree, int level, void visit(node* tree, int level)) {
  queue<node*> q;
  q.push(tree);
  while (!q.empty()) {
    tree = q.front();
    q.pop();
    visit(tree, level);
    if (tree->left != 0) q.push(tree->left);
    if (tree->right != 0) q.push(tree->right);
  }
}

void print_node(node* tree, int level) {
  cout << "level=" << level 
       << " node->data=" << tree->data 
       << " parent=" << tree->parent
       << " left=" << tree->left 
       << " right=" << tree->right 
       << endl;
}

node* scan(node* tree) {
  while (tree->parent != 0) {
    // if this is the left-side of our parent node, then that's the one!
    if (tree == tree->parent->left) {
      return tree->parent;
    } else {
      tree = tree->parent;
    }
  }
  return 0;
}

// finds the leftmost node of the given subtree
node* leftmost(node* tree) {
  while (tree->left != 0) {
    tree = tree->left;
  }
  return tree;
}

// assuming the leftmost side of the given tree has already been
// traversed, find the next node to traverse
node* next(node* tree) {
  if (tree->right == 0) {
    return scan(tree);
  } else {
    return leftmost(tree->right);
  }
}

void nonrecursive_traverse(node* tree, int level, void visit(node* tree, int level)) {
  for (tree = leftmost(tree); tree != 0; tree = next(tree)) {
    visit(tree, level);
  }
}


int main() {
  node* root = new node;
  root->data = "hello";

  insert(root, "alpha");
  insert(root, "beta");
  insert(root, "delta");
  insert(root, "omega");
  insert(root, "phi");
  //print(root);

  cout << "preorder:" << endl;
  traverse_preorder(root, 0, print_node);
  cout << endl;

  cout << "inorder:" << endl;
  traverse_inorder(root, 0, print_node);
  cout << endl;

  cout << "postorder:" << endl;
  traverse_postorder(root, 0, print_node);
  cout << endl;

  cout << "level order:" << endl;
  traverse_levelorder(root, 0, print_node);
  cout << endl;

  cout << "non-recursive traverse:" << endl;
  nonrecursive_traverse(root, 0, print_node);

  /*
  node* root2 = new node();
  root2->data = "a";
  insert(root2, "b");
  insert(root2, "c");
  insert(root2, "Z");
  */

  return 0;
}
