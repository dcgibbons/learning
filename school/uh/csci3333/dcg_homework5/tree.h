/**
 * PURPOSE:             Provide a Binary Search Tree data structure.
 * ALGORITHM:           This implementation is implemented using an
 *                      internal linked-list. Objects to be inserted into
 *                      the tree must subclass the ComparableKey base class
 *                      so that a unique key value can be extracted and
 *                      used as the basis for searching.
 */

class ComparableKey
{
  public:
    /**
     * PURPOSE:             Destructs a ComparableKey object
     */
    virtual ~ComparableKey()
    {
      // NO-OP
    }

    /**
     * PURPOSE:             Retrieves the unique key value to use for this
     *                      object.
     * RETURNS:             the unique key value
     */
    virtual const std::string& key() const = 0;
};

class TreeNode
{
  public:
    /**
     * PURPOSE:             Constructs a new TreeNode object
     * PARAMETERS:          element - the data element owned by this node
     *                      parent - the parent node of this new node
     *                      left - the left child of this node
     *                      right - the right child of this node
     */
    TreeNode(const ComparableKey* element, 
             const TreeNode* parent,
             TreeNode* left = 0,
             TreeNode* right = 0)
      : m_element(element), m_parent(parent), m_left(left), m_right(right)
    {
      // NO-OP
    }

    /**
     * PURPOSE:             Retrieves the data element owned by this node
     * RETURNS:             a pointer to the data element
     */
    const ComparableKey* element() const
    {
      return m_element;
    }

    /**
     * PURPOSE:             Retrieves the parent of this node
     * RETURNS:             a pointer to the parent tree node of this node
     */
    const TreeNode* parent() const
    {
      return m_parent;
    }

    /**
     * PURPOSE:             Retrieves the left child of this node
     * RETURNS:             a pointer to the left child of this node
     */
    const TreeNode* left() const
    {
      return m_left;
    }

    /**
     * PURPOSE:             Sets the left child node of this node
     * PARAMETERS:          left - a pointer to the new left child node
     */
    void setLeft(TreeNode* left)
    {
      m_left = left;
    }

    /**
     * PURPOSE:             Retrieves the right child of this node
     * PARAMETERS:          a pointer to the right child of this node
     */
    const TreeNode* right() const
    {
      return m_right;
    }

    /**
     * PURPOSE:             Sets the right child node of this node
     * PARAMETERS:          right - a pointer to the new right child node
     */
    void setRight(TreeNode* right)
    {
      m_right = right;
    }

  private:
    const ComparableKey* m_element;
    const TreeNode* m_parent;
    TreeNode* m_left;
    TreeNode* m_right;
};

class BinarySearchTree
{
  public:
    /**
     * PURPOSE:             Constructs a new binary search tree.
     */
    BinarySearchTree() : m_root(0)
    {
      // NO-OP
    }

    /**
     * PURPOSE:             Finds a node within the tree
     * PARAMETERS:          key - the unique key identifier of the element
     * RETURNS:             a pointer to the tree node containing the
     *                      element or null if it was not found
     * ALGORITHM:           This find algorithm is O(log n) on average and
     *                      O(n) for the worst-case. The pseudo-code for
     *                      algorithm is as follows:
     *
     *                      current = root
     *                      while current is not null do
     *                        if key = current.key then
     *                          return current  
     *                        else if key < current.key then
     *                          result = current.left_child
     *                        else
     *                          result = current.right_child
     *                        end if
     *                      end while
     *                      return null
     */
    const TreeNode* find(const std::string& key) const
    {
      bool found = false;
      const TreeNode* curr = m_root;

      while (!found && curr != 0)
      {
        int n = key.compare(curr->element()->key());
        if (n < 0)
        {
          curr = curr->left();
        }
        else if (n > 0)
        {
          curr = curr->right();
        }
        else
        {
          found = true;
        }
      }

      return found ? curr : 0;
    }

    /**
     * PURPOSE:             Inserts a new node into the tree in an
     *                      appropriate position to maintain sorted order
     * PARAMETERS:          element - the data element to insert
     * ALGORITHM:           This insert algorithm is O(log n) on average
     *                      and O(n) for the worst-case. The pseudo-code
     *                      for the algorithm is as follows:
     *
     *                      current = root
     *                      parent = null
     *
     *                      while current is not null do
     *                        parent = current
     *                        if element.key = current.key then
     *                          return
     *                        else if element.key < current.key then
     *                          current = current.left_child
     *                        else
     *                          current = current.right_child
     *                        end if
     *                      end while
     *
     *                      element.parent = parent
     *                      if parent is null then
     *                        root = element
     *                      else if element.key < parent.key then
     *                        parent.left_child = element
     *                      else
     *                        parent.right_child = element
     *                      end if
     */

    void insert(const ComparableKey* element)
    {
      const TreeNode* curr = m_root;
      const TreeNode* parent = 0;

      while (curr != 0)
      {
        parent = curr;

        int n = element->key().compare(curr->element()->key());
        if (n < 0)
        {
          curr = curr->left();
        }
        else if (n > 0)
        {
          curr = curr->right();
        }
        else
        {
          // ignore this new node since the key already exists
          return; 
        }
      }

      TreeNode* newNode = new TreeNode(element, parent);
      if (parent == 0)
      {
        m_root = newNode;
      }
      else
      {
        int n = element->key().compare(parent->element()->key());
        if (n < 0)
        {
          const_cast<TreeNode*>(parent)->setLeft(newNode);
        }
        else
        {
          const_cast<TreeNode*>(parent)->setRight(newNode);
        }
      }
    }

    /**
     * PURPOSE:             Perform an in-order traversal of the tree and
     *                      display the node data on the specified output
     *                      stream.
     * PARAMETERS:          out - the output stream
     *                      node - the current tree node
     * ALGORITHM:           Uses a recursive algorithm to traverse the
     *                      entire tree in ascending key order.
     *
     *                      if element is not null then
     *                        traverse(element.left)
     *                        print element.key
     *                        traverse(element.right)
     *                      end if
     */
    void printInOrder(std::ostream& out, const TreeNode* node) const
    {
      if (node != 0)
      {
        // traverse the left children
        printInOrder(out, node->left());

        // display the current node's key and pointer data
        out << "node=" << node << " (" << node->element()->key() << ")"
            << " parent=" << node->parent()
            << " left=" << node->left()
            << " right=" << node->right()
            << std::endl;

        // traverse the right children
        printInOrder(out, node->right());
      }
    }

    /**
     * PURPOSE:             Display the binary search tree on the provided
     *                      output stream
     * PARAMETERS:          out - the output stream
     *                      tree - the tree to display
     */
    friend std::ostream& operator<<(std::ostream& out, 
                                    const BinarySearchTree& tree)
    {
      out << "BinarySearchTree:" << std::endl;
      tree.printInOrder(out, tree.m_root);
      out << std::endl;
      return out;
    }

  private:
    TreeNode* m_root;
};

