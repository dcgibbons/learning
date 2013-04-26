/**
 * PROGRAMMER:          David C. Gibbons
 * COURSE:              CSCI 3333.01
 * DATE:                November 2, 2008
 * ASSIGNMENT:          Homework # 3
 * ENVIRONMENT:         Any ANSI C++ capable environment
 * FILES INCLUDED:      dcg_homework3.cpp
 * PURPOSE:             Perform basic link list operations.
 * ALGORITHM:           The following three basic list operations are
 *                      exercised (taken from the homework assignment doc):
 * 
 *                      Create list
 *                        The fist part of your program builds a list of 
 *                        your friend’s information such as last name, 
 *                        first name, and telephone numbers.  Use a linked
 *                        list implementation.
 *
 *                      Retrieve list
 *                        The second part of your program displays the list
 *                        of information that you collected in the first 
 *                        part.
 *
 *                      Delete
 *                        The third part of your program deletes a record 
 *                        from the list and displays the remaining list.
 */

#include <exception>
#include <stdexcept>
#include <iostream>
#include <iomanip>
#include <string>

using namespace std;

/**
 * CLASS:               ListNode
 * PURPOSE:             Provides a wrapper around a template class so it
 *                      may participate in a list data structure.
 */
template <class T> class ListNode
{
  public:
    /*
     * METHOD:          ListNode (constructor>
     * PURPOSE:         Constructs a new ListNode object instance.
     * INPUT:           data -- the data that will be stored in this node
     * PRECONDITIONS:   None
     * OUTPUT:          None
     * POSTCONDITIONS:  The ListNode will be initialized and considered
     *                  detached from any list, i.e. its previous and
     *                  next pointers will be set to null.
     */
    ListNode(const T& data) : m_prev(0), m_next(0), m_data(data)
    {
    }

    /*
     * METHOD:          operator<<
     * PURPOSE:         outputs a ListNode onto the provided output stream
     *                  in a human readable format
     * PRECONDITIONS:   None
     * INPUT:           out -- the ostream that will receive the output
     *                  node -- the ListNode to be output
     * OUTPUT:          out -- the ostream passed in, for continuation
     */
    friend ostream& operator<<(ostream& out, const ListNode<T>& node)
    {
      out << "("
          << "this=" << &node
          << " prev=" << node.m_prev 
          << " next=" << node.m_next
          << " data=" << node.m_data 
          << ")";
      return out;
    }

    ListNode<T>* m_prev;
    ListNode<T>* m_next;
    const T m_data;
};

/**
 * CLASS:               LinkedList
 * PURPOSE:             Provides a List abstract data type implemented 
 *                      using a linked list data structure.
 */
template <class T> class LinkedList
{
  public:
    /*
     * METHOD:          Linkedist (constructor>
     * PURPOSE:         Constructs a new LinkedList object instance.
     * INPUT:           None
     * PRECONDITIONS:   None
     * OUTPUT:          None
     * POSTCONDITIONS:  The LinkedList will be initialized and ready to
     *                  store list nodes, i.e. the head will be null.
     */
    LinkedList() : m_head(0)
    {
    }

    /*
     * METHOD:          insertLast
     * PURPOSE:         Insert a node into the last position of the list.
     * INPUT:           node -- a pointer to the list node to insert
     * PRECONDITIONS:   None
     * OUTPUT:          None
     * POSTCONDITIONS:  The node pointer will now be owned by this list
     *                  instance and its internal previous and next
     *                  pointers will be modified.
     * ALGORITHM:       The typical, and worst-case, run-time for this 
     *                  algorithm is O(N) where N is the number of elements
     *                  already in the list.
     *
     *                  The following pseudo-code describes the algorithm
     *                  used for this implementation:
     *                  
     *                  if head is nil then
     *                    head <- node
     *                    node->prev <- nil
     *                    node->next <- nil
     *                  else
     *                    curr <- head
     *                    while curr is not nil do
     *                      curr <- curr->next
     *                    end while
     *                    curr->next <- node
     *                    node->prev <- curr
     *                    node->next <- nil
     *                  end if
     */
    void insertLast(ListNode<T>* node)
    {
      // if the list is empty, just set the head to our new node, otherwise
      // it needs to be appended to the end of the list
      if (m_head == 0)
      {
        m_head = node;
        node->m_prev = 0;
        node->m_next = 0;
      }
      else
      {
        ListNode<T>* curr = m_head;
        while (curr->m_next != 0)
        {
          curr = curr->m_next;
        }

        curr->m_next = node;
        node->m_prev = curr;
        node->m_next = 0;
      }
    }

    /*
     * METHOD:          findNode
     * PURPOSE:         Searches the list for a node that contains the 
     *                  provided data.
     * INPUT:           data -- the data to search the list for
     * PRECONDITIONS:   None
     * OUTPUT:          A valid ListNode pointer, or 0 if not found
     * ALGORITHM:       The worst-case run-time for this algorithm is 
     *                  O(N) where N is the number of elements in the list.
     *
     *                  The following pseudo-code describes the algorithm
     *                  used for this implementation:
     *
     *                  found <- false
     *                  curr <- head
     *                  while not found and curr is not nil do
     *                    if curr->data == data then
     *                      found <- true
     *                    else
     *                      curr <- curr->next
     *                    end if
     *                  end while
     *
     *                  if found then
     *                    return curr
     *                  else
     *                    return nil
     *                  end if 
     */
    ListNode<T>* findNode(const T& data)
    {
      bool found = false;
      ListNode<T>* curr = m_head;

      while (!found && curr != 0)
      {
        if (curr->m_data == data)
        {
          found = true;
        }
        else
        {
          curr = curr->m_next;
        }
      }

      return (found) ? curr : 0;
    }

    /*
     * METHOD:          deleteNode
     * PURPOSE:         Searches the list for a node that contains the 
     *                  provided data and deletes that node from the list.
     * INPUT:           data -- the data to search the list for
     * PRECONDITIONS:   None
     * OUTPUT:          true if the node was found and deleted, otherwise
     *                  false
     * ALGORITHM:       The worst-case run-time for this algorithm is 
     *                  O(N) where N is the number of elements in the list.
     *
     *                  The following pseudo-code describes the algorithm
     *                  used for this implementation:
     *
     *                  node <- findNode(data)
     *                  found <- node is not nil
     *                  if found then
     *                    if node == head then
     *                      head <- node->next
     *                    else
     *                      node->prev->next <- node->next 
     *                    end if
     *                    if node->next is not nil then
     *                      node->next->prev <- node->prev
     *                    endif
     *                    node->prev <- nil
     *                    node->next <- nil
     *                  end if
     *                  return found
     */
    bool deleteNode(const T& data)
    {
      ListNode<T>* node = findNode(data);
      const bool found = (node != 0);

      if (found)
      {
        if (node == m_head)
        {
          m_head = node->m_next;
        }
        else
        {
          node->m_prev->m_next = node->m_next;
        }

        if (node->m_next != 0)
        {
          node->m_next->m_prev = node->m_prev;
        }

        node->m_prev = 0;
        node->m_next = 0;
      }

      return found;
    }

    /*
     * METHOD:          operator<<
     * PURPOSE:         outputs a LinkedList onto the provided output stream
     *                  in a human readable format
     * PRECONDITIONS:   None
     * INPUT:           out -- the ostream that will receive the output
     *                  node -- the LinkedList to be output
     * OUTPUT:          out -- the ostream passed in, for continuation
     * ALGORITHM:       The run-time for this algorithm is a constant O(N),
     *                  where N is the number of nodes in the list.
     *
     *                  The following pseudo-code describes the algorithm
     *                  used for this implementation:
     *
     *                  nodeNum <- 0
     *                  curr <- head
     *                  while curr is not nil do
     *                    nodeNum <- nodeNum + 1
     *                    print nodeNum, curr
     *                    curr <- curr->next
     *                  end while
     */
    friend ostream& operator<<(ostream& out, const LinkedList& list)
    {
      out << "{" << endl;

      int nodeCount = 0;
      const ListNode<T>* curr = list.m_head;
      while (curr)
      {
        nodeCount++;
        out << "  [" << nodeCount << "]=" << *curr << endl;
        curr = curr->m_next;
      }

      out << "}";
      return out;
    }

  private:
    ListNode<T>* m_head;
};

/*
 * FUNCTION:            getData
 * PURPOSE:             Retrieves names from the user and stores it in the
 *                      provided list.
 * INPUT:               list -- the linked list to add names to
 * PRECONDITIONS:       None
 * OUTPUT:              None
 * POSTCONDITIONS:      All provided data will be added to the list.
 */
void getData(LinkedList<string>& list)
{
  cout << "Please the names of some of your friends at the" << endl
       << "following prompts. When finished, enter a blank" << endl
       << "line and a list of your friend's information will" << endl
       << "be built." << endl
       << endl;

  while (true)
  {
    string line;
    cout << "> " << flush;
    getline(cin, line);

    if (line.length() > 0)
    {
      list.insertLast(new ListNode<string>(line));
    }
    else
    {
      break;
    }
  }
}

/*
 * FUNCTION:            deleteData
 * PURPOSE:             Deletes a single name from the provided list, based
 *                      upon a name the user provides.
 * INPUT:               list -- the linked list to add names to
 * PRECONDITIONS:       None
 * OUTPUT:              None
 * POSTCONDITIONS:      The provided name will be deleted from the list, if
 *                      it is found.
 */
void deleteData(LinkedList<string>& list)
{
  cout << "Please provide the name of a friend you wish to delete " << endl
       << "from the list." << endl
       << endl;

  string line;
  cout << "> " << flush;
  getline(cin, line);

  if (line.length() > 0)
  {
    bool deleted = list.deleteNode(line);
    if (!deleted)
    {
      cout << line << " was not found in the list." << endl;
    }
  }
}

/*
 * FUNCTION:            main
 * PURPOSE:             main program entry point
 */
int main()
{
  LinkedList<string> theList;

  // operation #1 - get user input and add it to the list
  getData(theList);

  // operation #2 - display the data in the list
  cout << endl << "Here is the list with your data:" << endl
       << theList << endl
       << endl;

  // operation #3 - delete an item from the list and display the new list
  deleteData(theList);
  cout << endl << "Here is the list with your data:" << endl
       << theList << endl
       << endl;

  // all done!
  return 0;
}

