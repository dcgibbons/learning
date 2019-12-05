/**
 * PROGRAMMER:          David C. Gibbons
 * COURSE:              CSCI 3333.01
 * DATE:                December 3, 2008
 * ASSIGNMENT:          Homework # 5
 * ENVIRONMENT:         Any ANSI C++ capable environment
 * FILES INCLUDED:      dcg_homework5.cpp, tree.h
 * PURPOSE:             1. Create a balanced Binary Search Tree (BST)
 *                         The first part of your program builds a BST of 
 *                         your friend’s information with ID as unique key 
 *                         values.  Collect each friend’s last name, first
 *                         name, and phone numbers.
 *                      2. Print the entire BST displaying the key. Make 
 *                         sure that key values are displayed in ascending 
 *                         order.
 *                      3. Allow user to search for a friend’s information 
 *                         using the key values and display the information
 *                         to the us
 */

#include <cctype>
#include <iomanip>
#include <iostream>
#include <string>

#include "tree.h"

using namespace std;

class Friend : public ComparableKey
{
  public:
    /**
     * PURPOSE:             Constructs a new Friend object.
     * PARAMETERS:          key - the unique key identifier of the friend
     *                      lastName - the last name of the friend
     *                      firstName - the first name of the friend
     *                      phone - the phone # of the friend
     */
    Friend(const string& key,
           const string& lastName, 
           const string& firstName, 
           const string& phone)
      : m_key(key), 
        m_lastName(lastName), 
        m_firstName(firstName), 
        m_phone(phone)
    {
      // NO-OP
    }

    /**
     * PURPOSE:             Destructs a Friend object.
     */
    virtual ~Friend()
    {
      // NO-OP
    }

    /**
     * PURPOSE:             Retrieves the comparable key value for this
     *                      friend (see ComparableKey class).
     */
    virtual const string& key() const
    {
      return m_key;
    }

    /**
     * PURPOSE:             Displays a Friend object on an output stream.
     * PARAMETERS:          out - the output stream on which to display
     *                      f - the friend to display
     */
    friend ostream& operator<<(ostream& out, const Friend& f)
    {
      out << "Friend("
          << "m_key=" << f.m_key
          << ",m_lastName=" << f.m_lastName
          << ",m_firstName=" << f.m_firstName
          << ",m_phone=" << f.m_phone
          << ")";
      return out;
    }

  private:
    const string m_key;
    const string m_lastName;
    const string m_firstName;
    const string m_phone;
};

/**
 * PURPOSE:             Retrieves a new Friend object based upon user
 *                      input.
 * RETURNS:             a new Friend object pointer
 */
Friend* getFriend()
{
  cout << "Friend ID: " << flush;
  string id;
  getline(cin, id);

  cout << "Last Name: " << flush;
  string lastName;
  getline(cin, lastName);

  cout << "First Name: " << flush;
  string firstName;
  getline(cin, firstName);

  cout << "Phone #: " << flush;
  string phone;
  getline(cin, phone);

  return new Friend(id, lastName, firstName, phone);
}

/**
 * PURPOSE:             Retrieves friend information from the user and adds
 *                      them, as Friend object pointers, to the provided
 *                      binary search tree.
 * PARAMETERS:          tree - the tree that will receive new friends
 */
void getFriends(BinarySearchTree& tree)
{

  while (true)
  {
    Friend* f = getFriend();
    tree.insert(f);

    cout << endl << "Add another friend? [Y/n] " << flush;
    string answer;
    getline(cin, answer);
    if (answer.length() > 0 && tolower(answer[0]) == 'n')
    {
      break;
    }
  }
}

/**
 * PURPOSE:             Allows the user to search for their friends
 * PARAMETERS:          tree - the binary search tree that will be searched
 */
void findFriends(const BinarySearchTree& tree)
{
  cout << "Find Friends" << endl << endl;
  
  while (true)
  {
    cout << "Friend ID: " << flush;
    string friendId;
    getline(cin, friendId);
    
    const TreeNode* node = tree.find(friendId);
    if (node == 0)
    {
      cout << "Friend not found!" << endl;
    }
    else
    {
      const Friend* f = dynamic_cast<const Friend*>(node->element());
      cout << "Friend found!" << endl
           << (*f) << endl;
    }

    cout << endl << "Find another friend? [Y/n] " << flush;
    string answer;
    getline(cin, answer);
    if (answer.length() > 0 && tolower(answer[0]) == 'n')
    {
      break;
    }
  }
}

/**
 * PURPOSE:             Main program entry point.
 */
int main()
{
   cout << "David C. Gibbons" << endl
        << "Homework Assignment # 5" << endl
        << "CSCI 3333 - Data Structures" << endl
        << "Fall 2008" << endl
        << "Bindra Shrestha" << endl
        << setfill('=') << setw(38) << " "
        << endl << endl;

  // create a binary search tree and get friends from the user that will
  // be added to it
  BinarySearchTree tree;
  getFriends(tree);

  // do an in-order traversal of the tree to display its contents after
  // all friends have been added
  cout << setfill('=') << setw(38) << " " << endl << endl;
  cout << tree << endl;
  cout << setfill('=') << setw(38) << " " << endl << endl;

  // allow the user to search for friends
  findFriends(tree);
}

