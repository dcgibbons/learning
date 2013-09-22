#include <iostream>
#include <string>

using namespace std;

template<class T> class List {
  public:
    List() : head(0), tail(0) {
      cout << "constructing a List object" << endl;
    }

    ~List() {
      cout << "destructing a List object" << endl;

      // clean up any nodes currently on the list
      while (head) {
        ListNode* p = head;
        head = head->next;
        delete p;
      }

      head = tail = 0;
    }

    friend ostream& operator<<(ostream& out, const List& list) {
      out << "List: ";

      if (list.head) {
        out << endl;

        int nnode = 0;
        ListNode* p = list.head;
        while (p) {
          out << "(" << nnode << ") ";
          if (p == list.head) {
            cout << "(H) ";
          } 
          if (p == list.tail) {
            cout << "(T) ";
          }
          out << p->data << endl;
          p = p->next;
          nnode++;
        }
      } else {
        out << "(empty)" << endl;
      }

      return out;
    }

    /*
     * adds a single element to the head of the list
     */
    void add(T data) {
      ListNode* new_node = new ListNode();
      new_node->next = head;
      new_node->data = data;
      head = new_node;
      if (!tail) {
        tail = new_node;
      }
    }

    /*
     * removes a single element from the list
     */
    bool remove(const T& data) {
      bool removed = false;
      // make sure there are elements on the list
      if (head) {
        // check if the head is the element we're trying to remove
        if (data == head->data) {
          ListNode* p = head;
          head = head->next;
          delete p;
          removed = true;
          if (head == 0) {
            tail = 0;
          }
        } else {
          for (ListNode* curr = head; curr != 0; curr = curr->next) {
            if (curr->next != 0 && curr->next->data == data) {
              ListNode* p = curr->next;    // save our found node for later deletion
              curr->next = p->next;        // move the next pointer to the next node in the list
              delete p;                 // and now remove the linked list node
              removed = true;

              // if the next pointer is null, we need to update the tail!
              if (curr->next == 0) {
                tail = curr;
              }

              break;
            }
          }
        }
      }
      return removed;
    }

    /*
     * reverses the list, in place
     */
    void reverse() {
      if (head) {
        ListNode* p;
        ListNode* curr;
        ListNode* next;
        for (p = 0, curr = head; curr != 0; p = curr, curr = next) {
          next = curr->next;
          curr->next = p;
          if (p == 0) {
            tail = curr;
          }
        }
        head = p;
      }
    }

    bool findMthToLastElement(T& data, int m) {
      ListNode* behind = 0;
      ListNode* curr = head;
      for (int i = 0; i < m; i++) {
        if (curr) {
          curr = curr->next;
        } else {
          return false;
        }
      }
      
      behind = head;
      while (curr->next != 0) {
        behind = behind->next;
        curr = curr->next;
      }

      data = behind->data;
      return true;
    }

  private:
    typedef struct list_node {
      struct list_node* next;
      T data;
    } ListNode;

    ListNode* head;
    ListNode* tail;
};

int main() {
  List<string> l1;
  l1.add("foobar");
  l1.add("is tasty");
  l1.add("yum yum");
  cout << "list after creation:" << endl << l1 << endl;

  l1.remove("is tasty");
  cout << "list after remove:" << endl << l1 << endl;

  l1.remove("foobar");
  cout << "list after remove:" << endl << l1 << endl;

  l1.remove("yum yum");
  cout << "list after remove:" << endl << l1 << endl;

  l1.remove("uhoh");

  List<string>* l2 = new List<string>();
  l2->add("hello");
  l2->add("there");
  l2->add("world");
  cout << "list before reverse:" << endl << *l2 << endl;
  l2->reverse();
  cout << "list after reverse:" << endl << *l2 << endl;

  l2->add("foo");
  l2->add("bar");
  cout << "list before finds: " << endl << *l2 << endl;

  string data;
  l2->findMthToLastElement(data, 3);
  cout << "3rd from last element: " << data << endl;

}
