#include <iostream>
#include <string>

using namespace std;

template<class T> class List {
  public:
    List() : head(0) {
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
    }

    friend ostream& operator<<(ostream& out, const List& list) {
      out << "List: ";

      if (list.head) {
        out << endl;

        int nnode = 0;
        ListNode* p = list.head;
        while (p) {
          out << "(" << nnode << ") " << p->data << endl;
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
        } else {
          ListNode* p = head;
          // look @ the remaining list, ok to skip the head
          while (p->next) {
            if (p->next->data == data) {
              ListNode* q = p->next;    // save our found node for later deletion
              p->next = q->next;        // move the next pointer to the next node in the list
              delete q;                 // and now remove the linked list node
              removed = true;
              break;
            } else {
              p = p->next;              // move to the next node
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
        }
        head = p;
      }
    }

  private:
    typedef struct list_node {
      struct list_node* next;
      T data;
    } ListNode;

    ListNode* head;
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
  delete l2;
}
