#include <iostream>
#include <string>

using namespace std;

template<class T> class List {
  public:
    List() : head(0), tail(0) {
      cout << "constructing a List" << endl;
    }

    ~List() {
      cout << "destructing a List" << endl;
    }

    friend ostream& operator<<(ostream& out, const List& list) {
      out << "List: " << endl;
      if (!list.head) {
        out << "(empty)";
      } else {
        ListNode* curr = list.head;
        int i = 0;
        while (curr != 0) {
          out << "(" << i << ") ";
          if (curr == list.head) {
            out << "(H) ";
          }
          if (curr == list.tail) {
            out << "(T) ";
          }
          out << curr->data << endl;
          i++;
          curr = curr->next;
        }
      }
      out << endl;
      return out;
    }

    void addHead(const T& data) {
      ListNode* new_node = new ListNode;
      new_node->data = data;
      new_node->prev = 0;
      new_node->next = head;
      head = new_node;
      if (tail == 0) {
        tail = new_node;
      }
    }

    void addTail(const T& data) {
      ListNode* new_node = new ListNode;
      new_node->data = data;
      new_node->next = 0;
      new_node->prev = tail;
      tail = new_node;
      if (!head) {
        head = new_node;
      }
      if (new_node->prev != 0) {
        new_node->prev->next = new_node;
      }
    }

    void reverse() {
      ListNode* prev;
      ListNode* curr;
      ListNode* next;

      prev = 0;
      curr = head;
      for (prev = 0, curr = head; curr != 0; prev = curr, curr = next) {
        next = curr->next;
        cout << "prev=" << prev << " curr=" << curr << " next=" << next << endl;

        curr->next = prev;
        curr->prev = next;

        // if we're at the start, move the tail to the original head
        if (prev == 0) {
          tail = curr;
        }
      }
      // reset the head pointer to the end of the original list
      head = prev;
    }

  private:
    typedef struct list_node {
      struct list_node* prev;
      struct list_node* next;
      T data;
    } ListNode;

    ListNode* head;
    ListNode* tail;
};

int main() {
  List<string> l1;
  l1.addHead("hello");
  l1.addHead("there");
  l1.addHead("world");
  cout << "list after creation:" << endl << l1 << endl;

  List<string> l2;
  l2.addTail("hello");
  l2.addTail("there");
  l2.addTail("world");
  cout << "list after creation:" << endl << l2 << endl;

  l1.reverse();
  cout << "list l1 after reverse:" << endl << l1 << endl;

  l2.reverse();
  cout << "list l2 after reverse:" << endl << l2 << endl;
}
