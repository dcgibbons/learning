#include <iostream>
#include <string>

using namespace std;

typedef struct list_node {
  struct list_node* next;
  string data;
  list_node() : next(0) {}
} ListNode;

ListNode* addHead(ListNode* head, string data) {
  ListNode* new_node = new ListNode();
  new_node->next = head;
  new_node->data = data;
  return new_node;
}

bool is_cyclic(ListNode* head) {
  bool cyclic = false;

  ListNode* fast = head;
  ListNode* slow = head;
  while (slow && fast && slow->next && fast->next && fast->next->next) {
    slow = slow->next;
    fast = fast->next->next;
    if (fast == slow) {
      cyclic = true;
      break;
    }
  }
  return cyclic;
}

int main() {
  ListNode* head = 0;
  head = addHead(head, "foo");
  head = addHead(head, "bar");
  ListNode* temp = head;
  head = addHead(head, "hello");
  head = addHead(head, "world");

  // find the tail and hack the next pointer to somewhere in the middle of the list
  ListNode* curr;
  for (curr = head; curr->next != 0; curr = curr->next) {
    // NO-OP
  }
  curr->next = temp;

  cout << "list is cyclic? " << is_cyclic(head) << endl;
}
