#include <stdio.h>
#include <string.h>
#include <stdlib.h>

struct list_node {
  char* s;
  struct list_node* prev;
  struct list_node* next;
};

struct list_node* add_node(struct list_node* head, char* s) {
  struct list_node* tail = malloc(sizeof(struct list_node));
  tail->s = s;
  tail->prev = NULL;
  tail->next = NULL;

  while (head->next != NULL) {
    head = head->next;
  }

  head->next = tail;

  return tail;
}

void print_list(struct list_node* head) {
  do {
    printf("s=%s prev=%p next=%p\n", head->s, head->prev, head->next);
    head = head->next;
  } while (head != NULL);
}

struct list_node* reverse_list(struct list_node* head) {
  if (head != NULL) {
    struct list_node* p;
    struct list_node* q;
    struct list_node* r;

    // save a pointer to the head, save it's next, and then mark it as the future
    // end of list by changing the next pointer to NULL
    p = head;
    q = p->next;
    p->next = NULL;

    while (q != NULL) { // keep going until we reach the end of the original list
      r = q->next;      // save current node's next pointer
      q->next = p;      // change current node's next pointer to the future head
      p = q;            // move current point to the 'end' of the list
      q = r;            // reassign current point to the original next node
    }

    head = p;
  }

  return head;
}

int main() {
  struct list_node* head = malloc(sizeof(struct list_node));
  head->s = "ye olde original head";
  head->prev = NULL;
  head->next = NULL;

  add_node(head, "node2");
  add_node(head, "node3");
  add_node(head, "node4");
  print_list(head);

  puts("\nreversing...\n");
  head = reverse_list(head);
  print_list(head);

  return 0;
}
