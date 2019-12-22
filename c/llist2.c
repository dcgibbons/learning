#include <stdio.h>
#include <string.h>
#include <stdlib.h>

struct list_node {
  char* s;
  struct list_node* next;
};

struct list_node* add_node(struct list_node* head, char* s) {
  if (head == NULL) {
    head = malloc(sizeof(struct list_node));
    head->s = s;
    head->next = NULL;
  } else {
    struct list_node* p = head;
    while (p->next != NULL) {
      p = p->next;
    }

    struct list_node* q = malloc(sizeof(struct list_node));
    q->s = s;
    q->next = NULL;
    p->next = q;
  }

  return head;
}

void print_list(struct list_node* head) {
  int n = 0;
  while (head != NULL) {
    printf("n=%d s=%s next=%p\n", n++, head->s, head->next);
    head = head->next;
  }
}

struct list_node* reverse_list(struct list_node* head) {
  struct list_node* p;
  struct list_node* q;
  struct list_node* r;

  p = head;
  q = head->next;
  p->next = NULL;
  while (q != NULL) {
    r = q->next;
    q->next = p;
    q = r;
  }
  head = p;
  return head;
}

int main() {
  struct list_node* head = NULL;

  head = add_node(head, "node1");
  head = add_node(head, "node2");
  head = add_node(head, "node3");
  head = add_node(head, "node4");
  print_list(head);

  puts("\nreversing...\n");
  head = reverse_list(head);
  print_list(head);

  return 0;
}
