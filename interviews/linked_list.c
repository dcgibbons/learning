/*
 * linked_list.c
 * simple linked list exercises
 */

#include <stdio.h>
#include <stdlib.h>

struct _list_node;
typedef struct _list_node* list_node_t;
struct _list_node {
    int value;
    list_node_t next;
};

void list_insert(list_node_t* list, int value)
{
    list_node_t new_node = malloc(sizeof(struct _list_node));
    if (!new_node) abort();
    new_node->value = value;
    new_node->next = *list;
    *list = new_node;
}

void list_print(list_node_t list)
{
    for (list_node_t curr = list; curr != NULL; curr = curr->next) {
        printf("value:%d next:%p\n", curr->value, curr->next);
    }
}

void list_reverse(list_node_t* list)
{
    list_node_t prev = NULL;
    list_node_t curr = *list;
    while (curr != NULL) {
        list_node_t tmp = curr->next;
        curr->next = prev;
        prev = curr;
        curr = tmp;
    }
    *list = prev;
}


int main(void)
{
    list_node_t list = NULL;
    list_insert(&list, 1);
    list_insert(&list, 5);
    list_insert(&list, 37);
    list_print(list);

    printf("\nreversed:\n");
    list_reverse(&list);
    list_print(list);

    return EXIT_SUCCESS;
}

