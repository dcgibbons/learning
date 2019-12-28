/*
 * priority_queue.c
 * a simple (min) priority queue
 */

#include <assert.h>
#include <stdlib.h>

#include "priority_queue.h"

void queue_push(queue_t** queue, int priority, void* value)
{
    assert(queue != NULL);
    
    queue_t* new = malloc(sizeof(queue_t));
    assert(new != NULL);
    new->priority = priority;
    new->value = value;
    new->next = NULL;

    // special case: nothing in the queue!
    if (*queue == NULL) {
        *queue = new;
    } else {
        // special case: node should be inserted at head
        if ((*queue)->priority > priority) {
            new->next = *queue;
            *queue = new;
        } else {
            queue_t* prev = NULL;
            queue_t* curr = *queue;
            while (curr != NULL && curr->priority < priority) {
                prev = curr;
                curr = curr->next;
            }
            if (curr != NULL) {
                new->next = curr;
            }
            if (prev != NULL) {
                prev->next = new;
            } else {
                *queue = new;
            }
        }
    }
}

void queue_update_priority(queue_t** queue, int priority, void* value)
{
    assert(queue != NULL);
    queue_t* prev = NULL;
    queue_t* curr = *queue;
    while (curr != NULL && curr->value != value) {
        prev = curr;
        curr = curr->next;
    }
    assert(curr != NULL);
    
    if (prev != NULL) {
        prev->next = curr->next;
    } else {
        *queue = curr->next;
    }
    free(curr);

    queue_push(queue, priority, value);
}

void* queue_pop(queue_t** queue)
{
    assert(queue != NULL);
    if (*queue != NULL) {
        queue_t* head = *queue;
        queue_t* next = head->next;
        *queue = next;
        void* value = head->value;
        free(head);
        return value;
    } else {
        return NULL;
    }
}

void queue_free(queue_t** queue)
{
    assert(queue != NULL);
    queue_t* prev = NULL;
    queue_t* curr = *queue;
    while (curr != NULL) {
        prev = curr;
        curr = curr->next;
        free(prev);
    }
    *queue = NULL;
}

