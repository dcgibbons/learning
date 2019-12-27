/*
 * priority_queue.h
 * a simple (min) priority queue
 *
 * Chad Gibbons
 * dcgibbons@google.com
 * November 19, 2019
 */

#pragma once

typedef struct queue_node {
    struct queue_node* next;
    int priority;
    void* value; 
} queue_t;

extern void queue_push(queue_t** queue, int priority, void* value);
extern void queue_update_priority(queue_t** queue, int priority, void* value);
extern void* queue_pop(queue_t** queue);
extern void queue_free(queue_t** queue);

