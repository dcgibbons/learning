/*
 * heap.h
 * a heap data structure, written in C
 *
 * David C. Gibbons
 * dcgibbons@gmail.com
 * November 23, 2019
 */

#pragma once

#include <stdbool.h>
#include <stdio.h>

typedef struct _heap* heap_t;

/*
 * heap_create: create a new heap with up to max_size elements. This heap
 * should be destroyed after use with heap_destroy to free any allocated
 * memory. If is_max_heap is true, then the heap will be a max-value heap,
 * otherwise the heap will be a min-value heap.
 */
extern heap_t heap_create(const size_t max_size, const bool is_max_heap);


/*
 * heap_destroy: destroys a previously created heap
 */
extern void heap_destroy(heap_t heap);


/*
 * heap_is_empty: returns true if the specified heap is empty
 */
extern bool heap_is_empty(heap_t heap);


/*
 * heap_build: builds a new heap from an an array of values
 */
extern heap_t heap_build(const size_t max_size, const bool is_max_heap,
                         const size_t n, int values[], void* data[]);


/*
 * heap_push: pushes a new value and associated data onto the heap. Returns
 * false if the operation failed.
 */
extern bool heap_push(heap_t heap, int value, void* data);


/*
 * heap_pop: pops the lowest (min heap) or highest (max heap) off the heap and
 * returns the value and associated data. Returns false is no data was on the
 * heap.
 */
extern bool heap_pop(heap_t heap, int* value, void** data);


/*
 * heap_peek: peeps at the lowest (min heap) or highest (max heap) value on
 * the heap, but does not remove it. Returns false if no data was on the heap.
 */
extern bool heap_peek(heap_t heap, int* value, void** data);


/*
 * heap_print: prints the heap to the specified stdio file stream.
 */
extern void heap_print(heap_t heap, FILE* out);

