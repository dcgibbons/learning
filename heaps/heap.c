/*
 * heap.c
 * a heap data structure, implemented in C
 *
 * David C. Gibbons
 * dcgibbons@gmail.com
 * November 23, 2019
 */

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "heap.h"


/* Indent 10 spaces for each level when printing the tree */
#define HEAP_PRINT_2D_COUNT     10

/* Calculate left/right/parent positions assuming a binary heap layout */
#define HEAP_LEFT(pos)          (2*(pos))
#define HEAP_RIGHT(pos)         (2*(pos)+1)
#define HEAP_PARENT(pos)        ((pos)/2)


/* A heap node contains the node value (priority) and any associated data */
struct _heap_node {
    int value;
    void* data;
};
typedef struct _heap_node* heap_node_t;


/* A heap contains an array of nodes and bookkeeping variables */
struct _heap {
    size_t struct_size;
    bool is_max_heap;
    size_t max_size;
    size_t curr_size;
    heap_node_t nodes[];  // flexible array member
};


/* forward refernece for internal functions */
static void min_heapify(heap_t heap, size_t pos);
static void max_heapify(heap_t heap, size_t pos);


heap_t heap_create(const size_t max_size, const bool is_max_heap)
{
    // add 1 to the max nodes so we can avoid element 0 to ease coding
    size_t nnodes = max_size + 1;
    size_t struct_size = sizeof(struct _heap)+(sizeof(struct _heap_node)*nnodes);
    heap_t h = calloc(1, struct_size);
    if (h) {
        h->struct_size = struct_size;
        h->is_max_heap = is_max_heap;
        h->max_size = max_size;
    }
    return h;
}


void heap_destroy(heap_t heap)
{
    for (int i = 1; i <= heap->curr_size; i++) {
        if (heap->nodes[i] != NULL) {
            memset(heap->nodes[i], 0xcc, sizeof(struct _heap_node));
        }
        free(heap->nodes[i]);
        heap->nodes[i] = NULL;
    }
    memset(heap, 0xbb, heap->struct_size);
    free(heap);
}


bool heap_is_empty(heap_t heap)
{
    return heap->curr_size == 0;
}


heap_t heap_build(const size_t max_size, const bool is_max_heap,
        const size_t n, int values[], void* data[])
{
    heap_t h = heap_create(max_size, is_max_heap);
    if (h) {
        // insert all of the provided nodes into the heap in the order that they
        // were provided
        for (int i = 0; i < n; i++) {
            heap_node_t new_node = malloc(sizeof(*new_node));
            new_node->value = values[i];
            new_node->data = data[i];
            h->curr_size++;
            h->nodes[h->curr_size] = new_node;
            assert(h->curr_size <= h->max_size);
        }

        // re-heapify the heap to order it properly
        for (int i = h->curr_size / 2; i > 0; i--) {
            if (is_max_heap) {
                max_heapify(h, i);
            } else {
                min_heapify(h, i);
            }
        }
    }
    return h;
}


static void min_heapify(heap_t h, size_t pos)
{
    size_t left = HEAP_LEFT(pos);
    size_t right = HEAP_RIGHT(pos);
    size_t smallest = pos;

    if (left <= h->curr_size && 
            h->nodes[left]->value < h->nodes[smallest]->value) {
        smallest = left;
    }

    if (right <= h->curr_size && 
            h->nodes[right]->value < h->nodes[smallest]->value) {
        smallest = right;
    }

    if (smallest != pos) {
        heap_node_t temp = h->nodes[pos];
        h->nodes[pos] = h->nodes[smallest];
        h->nodes[smallest] = temp;
        min_heapify(h, smallest);
    }
}


static void max_heapify(heap_t h, size_t pos)
{
    size_t left = HEAP_LEFT(pos);
    size_t right = HEAP_RIGHT(pos);
    size_t largest = pos;

    if (left <= h->curr_size && 
            h->nodes[left]->value > h->nodes[largest]->value) {
        largest = left;
    }
    if (right <= h->curr_size && 
            h->nodes[right]->value > h->nodes[largest]->value) {
        largest = right;
    }

    if (largest != pos) {
        heap_node_t temp = h->nodes[pos];
        h->nodes[pos] = h->nodes[largest];
        h->nodes[largest] = temp;
        max_heapify(h, largest);
    }
}


bool heap_push(heap_t heap, int value, void* data)
{
    if (heap->curr_size >= heap->max_size) {
        return false;
    }
    int new_pos = ++(heap->curr_size);

    heap_node_t new_node = malloc(sizeof(*new_node));
    if (!new_node) {
        return false;
    }
    new_node->value = value;
    new_node->data = data;

    // add the new node to the most-left position in the heap
    heap->nodes[new_pos] = new_node;

    if (new_pos > 1) {
        // percolate-up to re-heapify
        int parent_pos = HEAP_PARENT(new_pos);
        int curr_pos = new_pos;
        while (heap->is_max_heap ? (heap->nodes[parent_pos]->value < value)
                                 : (heap->nodes[parent_pos]->value > value)) {

            heap_node_t temp = heap->nodes[parent_pos];
            heap->nodes[parent_pos] = new_node;
            heap->nodes[curr_pos] = temp;
            
            if (parent_pos == 1) break; // we are at the root, finish!

            curr_pos = parent_pos;
            parent_pos = HEAP_PARENT(parent_pos);
        }
    }

    return true;
}


bool heap_peek(heap_t heap, int* value, void** data)
{
    bool success = false;

    if (heap->curr_size > 0) {
        assert(heap->nodes[1] != NULL);
        success = true;
        if (value != NULL) *value = heap->nodes[1]->value;
        if (data != NULL) *data = heap->nodes[1]->data;
    }

    return success;
}


bool heap_pop(heap_t heap, int* value, void** data)
{
    bool success = false;

    if (heap->curr_size > 0) {
        success = true;

        // remove the root node and then percolate-down to re-heapify
        heap_node_t old_node = heap->nodes[1];
        *value = old_node->value;
        *data = old_node->data;
        memset(old_node, 0xff, sizeof(*old_node));
        free(old_node);

        if (heap->curr_size > 1) {
            heap->nodes[1] = heap->nodes[heap->curr_size];
            heap->curr_size--;

            if (heap->is_max_heap) {
                max_heapify(heap, 1);
            } else {
                min_heapify(heap, 1);
            }
        }
    }

    return success;
}


static void print_2d(heap_t heap, int pos, int space, FILE* out)
{
    // base case - no node because we're out of space
    if (pos > heap->curr_size) {
       return;
    }

    // base case - no node at all
    const heap_node_t node = heap->nodes[pos];
    if (node == NULL) {
        return;
    }

    // increase distance between levels
    space += HEAP_PRINT_2D_COUNT;

    // process right child first
    print_2d(heap, HEAP_RIGHT(pos), space, out);

    // print current node
    fprintf(out, "\n");
    for (int i = HEAP_PRINT_2D_COUNT; i < space; i++) {
        fprintf(out, " ");
    }
    fprintf(out, "%d\n", node->value);

    // process left child
    print_2d(heap, HEAP_LEFT(pos), space, out);
}


void heap_print(heap_t heap, FILE* out)
{
    fprintf(out, "--- HEAP:\n");
    print_2d(heap, 1, 0, out);
    fprintf(out, "---\n");
    fflush(out);
}

