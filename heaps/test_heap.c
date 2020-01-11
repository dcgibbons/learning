/*
 * test_heap.c
 * heap test module
 *
 * David C. Gibbons
 * dcgibbons@gmail.com
 * November 23, 2019
 */

#include <assert.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#include "heap.h"


void test_heap_create()
{
    printf("*** TEST: start test_heap_create\n");

    heap_t heap = heap_create(10, true);
    assert(heap != NULL);

    bool is_empty = heap_is_empty(heap);
    assert(is_empty);

    int value = -1;
    void* data = NULL;
    bool success = heap_peek(heap, &value, &data);
    assert(!success && value == -1 && data == NULL);

    heap_destroy(heap);

    printf("*** TEST: start test_heap_create\n");
}

void test_max_heap()
{
    printf("*** TEST: start test_max_heap\n");

    heap_t heap = heap_create(10, true);
    assert(heap != NULL);

    bool success = heap_push(heap, 100, NULL);
    assert(success);

    int value = -1;
    void* data = NULL;
    success = heap_peek(heap, &value, &data);
    assert(success && value == 100 && data == NULL);

    success = heap_push(heap, 1, NULL);
    assert(success);

    success = heap_push(heap, 101, NULL);
    assert(success);

    success = heap_push(heap, 105, NULL);
    assert(success);

    success = heap_push(heap, 95, NULL);
    assert(success);

    success = heap_peek(heap, &value, &data);
    assert(success && value == 105 && data == NULL);

    heap_destroy(heap);

    printf("*** TEST: end test_max_heap\n");
}

void test_min_heap()
{
    printf("*** TEST: start test_min_heap\n");

    heap_t heap = heap_create(10, false);
    assert(heap != NULL);

    bool success = heap_push(heap, 100, NULL);
    assert(success);

    success = heap_push(heap, 99, NULL);
    assert(success);

    success = heap_push(heap, 2, NULL);
    assert(success);

    success = heap_push(heap, INT_MIN, NULL);
    assert(success);

    int value = -1;
    void* data = NULL;
    success = heap_pop(heap, &value, &data);
    assert(success && value == INT_MIN && data == NULL);

    success = heap_pop(heap, &value, &data);
    assert(success && value == 2 && data == NULL);

    heap_destroy(heap);

    printf("*** TEST: end test_min_heap\n");
}

void test_max_heap_build()
{
    printf("*** TEST: start test_max_heap_build\n");

    int values[] = { 9, 11, 15, 3, 4, 1 };
    void* data[] = { 0, 0, 0, 0, 0, 0 };
    heap_t heap = heap_build(6, true, 6, values, data);
    heap_print(heap, stdout);
    puts("---");

    int value;
    void* data2;
    bool success = heap_peek(heap, &value, &data2);
    assert(success && value == 15);
    heap_print(heap, stdout);

    success = heap_pop(heap, &value, &data2);
    assert(success && value == 15);

    success = heap_pop(heap, &value, &data2);
    assert(success && value == 11);

    heap_destroy(heap);

    printf("*** TEST: end test_max_heap_build\n");
}

void test_min_heap_build()
{
    printf("*** TEST: start test_min_heap_build\n");
    int values[] = { 9, 11, 15, 3, 4, 1 };
    void* data[] = { 0, 0, 0, 0, 0, 0 };
    heap_t heap = heap_build(6, false, 6, values, data);
    printf("heap_build:\n");
    heap_print(heap, stdout);
    puts("---");

    int value;
    void* data2;
    bool success = heap_peek(heap, &value, &data2);
    assert(success && value == 1);

    success = heap_pop(heap, &value, &data2);
    assert(success && value == 1);

    success = heap_pop(heap, &value, &data2);
    assert(success && value == 3);

    heap_destroy(heap);

    printf("*** TEST: end test_min_heap_build\n");
}

void test_heap_sizes()
{
    printf("*** TEST: start test_heap_sizes\n");

    heap_t heap = heap_create(2, true);
    assert(heap != NULL);

    bool success = heap_push(heap, 100, NULL);
    assert(success);

    success = heap_push(heap, 101, NULL);
    assert(success);

    success = heap_push(heap, 99, NULL);
    assert(!success);

    heap_destroy(heap);

    printf("*** TEST: end test_heap_sizes\n");
}

int main(int argc, char* argv[]) 
{
    test_heap_create();
    test_max_heap();
    test_min_heap();
    test_max_heap_build();
    test_min_heap_build();
    test_heap_sizes();
    
    return EXIT_SUCCESS;
}
