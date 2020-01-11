/*
 * hash_table.c
 * simple hash table
 */

#include <assert.h>
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "hash_table.h"
#include "priority_queue.h"

static int default_compare(const void* a, const void* b) 
{
    return a == b; // actual values compare
}

hashtable_t* hash_init(size_t size)
{
    return hash_init2(size, default_compare);
}

hashtable_t* hash_init2(size_t size, int (*compar)(const void *, const void*))
{
  hashtable_t* h = calloc(1, sizeof(hashtable_t));
  if (!h) abort();

  h->table_size = size;
  h->compar = compar;

  h->table = calloc(h->table_size, sizeof(hashnode_t));
  if (!h->table) abort();
  return h;
}

int hash(hashtable_t* hashtable, void* key)
{
  intptr_t k = (intptr_t)key;
  int hash = (k == 0) ? 0 : (k % hashtable->table_size);
  return hash;
}

hashnode_t* hash_lookup(hashtable_t* hashtable, void* key)
{
  int h = hash(hashtable, key);
  hashnode_t* n = hashtable->table[h];
  while (n != NULL && (hashtable->compar(n->key, key) != 0)) {
    n = n->next;
  }
  assert(n == NULL || (hashtable->compar(n->key, key) == 0));
  return n;
}

void hash_insert(hashtable_t* hashtable, void* key, void* value)
{
  int h = hash(hashtable, key);
  hashnode_t* curr = hashtable->table[h];
  hashnode_t* prev = NULL;

  // walk the linked list until we exhaust it or find our existing key
  while (curr != NULL && (hashtable->compar(curr->key, key) != 0)) {
    prev = curr;
    curr = curr->next;
  }

  // if we've exhausted the linked list, insert a new node (at the head
  // or at the tail)
  if (curr == NULL) {
    hashnode_t* n = calloc(1, sizeof(hashnode_t));
    assert(n != NULL);
    n->key = key;
    n->value = value;
    if (prev == NULL) {
      hashtable->table[h] = n;
    } else {
      prev->next = n;
    }
  } else { // curr != NULL
    // otherwise, update the existing node's value
    assert(hashtable->compar(curr->key, key) == 0);
    curr->value = value;
  }
}

void hash_free(hashtable_t** t)
{
    for (int i = 0; i < (*t)->table_size; i++) {
        hashnode_t* prev = NULL;
        hashnode_t* curr = (*t)->table[i];
        while (curr != NULL) {
            prev = curr;
            curr = curr->next;
            free(prev);
        }
        (*t)->table[i] = NULL;
    }
    free((*t)->table);
    (*t)->table = NULL;
    (*t)->table_size = 0;
    free(*t);
    *t = NULL;
}

