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

hashtable_t* hash_init(size_t size)
{
  hashtable_t* h = calloc(1, sizeof(hashtable_t));
  if (!h) abort();

  h->table_size = size;

  h->table = calloc(h->table_size, sizeof(hashnode_t));
  if (!h->table) abort();
  return h;
}

int hash(hashtable_t* hashtable, const char* key)
{
  unsigned hashval;
  for (hashval = 0; *key != '\0'; key++) {
      hashval = *key + 31 * hashval;
  }
  return hashval % hashtable->table_size;
}

hashnode_t* hash_lookup(hashtable_t* hashtable, const char* key)
{
  int h = hash(hashtable, key);
  //printf("key=%s h=%d\n", key, h);
  hashnode_t* n = hashtable->table[h];
  while (n != NULL && strcmp(n->key, key) != 0) {
    //printf("\tn->key=%s\n", n->key);
    n = n->next;
  }
  //if (n != NULL) printf("\t found: n->key=%s\n", n->key);
  assert(n == NULL || strcmp(n->key, key) == 0);
  return n;
}

void hash_insert(hashtable_t* hashtable, const char* key, void* value)
{
  int h = hash(hashtable, key);
  hashnode_t* curr = hashtable->table[h];
  hashnode_t* prev = NULL;

  // walk the linked list until we exhaust it or find our existing key
  while (curr != NULL && strcmp(curr->key, key) != 0) {
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
    assert(strcmp(curr->key, key) == 0);
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

void hash_dump(hashtable_t* hashtable)
{
    printf("hashtable->table_size=%lu\n", hashtable->table_size);
    for (int i = 0; i < hashtable->table_size; i++) {
        hashnode_t* n = hashtable->table[i];
        while (n != NULL) {
            printf("\tn->key=%s\n", n->key);
            n = n->next;
        }
    }
}

