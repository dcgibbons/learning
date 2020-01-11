/*
 * hash_table.h
 * simple hash table
 *
 * Chad Gibbons
 * dcgibbons@google.com
 * November 19, 2019
 */

#pragma once

typedef struct hashnode {
  struct hashnode* next;
  void* key;
  void* value;
} hashnode_t;

typedef struct hashtable {
  size_t table_size;
  int (*compar)(const void*, const void*);
  hashnode_t** table;
} hashtable_t;

extern hashtable_t* hash_init(size_t size);
extern hashtable_t* hash_init2(size_t size, int (*compar)(const void *, const void*));
extern hashnode_t* hash_lookup(hashtable_t* hashtable, void* key);
extern void hash_insert(hashtable_t* hashtable, void* key, void* value);
extern void hash_free(hashtable_t** hashtable);

