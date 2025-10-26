#include "../include/lisp.h"
#include <string.h>
#include <stdlib.h>

/* Simple hash function (FNV-1a) */
static size_t hash_string(const char *str, size_t bucket_count) {
    unsigned long hash = 2166136261UL;
    const unsigned char *p = (const unsigned char *)str;

    while (*p) {
        hash ^= *p;
        hash *= 16777619UL;
        p++;
    }

    return hash % bucket_count;
}

/* Get entry from hash table */
struct HashEntry *hash_table_get_entry(LispObject *table, const char *key) {
    if (table->type != LISP_HASH_TABLE) {
        return NULL;
    }

    size_t hash = hash_string(key, table->value.hash_table.bucket_count);
    struct HashEntry **buckets = (struct HashEntry **)table->value.hash_table.buckets;

    struct HashEntry *entry = buckets[hash];
    while (entry) {
        if (strcmp(entry->key, key) == 0) {
            return entry;
        }
        entry = entry->next;
    }

    return NULL;
}

/* Set value in hash table */
struct HashEntry *hash_table_set_entry(LispObject *table, const char *key, LispObject *value) {
    if (table->type != LISP_HASH_TABLE) {
        return NULL;
    }

    size_t hash = hash_string(key, table->value.hash_table.bucket_count);
    struct HashEntry **buckets = (struct HashEntry **)table->value.hash_table.buckets;

    /* Check if key exists */
    struct HashEntry *entry = buckets[hash];
    while (entry) {
        if (strcmp(entry->key, key) == 0) {
            entry->value = value;
            return entry;
        }
        entry = entry->next;
    }

    /* Create new entry */
    entry = GC_malloc(sizeof(struct HashEntry));
    entry->key = GC_strdup(key);
    entry->value = value;
    entry->next = buckets[hash];
    buckets[hash] = entry;

    table->value.hash_table.entry_count++;

    /* TODO: Implement auto-resize when load factor > 0.75 */

    return entry;
}

/* Remove entry from hash table */
int hash_table_remove_entry(LispObject *table, const char *key) {
    if (table->type != LISP_HASH_TABLE) {
        return 0;
    }

    size_t hash = hash_string(key, table->value.hash_table.bucket_count);
    struct HashEntry **buckets = (struct HashEntry **)table->value.hash_table.buckets;

    struct HashEntry *entry = buckets[hash];
    struct HashEntry *prev = NULL;

    while (entry) {
        if (strcmp(entry->key, key) == 0) {
            if (prev) {
                prev->next = entry->next;
            } else {
                buckets[hash] = entry->next;
            }
            table->value.hash_table.entry_count--;
            return 1;
        }
        prev = entry;
        entry = entry->next;
    }

    return 0;
}

/* Clear all entries from hash table */
void hash_table_clear(LispObject *table) {
    if (table->type != LISP_HASH_TABLE) {
        return;
    }

    struct HashEntry **buckets = (struct HashEntry **)table->value.hash_table.buckets;

    for (size_t i = 0; i < table->value.hash_table.bucket_count; i++) {
        struct HashEntry *entry = buckets[i];
        while (entry) {
            struct HashEntry *next = entry->next;
            /* GC will handle freeing */
            entry = next;
        }
        buckets[i] = NULL;
    }

    table->value.hash_table.entry_count = 0;
}
