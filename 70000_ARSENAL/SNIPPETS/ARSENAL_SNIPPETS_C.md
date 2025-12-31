---
title: "ARCHAEON Arsenal - C Language Snippets"
version: "1.0.0"
category: "ARSENAL/SNIPPETS"
language: "C (C99/C11)"
purpose: "Practical C patterns for systems programming and application development"
created: "2025-12-31"
tags: ["c", "systems", "memory", "data-structures", "threads", "posix"]
complexity: "Intermediate to Advanced"
---

# ARCHAEON ARSENAL: C Language Snippets

> **Mission**: Provide battle-tested C patterns for systems programming,
> memory management, data structures, and robust application development.

---

## Table of Contents

1. [Memory Management Patterns](#memory-management-patterns)
2. [String Handling](#string-handling)
3. [File I/O Patterns](#file-io-patterns)
4. [Data Structures](#data-structures)
5. [Error Handling](#error-handling)
6. [Threading Patterns](#threading-patterns)
7. [Utility Macros](#utility-macros)
8. [Platform Detection](#platform-detection)

---

## Memory Management Patterns

### Safe Allocation Wrappers

```c
/*******************************************************************************
 * Safe Memory Allocation Wrappers
 * Handles NULL checks and provides debugging capabilities
 ******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Debug mode tracking */
#ifdef DEBUG_MEMORY
    #define MEM_TRACK 1
#else
    #define MEM_TRACK 0
#endif

/* Allocation tracking structure */
typedef struct mem_block {
    void *ptr;
    size_t size;
    const char *file;
    int line;
    struct mem_block *next;
} mem_block_t;

static mem_block_t *mem_list = NULL;
static size_t total_allocated = 0;
static size_t total_freed = 0;

/**
 * Safe malloc with NULL check and optional tracking
 */
void *safe_malloc(size_t size, const char *file, int line) {
    void *ptr = malloc(size);

    if (ptr == NULL) {
        fprintf(stderr, "ERROR: malloc failed at %s:%d (size=%zu)\n",
                file, line, size);
        exit(EXIT_FAILURE);
    }

    if (MEM_TRACK) {
        mem_block_t *block = malloc(sizeof(mem_block_t));
        if (block) {
            block->ptr = ptr;
            block->size = size;
            block->file = file;
            block->line = line;
            block->next = mem_list;
            mem_list = block;
            total_allocated += size;
        }
    }

    return ptr;
}

/**
 * Safe calloc with NULL check
 */
void *safe_calloc(size_t nmemb, size_t size, const char *file, int line) {
    void *ptr = calloc(nmemb, size);

    if (ptr == NULL) {
        fprintf(stderr, "ERROR: calloc failed at %s:%d (n=%zu, size=%zu)\n",
                file, line, nmemb, size);
        exit(EXIT_FAILURE);
    }

    if (MEM_TRACK) {
        mem_block_t *block = malloc(sizeof(mem_block_t));
        if (block) {
            block->ptr = ptr;
            block->size = nmemb * size;
            block->file = file;
            block->line = line;
            block->next = mem_list;
            mem_list = block;
            total_allocated += nmemb * size;
        }
    }

    return ptr;
}

/**
 * Safe realloc with NULL check
 */
void *safe_realloc(void *ptr, size_t size, const char *file, int line) {
    void *new_ptr = realloc(ptr, size);

    if (new_ptr == NULL && size != 0) {
        fprintf(stderr, "ERROR: realloc failed at %s:%d (size=%zu)\n",
                file, line, size);
        exit(EXIT_FAILURE);
    }

    if (MEM_TRACK && ptr != new_ptr) {
        /* Update tracking - remove old, add new */
        mem_block_t *prev = NULL;
        mem_block_t *curr = mem_list;

        while (curr != NULL) {
            if (curr->ptr == ptr) {
                total_freed += curr->size;
                if (prev)
                    prev->next = curr->next;
                else
                    mem_list = curr->next;
                free(curr);
                break;
            }
            prev = curr;
            curr = curr->next;
        }

        mem_block_t *block = malloc(sizeof(mem_block_t));
        if (block) {
            block->ptr = new_ptr;
            block->size = size;
            block->file = file;
            block->line = line;
            block->next = mem_list;
            mem_list = block;
            total_allocated += size;
        }
    }

    return new_ptr;
}

/**
 * Safe free with NULL check
 */
void safe_free(void *ptr, const char *file, int line) {
    if (ptr == NULL) {
        fprintf(stderr, "WARNING: Attempt to free NULL at %s:%d\n", file, line);
        return;
    }

    if (MEM_TRACK) {
        mem_block_t *prev = NULL;
        mem_block_t *curr = mem_list;

        while (curr != NULL) {
            if (curr->ptr == ptr) {
                total_freed += curr->size;
                if (prev)
                    prev->next = curr->next;
                else
                    mem_list = curr->next;
                free(curr);
                break;
            }
            prev = curr;
            curr = curr->next;
        }
    }

    free(ptr);
}

/**
 * Print memory leak report
 */
void mem_report(void) {
    if (!MEM_TRACK) return;

    printf("\n=== Memory Report ===\n");
    printf("Total allocated: %zu bytes\n", total_allocated);
    printf("Total freed:     %zu bytes\n", total_freed);
    printf("Balance:         %zu bytes\n", total_allocated - total_freed);

    if (mem_list != NULL) {
        printf("\nPotential memory leaks:\n");
        mem_block_t *curr = mem_list;
        while (curr != NULL) {
            printf("  %zu bytes at %s:%d\n", curr->size, curr->file, curr->line);
            curr = curr->next;
        }
    }
}

/* Convenience macros */
#define MALLOC(size)        safe_malloc(size, __FILE__, __LINE__)
#define CALLOC(n, size)     safe_calloc(n, size, __FILE__, __LINE__)
#define REALLOC(ptr, size)  safe_realloc(ptr, size, __FILE__, __LINE__)
#define FREE(ptr)           safe_free(ptr, __FILE__, __LINE__)
```

### Memory Pool Allocator

```c
/*******************************************************************************
 * Fixed-Size Memory Pool Allocator
 * Efficient allocation for objects of uniform size
 ******************************************************************************/

#include <stdint.h>
#include <stdbool.h>

#define POOL_BLOCK_SIZE 4096

typedef struct pool_block {
    struct pool_block *next;
    uint8_t data[];
} pool_block_t;

typedef struct {
    size_t obj_size;          /* Size of each object */
    size_t obj_per_block;     /* Objects per block */
    pool_block_t *blocks;     /* List of allocated blocks */
    void *free_list;          /* List of free objects */
    size_t total_allocated;
    size_t total_free;
} mem_pool_t;

/**
 * Initialize memory pool
 */
mem_pool_t *pool_create(size_t obj_size) {
    /* Ensure minimum size for free list pointer */
    if (obj_size < sizeof(void*))
        obj_size = sizeof(void*);

    /* Align to pointer size */
    obj_size = (obj_size + sizeof(void*) - 1) & ~(sizeof(void*) - 1);

    mem_pool_t *pool = malloc(sizeof(mem_pool_t));
    if (!pool) return NULL;

    pool->obj_size = obj_size;
    pool->obj_per_block = (POOL_BLOCK_SIZE - sizeof(pool_block_t)) / obj_size;
    pool->blocks = NULL;
    pool->free_list = NULL;
    pool->total_allocated = 0;
    pool->total_free = 0;

    return pool;
}

/**
 * Allocate new block and add objects to free list
 */
static bool pool_grow(mem_pool_t *pool) {
    size_t block_size = sizeof(pool_block_t) + pool->obj_size * pool->obj_per_block;
    pool_block_t *block = malloc(block_size);
    if (!block) return false;

    /* Link new block */
    block->next = pool->blocks;
    pool->blocks = block;

    /* Add all objects to free list */
    uint8_t *ptr = block->data;
    for (size_t i = 0; i < pool->obj_per_block; i++) {
        *(void**)ptr = pool->free_list;
        pool->free_list = ptr;
        ptr += pool->obj_size;
        pool->total_free++;
    }

    return true;
}

/**
 * Allocate object from pool
 */
void *pool_alloc(mem_pool_t *pool) {
    if (pool->free_list == NULL) {
        if (!pool_grow(pool)) return NULL;
    }

    void *obj = pool->free_list;
    pool->free_list = *(void**)obj;
    pool->total_free--;
    pool->total_allocated++;

    return obj;
}

/**
 * Return object to pool
 */
void pool_free(mem_pool_t *pool, void *obj) {
    if (!obj) return;

    *(void**)obj = pool->free_list;
    pool->free_list = obj;
    pool->total_free++;
    pool->total_allocated--;
}

/**
 * Destroy pool and free all memory
 */
void pool_destroy(mem_pool_t *pool) {
    pool_block_t *block = pool->blocks;
    while (block) {
        pool_block_t *next = block->next;
        free(block);
        block = next;
    }
    free(pool);
}
```

---

## String Handling

### Safe String Functions

```c
/*******************************************************************************
 * Safe String Handling Functions
 * Prevent buffer overflows and NULL pointer issues
 ******************************************************************************/

#include <string.h>
#include <ctype.h>
#include <stdarg.h>

/**
 * Safe string copy with guaranteed null termination
 */
size_t str_copy(char *dest, size_t dest_size, const char *src) {
    if (dest == NULL || dest_size == 0) return 0;
    if (src == NULL) {
        dest[0] = '\0';
        return 0;
    }

    size_t src_len = strlen(src);
    size_t copy_len = (src_len >= dest_size) ? dest_size - 1 : src_len;

    memcpy(dest, src, copy_len);
    dest[copy_len] = '\0';

    return copy_len;
}

/**
 * Safe string concatenation
 */
size_t str_concat(char *dest, size_t dest_size, const char *src) {
    if (dest == NULL || dest_size == 0) return 0;
    if (src == NULL) return strlen(dest);

    size_t dest_len = strlen(dest);
    if (dest_len >= dest_size - 1) return dest_len;

    size_t remaining = dest_size - dest_len - 1;
    size_t src_len = strlen(src);
    size_t copy_len = (src_len > remaining) ? remaining : src_len;

    memcpy(dest + dest_len, src, copy_len);
    dest[dest_len + copy_len] = '\0';

    return dest_len + copy_len;
}

/**
 * Safe string formatting
 */
int str_printf(char *dest, size_t dest_size, const char *fmt, ...) {
    if (dest == NULL || dest_size == 0) return -1;

    va_list args;
    va_start(args, fmt);
    int result = vsnprintf(dest, dest_size, fmt, args);
    va_end(args);

    /* Ensure null termination */
    if (result >= (int)dest_size)
        dest[dest_size - 1] = '\0';

    return result;
}

/**
 * Duplicate string with allocation
 */
char *str_dup(const char *src) {
    if (src == NULL) return NULL;

    size_t len = strlen(src) + 1;
    char *dest = malloc(len);
    if (dest)
        memcpy(dest, src, len);

    return dest;
}

/**
 * Duplicate n characters of string
 */
char *str_ndup(const char *src, size_t n) {
    if (src == NULL) return NULL;

    size_t len = strlen(src);
    if (n > len) n = len;

    char *dest = malloc(n + 1);
    if (dest) {
        memcpy(dest, src, n);
        dest[n] = '\0';
    }

    return dest;
}

/**
 * Trim whitespace from both ends (modifies in place)
 */
char *str_trim(char *str) {
    if (str == NULL) return NULL;

    /* Trim leading whitespace */
    while (isspace((unsigned char)*str)) str++;

    if (*str == '\0') return str;

    /* Trim trailing whitespace */
    char *end = str + strlen(str) - 1;
    while (end > str && isspace((unsigned char)*end)) end--;
    *(end + 1) = '\0';

    return str;
}

/**
 * Convert string to uppercase (modifies in place)
 */
char *str_upper(char *str) {
    if (str == NULL) return NULL;

    for (char *p = str; *p; p++)
        *p = toupper((unsigned char)*p);

    return str;
}

/**
 * Convert string to lowercase (modifies in place)
 */
char *str_lower(char *str) {
    if (str == NULL) return NULL;

    for (char *p = str; *p; p++)
        *p = tolower((unsigned char)*p);

    return str;
}

/**
 * Split string by delimiter
 * Returns array of strings (caller must free)
 */
char **str_split(const char *str, char delim, int *count) {
    if (str == NULL || count == NULL) return NULL;

    /* Count tokens */
    int n = 1;
    for (const char *p = str; *p; p++)
        if (*p == delim) n++;

    char **result = malloc(sizeof(char*) * (n + 1));
    if (!result) return NULL;

    char *copy = str_dup(str);
    if (!copy) {
        free(result);
        return NULL;
    }

    int i = 0;
    char *token = copy;
    char *p = copy;

    while (*p) {
        if (*p == delim) {
            *p = '\0';
            result[i++] = str_dup(token);
            token = p + 1;
        }
        p++;
    }
    result[i++] = str_dup(token);
    result[i] = NULL;

    free(copy);
    *count = i;
    return result;
}

/**
 * Free split result
 */
void str_split_free(char **tokens) {
    if (tokens == NULL) return;

    for (char **p = tokens; *p; p++)
        free(*p);
    free(tokens);
}

/**
 * Check if string starts with prefix
 */
bool str_starts_with(const char *str, const char *prefix) {
    if (str == NULL || prefix == NULL) return false;
    return strncmp(str, prefix, strlen(prefix)) == 0;
}

/**
 * Check if string ends with suffix
 */
bool str_ends_with(const char *str, const char *suffix) {
    if (str == NULL || suffix == NULL) return false;

    size_t str_len = strlen(str);
    size_t suffix_len = strlen(suffix);

    if (suffix_len > str_len) return false;

    return strcmp(str + str_len - suffix_len, suffix) == 0;
}

/**
 * Replace all occurrences of substring
 * Returns new allocated string
 */
char *str_replace(const char *str, const char *old, const char *new) {
    if (str == NULL || old == NULL || new == NULL) return NULL;
    if (old[0] == '\0') return str_dup(str);

    size_t old_len = strlen(old);
    size_t new_len = strlen(new);

    /* Count occurrences */
    int count = 0;
    const char *p = str;
    while ((p = strstr(p, old)) != NULL) {
        count++;
        p += old_len;
    }

    if (count == 0) return str_dup(str);

    /* Allocate result */
    size_t result_len = strlen(str) + count * (new_len - old_len);
    char *result = malloc(result_len + 1);
    if (!result) return NULL;

    /* Build result */
    char *dest = result;
    p = str;
    const char *q;

    while ((q = strstr(p, old)) != NULL) {
        size_t len = q - p;
        memcpy(dest, p, len);
        dest += len;
        memcpy(dest, new, new_len);
        dest += new_len;
        p = q + old_len;
    }

    strcpy(dest, p);
    return result;
}
```

---

## File I/O Patterns

### Robust File Operations

```c
/*******************************************************************************
 * Robust File I/O Operations
 * Error handling, buffering, and cross-platform support
 ******************************************************************************/

#include <stdio.h>
#include <errno.h>
#include <sys/stat.h>

/**
 * Read entire file into buffer
 * Caller must free the returned buffer
 */
char *file_read_all(const char *path, size_t *size) {
    FILE *fp = fopen(path, "rb");
    if (!fp) {
        perror("fopen");
        return NULL;
    }

    /* Get file size */
    if (fseek(fp, 0, SEEK_END) != 0) {
        perror("fseek");
        fclose(fp);
        return NULL;
    }

    long file_size = ftell(fp);
    if (file_size < 0) {
        perror("ftell");
        fclose(fp);
        return NULL;
    }

    rewind(fp);

    /* Allocate buffer */
    char *buffer = malloc(file_size + 1);
    if (!buffer) {
        fprintf(stderr, "malloc failed for %ld bytes\n", file_size);
        fclose(fp);
        return NULL;
    }

    /* Read file */
    size_t bytes_read = fread(buffer, 1, file_size, fp);
    if (bytes_read != (size_t)file_size && ferror(fp)) {
        perror("fread");
        free(buffer);
        fclose(fp);
        return NULL;
    }

    buffer[bytes_read] = '\0';

    fclose(fp);

    if (size) *size = bytes_read;
    return buffer;
}

/**
 * Write buffer to file atomically
 * Writes to temp file first, then renames
 */
int file_write_atomic(const char *path, const void *data, size_t size) {
    char temp_path[FILENAME_MAX];
    snprintf(temp_path, sizeof(temp_path), "%s.tmp.%d", path, getpid());

    FILE *fp = fopen(temp_path, "wb");
    if (!fp) {
        perror("fopen");
        return -1;
    }

    size_t written = fwrite(data, 1, size, fp);
    if (written != size) {
        perror("fwrite");
        fclose(fp);
        remove(temp_path);
        return -1;
    }

    /* Flush to disk */
    if (fflush(fp) != 0) {
        perror("fflush");
        fclose(fp);
        remove(temp_path);
        return -1;
    }

#ifdef _POSIX_VERSION
    /* fsync for durability */
    if (fsync(fileno(fp)) != 0) {
        perror("fsync");
    }
#endif

    fclose(fp);

    /* Atomic rename */
    if (rename(temp_path, path) != 0) {
        perror("rename");
        remove(temp_path);
        return -1;
    }

    return 0;
}

/**
 * Append data to file
 */
int file_append(const char *path, const void *data, size_t size) {
    FILE *fp = fopen(path, "ab");
    if (!fp) {
        perror("fopen");
        return -1;
    }

    size_t written = fwrite(data, 1, size, fp);
    if (written != size) {
        perror("fwrite");
        fclose(fp);
        return -1;
    }

    fclose(fp);
    return 0;
}

/**
 * Read file line by line with callback
 */
typedef void (*line_callback_t)(const char *line, size_t line_num, void *ctx);

int file_read_lines(const char *path, line_callback_t callback, void *ctx) {
    FILE *fp = fopen(path, "r");
    if (!fp) {
        perror("fopen");
        return -1;
    }

    char *line = NULL;
    size_t len = 0;
    ssize_t read;
    size_t line_num = 0;

    while ((read = getline(&line, &len, fp)) != -1) {
        /* Remove trailing newline */
        if (read > 0 && line[read - 1] == '\n')
            line[read - 1] = '\0';

        callback(line, ++line_num, ctx);
    }

    free(line);
    fclose(fp);
    return 0;
}

/**
 * Check if file exists
 */
bool file_exists(const char *path) {
    struct stat st;
    return stat(path, &st) == 0;
}

/**
 * Get file size
 */
ssize_t file_size(const char *path) {
    struct stat st;
    if (stat(path, &st) != 0) return -1;
    return st.st_size;
}

/**
 * Copy file
 */
int file_copy(const char *src_path, const char *dest_path) {
    FILE *src = fopen(src_path, "rb");
    if (!src) {
        perror("fopen src");
        return -1;
    }

    FILE *dest = fopen(dest_path, "wb");
    if (!dest) {
        perror("fopen dest");
        fclose(src);
        return -1;
    }

    char buffer[8192];
    size_t bytes;
    int result = 0;

    while ((bytes = fread(buffer, 1, sizeof(buffer), src)) > 0) {
        if (fwrite(buffer, 1, bytes, dest) != bytes) {
            perror("fwrite");
            result = -1;
            break;
        }
    }

    if (ferror(src)) {
        perror("fread");
        result = -1;
    }

    fclose(src);
    fclose(dest);

    return result;
}
```

---

## Data Structures

### Linked List

```c
/*******************************************************************************
 * Generic Doubly Linked List Implementation
 ******************************************************************************/

typedef struct list_node {
    void *data;
    struct list_node *prev;
    struct list_node *next;
} list_node_t;

typedef struct {
    list_node_t *head;
    list_node_t *tail;
    size_t size;
    void (*free_func)(void*);
} linked_list_t;

/**
 * Create new linked list
 */
linked_list_t *list_create(void (*free_func)(void*)) {
    linked_list_t *list = malloc(sizeof(linked_list_t));
    if (!list) return NULL;

    list->head = NULL;
    list->tail = NULL;
    list->size = 0;
    list->free_func = free_func;

    return list;
}

/**
 * Add element to front
 */
bool list_push_front(linked_list_t *list, void *data) {
    list_node_t *node = malloc(sizeof(list_node_t));
    if (!node) return false;

    node->data = data;
    node->prev = NULL;
    node->next = list->head;

    if (list->head)
        list->head->prev = node;
    else
        list->tail = node;

    list->head = node;
    list->size++;

    return true;
}

/**
 * Add element to back
 */
bool list_push_back(linked_list_t *list, void *data) {
    list_node_t *node = malloc(sizeof(list_node_t));
    if (!node) return false;

    node->data = data;
    node->next = NULL;
    node->prev = list->tail;

    if (list->tail)
        list->tail->next = node;
    else
        list->head = node;

    list->tail = node;
    list->size++;

    return true;
}

/**
 * Remove element from front
 */
void *list_pop_front(linked_list_t *list) {
    if (!list->head) return NULL;

    list_node_t *node = list->head;
    void *data = node->data;

    list->head = node->next;
    if (list->head)
        list->head->prev = NULL;
    else
        list->tail = NULL;

    free(node);
    list->size--;

    return data;
}

/**
 * Remove element from back
 */
void *list_pop_back(linked_list_t *list) {
    if (!list->tail) return NULL;

    list_node_t *node = list->tail;
    void *data = node->data;

    list->tail = node->prev;
    if (list->tail)
        list->tail->next = NULL;
    else
        list->head = NULL;

    free(node);
    list->size--;

    return data;
}

/**
 * Find element with callback
 */
typedef bool (*list_predicate_t)(const void *data, const void *ctx);

void *list_find(linked_list_t *list, list_predicate_t pred, const void *ctx) {
    for (list_node_t *node = list->head; node; node = node->next) {
        if (pred(node->data, ctx))
            return node->data;
    }
    return NULL;
}

/**
 * Iterate over list
 */
typedef void (*list_iterator_t)(void *data, void *ctx);

void list_foreach(linked_list_t *list, list_iterator_t func, void *ctx) {
    for (list_node_t *node = list->head; node; node = node->next) {
        func(node->data, ctx);
    }
}

/**
 * Destroy list
 */
void list_destroy(linked_list_t *list) {
    list_node_t *node = list->head;
    while (node) {
        list_node_t *next = node->next;
        if (list->free_func)
            list->free_func(node->data);
        free(node);
        node = next;
    }
    free(list);
}
```

### Hash Table

```c
/*******************************************************************************
 * Generic Hash Table Implementation
 * Uses separate chaining for collision resolution
 ******************************************************************************/

#include <stdint.h>

#define HASH_INITIAL_SIZE 16
#define HASH_LOAD_FACTOR  0.75

typedef struct hash_entry {
    char *key;
    void *value;
    struct hash_entry *next;
} hash_entry_t;

typedef struct {
    hash_entry_t **buckets;
    size_t bucket_count;
    size_t size;
    void (*free_func)(void*);
} hash_table_t;

/**
 * FNV-1a hash function
 */
static uint32_t hash_fnv1a(const char *key) {
    uint32_t hash = 2166136261u;
    while (*key) {
        hash ^= (uint8_t)*key++;
        hash *= 16777619u;
    }
    return hash;
}

/**
 * Create hash table
 */
hash_table_t *hash_create(void (*free_func)(void*)) {
    hash_table_t *ht = malloc(sizeof(hash_table_t));
    if (!ht) return NULL;

    ht->bucket_count = HASH_INITIAL_SIZE;
    ht->buckets = calloc(ht->bucket_count, sizeof(hash_entry_t*));
    if (!ht->buckets) {
        free(ht);
        return NULL;
    }

    ht->size = 0;
    ht->free_func = free_func;

    return ht;
}

/**
 * Resize hash table
 */
static bool hash_resize(hash_table_t *ht, size_t new_size) {
    hash_entry_t **new_buckets = calloc(new_size, sizeof(hash_entry_t*));
    if (!new_buckets) return false;

    /* Rehash all entries */
    for (size_t i = 0; i < ht->bucket_count; i++) {
        hash_entry_t *entry = ht->buckets[i];
        while (entry) {
            hash_entry_t *next = entry->next;
            uint32_t index = hash_fnv1a(entry->key) % new_size;

            entry->next = new_buckets[index];
            new_buckets[index] = entry;

            entry = next;
        }
    }

    free(ht->buckets);
    ht->buckets = new_buckets;
    ht->bucket_count = new_size;

    return true;
}

/**
 * Insert or update key-value pair
 */
bool hash_put(hash_table_t *ht, const char *key, void *value) {
    /* Check load factor */
    if ((double)ht->size / ht->bucket_count > HASH_LOAD_FACTOR) {
        if (!hash_resize(ht, ht->bucket_count * 2))
            return false;
    }

    uint32_t index = hash_fnv1a(key) % ht->bucket_count;

    /* Check if key exists */
    hash_entry_t *entry = ht->buckets[index];
    while (entry) {
        if (strcmp(entry->key, key) == 0) {
            if (ht->free_func)
                ht->free_func(entry->value);
            entry->value = value;
            return true;
        }
        entry = entry->next;
    }

    /* Create new entry */
    entry = malloc(sizeof(hash_entry_t));
    if (!entry) return false;

    entry->key = str_dup(key);
    if (!entry->key) {
        free(entry);
        return false;
    }

    entry->value = value;
    entry->next = ht->buckets[index];
    ht->buckets[index] = entry;
    ht->size++;

    return true;
}

/**
 * Get value by key
 */
void *hash_get(hash_table_t *ht, const char *key) {
    uint32_t index = hash_fnv1a(key) % ht->bucket_count;

    hash_entry_t *entry = ht->buckets[index];
    while (entry) {
        if (strcmp(entry->key, key) == 0)
            return entry->value;
        entry = entry->next;
    }

    return NULL;
}

/**
 * Check if key exists
 */
bool hash_contains(hash_table_t *ht, const char *key) {
    return hash_get(ht, key) != NULL;
}

/**
 * Remove key
 */
bool hash_remove(hash_table_t *ht, const char *key) {
    uint32_t index = hash_fnv1a(key) % ht->bucket_count;

    hash_entry_t *prev = NULL;
    hash_entry_t *entry = ht->buckets[index];

    while (entry) {
        if (strcmp(entry->key, key) == 0) {
            if (prev)
                prev->next = entry->next;
            else
                ht->buckets[index] = entry->next;

            free(entry->key);
            if (ht->free_func)
                ht->free_func(entry->value);
            free(entry);
            ht->size--;

            return true;
        }
        prev = entry;
        entry = entry->next;
    }

    return false;
}

/**
 * Iterate over all entries
 */
typedef void (*hash_iterator_t)(const char *key, void *value, void *ctx);

void hash_foreach(hash_table_t *ht, hash_iterator_t func, void *ctx) {
    for (size_t i = 0; i < ht->bucket_count; i++) {
        hash_entry_t *entry = ht->buckets[i];
        while (entry) {
            func(entry->key, entry->value, ctx);
            entry = entry->next;
        }
    }
}

/**
 * Destroy hash table
 */
void hash_destroy(hash_table_t *ht) {
    for (size_t i = 0; i < ht->bucket_count; i++) {
        hash_entry_t *entry = ht->buckets[i];
        while (entry) {
            hash_entry_t *next = entry->next;
            free(entry->key);
            if (ht->free_func)
                ht->free_func(entry->value);
            free(entry);
            entry = next;
        }
    }
    free(ht->buckets);
    free(ht);
}
```

---

## Error Handling

### Error Handling Macros and Utilities

```c
/*******************************************************************************
 * Error Handling Framework
 ******************************************************************************/

#include <errno.h>
#include <setjmp.h>

/* Error codes */
typedef enum {
    ERR_OK = 0,
    ERR_NULL_PTR,
    ERR_OUT_OF_MEMORY,
    ERR_INVALID_ARGUMENT,
    ERR_FILE_NOT_FOUND,
    ERR_IO_ERROR,
    ERR_TIMEOUT,
    ERR_OVERFLOW,
    ERR_UNKNOWN
} error_code_t;

/* Error context structure */
typedef struct {
    error_code_t code;
    int sys_errno;
    const char *file;
    int line;
    const char *func;
    char message[256];
} error_t;

/* Thread-local error storage */
static _Thread_local error_t g_last_error = {0};

/**
 * Set error with context
 */
#define SET_ERROR(code, msg, ...) \
    set_error_impl(code, __FILE__, __LINE__, __func__, msg, ##__VA_ARGS__)

void set_error_impl(error_code_t code, const char *file, int line,
                    const char *func, const char *fmt, ...) {
    g_last_error.code = code;
    g_last_error.sys_errno = errno;
    g_last_error.file = file;
    g_last_error.line = line;
    g_last_error.func = func;

    va_list args;
    va_start(args, fmt);
    vsnprintf(g_last_error.message, sizeof(g_last_error.message), fmt, args);
    va_end(args);
}

/**
 * Get last error
 */
const error_t *get_last_error(void) {
    return &g_last_error;
}

/**
 * Clear error
 */
void clear_error(void) {
    memset(&g_last_error, 0, sizeof(g_last_error));
}

/**
 * Print error message
 */
void print_error(void) {
    if (g_last_error.code != ERR_OK) {
        fprintf(stderr, "Error [%d] at %s:%d in %s(): %s",
                g_last_error.code,
                g_last_error.file,
                g_last_error.line,
                g_last_error.func,
                g_last_error.message);
        if (g_last_error.sys_errno != 0) {
            fprintf(stderr, " (errno=%d: %s)",
                    g_last_error.sys_errno,
                    strerror(g_last_error.sys_errno));
        }
        fprintf(stderr, "\n");
    }
}

/* Convenience macros */
#define CHECK_NULL(ptr) \
    do { \
        if ((ptr) == NULL) { \
            SET_ERROR(ERR_NULL_PTR, "NULL pointer: " #ptr); \
            return NULL; \
        } \
    } while (0)

#define CHECK_NULL_RET(ptr, ret) \
    do { \
        if ((ptr) == NULL) { \
            SET_ERROR(ERR_NULL_PTR, "NULL pointer: " #ptr); \
            return (ret); \
        } \
    } while (0)

#define CHECK_ALLOC(ptr) \
    do { \
        if ((ptr) == NULL) { \
            SET_ERROR(ERR_OUT_OF_MEMORY, "Allocation failed"); \
            return NULL; \
        } \
    } while (0)

#define RETURN_IF_ERROR(expr) \
    do { \
        int _result = (expr); \
        if (_result != 0) { \
            return _result; \
        } \
    } while (0)

/* Result type pattern */
#define RESULT_OK(T)    ((result_##T##_t){ .ok = true, .value = (T){0} })
#define RESULT_ERR(T)   ((result_##T##_t){ .ok = false })

#define DEFINE_RESULT(T) \
    typedef struct { \
        bool ok; \
        T value; \
    } result_##T##_t
```

---

## Threading Patterns

### Thread Pool Implementation

```c
/*******************************************************************************
 * Thread Pool Implementation
 * POSIX threads based
 ******************************************************************************/

#include <pthread.h>
#include <stdbool.h>

typedef void (*task_func_t)(void *arg);

typedef struct task {
    task_func_t func;
    void *arg;
    struct task *next;
} task_t;

typedef struct {
    pthread_t *threads;
    size_t thread_count;
    task_t *queue_head;
    task_t *queue_tail;
    pthread_mutex_t queue_mutex;
    pthread_cond_t queue_cond;
    bool shutdown;
    size_t active_tasks;
} thread_pool_t;

/**
 * Worker thread function
 */
static void *worker_thread(void *arg) {
    thread_pool_t *pool = (thread_pool_t *)arg;

    while (true) {
        pthread_mutex_lock(&pool->queue_mutex);

        /* Wait for task or shutdown */
        while (pool->queue_head == NULL && !pool->shutdown) {
            pthread_cond_wait(&pool->queue_cond, &pool->queue_mutex);
        }

        if (pool->shutdown && pool->queue_head == NULL) {
            pthread_mutex_unlock(&pool->queue_mutex);
            break;
        }

        /* Get task from queue */
        task_t *task = pool->queue_head;
        pool->queue_head = task->next;
        if (pool->queue_head == NULL)
            pool->queue_tail = NULL;

        pool->active_tasks++;
        pthread_mutex_unlock(&pool->queue_mutex);

        /* Execute task */
        task->func(task->arg);
        free(task);

        pthread_mutex_lock(&pool->queue_mutex);
        pool->active_tasks--;
        pthread_mutex_unlock(&pool->queue_mutex);
    }

    return NULL;
}

/**
 * Create thread pool
 */
thread_pool_t *pool_create(size_t thread_count) {
    thread_pool_t *pool = malloc(sizeof(thread_pool_t));
    if (!pool) return NULL;

    pool->thread_count = thread_count;
    pool->queue_head = NULL;
    pool->queue_tail = NULL;
    pool->shutdown = false;
    pool->active_tasks = 0;

    pthread_mutex_init(&pool->queue_mutex, NULL);
    pthread_cond_init(&pool->queue_cond, NULL);

    pool->threads = malloc(sizeof(pthread_t) * thread_count);
    if (!pool->threads) {
        free(pool);
        return NULL;
    }

    for (size_t i = 0; i < thread_count; i++) {
        if (pthread_create(&pool->threads[i], NULL, worker_thread, pool) != 0) {
            /* Cleanup on failure */
            pool->shutdown = true;
            pthread_cond_broadcast(&pool->queue_cond);
            for (size_t j = 0; j < i; j++) {
                pthread_join(pool->threads[j], NULL);
            }
            free(pool->threads);
            free(pool);
            return NULL;
        }
    }

    return pool;
}

/**
 * Submit task to pool
 */
bool pool_submit(thread_pool_t *pool, task_func_t func, void *arg) {
    task_t *task = malloc(sizeof(task_t));
    if (!task) return false;

    task->func = func;
    task->arg = arg;
    task->next = NULL;

    pthread_mutex_lock(&pool->queue_mutex);

    if (pool->queue_tail)
        pool->queue_tail->next = task;
    else
        pool->queue_head = task;

    pool->queue_tail = task;

    pthread_cond_signal(&pool->queue_cond);
    pthread_mutex_unlock(&pool->queue_mutex);

    return true;
}

/**
 * Wait for all tasks to complete
 */
void pool_wait(thread_pool_t *pool) {
    pthread_mutex_lock(&pool->queue_mutex);
    while (pool->queue_head != NULL || pool->active_tasks > 0) {
        pthread_mutex_unlock(&pool->queue_mutex);
        usleep(1000);  /* 1ms sleep */
        pthread_mutex_lock(&pool->queue_mutex);
    }
    pthread_mutex_unlock(&pool->queue_mutex);
}

/**
 * Destroy thread pool
 */
void pool_destroy(thread_pool_t *pool) {
    pthread_mutex_lock(&pool->queue_mutex);
    pool->shutdown = true;
    pthread_cond_broadcast(&pool->queue_cond);
    pthread_mutex_unlock(&pool->queue_mutex);

    for (size_t i = 0; i < pool->thread_count; i++) {
        pthread_join(pool->threads[i], NULL);
    }

    /* Free remaining tasks */
    task_t *task = pool->queue_head;
    while (task) {
        task_t *next = task->next;
        free(task);
        task = next;
    }

    pthread_mutex_destroy(&pool->queue_mutex);
    pthread_cond_destroy(&pool->queue_cond);
    free(pool->threads);
    free(pool);
}
```

---

## Utility Macros

### Common Utility Macros

```c
/*******************************************************************************
 * Common Utility Macros
 ******************************************************************************/

/* Array size */
#define ARRAY_SIZE(arr)     (sizeof(arr) / sizeof((arr)[0]))

/* Min/Max */
#define MIN(a, b)           ((a) < (b) ? (a) : (b))
#define MAX(a, b)           ((a) > (b) ? (a) : (b))
#define CLAMP(x, lo, hi)    MIN(MAX(x, lo), hi)

/* Swap */
#define SWAP(a, b)          do { typeof(a) _tmp = (a); (a) = (b); (b) = _tmp; } while(0)

/* Bit manipulation */
#define BIT(n)              (1UL << (n))
#define SET_BIT(x, n)       ((x) |= BIT(n))
#define CLEAR_BIT(x, n)     ((x) &= ~BIT(n))
#define TOGGLE_BIT(x, n)    ((x) ^= BIT(n))
#define TEST_BIT(x, n)      (((x) & BIT(n)) != 0)

/* Alignment */
#define ALIGN_UP(x, align)    (((x) + (align) - 1) & ~((align) - 1))
#define ALIGN_DOWN(x, align)  ((x) & ~((align) - 1))
#define IS_ALIGNED(x, align)  (((x) & ((align) - 1)) == 0)

/* Power of 2 check */
#define IS_POWER_OF_2(x)    ((x) != 0 && ((x) & ((x) - 1)) == 0)

/* Container of */
#define CONTAINER_OF(ptr, type, member) \
    ((type *)((char *)(ptr) - offsetof(type, member)))

/* Stringify */
#define STRINGIFY(x)        #x
#define TOSTRING(x)         STRINGIFY(x)

/* Concatenation */
#define CONCAT_(a, b)       a##b
#define CONCAT(a, b)        CONCAT_(a, b)

/* Unique identifier */
#define UNIQUE_ID(prefix)   CONCAT(prefix, __LINE__)

/* Likely/Unlikely hints */
#ifdef __GNUC__
    #define LIKELY(x)       __builtin_expect(!!(x), 1)
    #define UNLIKELY(x)     __builtin_expect(!!(x), 0)
#else
    #define LIKELY(x)       (x)
    #define UNLIKELY(x)     (x)
#endif

/* Unused parameter */
#define UNUSED(x)           (void)(x)

/* Static assertion */
#define STATIC_ASSERT(expr, msg) \
    _Static_assert(expr, msg)

/* Compile-time sizeof assertion */
#define ASSERT_SIZEOF(type, size) \
    STATIC_ASSERT(sizeof(type) == (size), "Size of " #type " must be " #size)

/* Deprecation warning */
#ifdef __GNUC__
    #define DEPRECATED(msg) __attribute__((deprecated(msg)))
#else
    #define DEPRECATED(msg)
#endif
```

---

## Platform Detection

### Cross-Platform Compatibility

```c
/*******************************************************************************
 * Platform Detection and Compatibility Layer
 ******************************************************************************/

/* Operating System Detection */
#if defined(_WIN32) || defined(_WIN64)
    #define PLATFORM_WINDOWS 1
    #define PLATFORM_NAME "Windows"
#elif defined(__APPLE__) && defined(__MACH__)
    #define PLATFORM_MACOS 1
    #define PLATFORM_NAME "macOS"
#elif defined(__linux__)
    #define PLATFORM_LINUX 1
    #define PLATFORM_NAME "Linux"
#elif defined(__FreeBSD__)
    #define PLATFORM_FREEBSD 1
    #define PLATFORM_NAME "FreeBSD"
#elif defined(__unix__)
    #define PLATFORM_UNIX 1
    #define PLATFORM_NAME "Unix"
#else
    #define PLATFORM_UNKNOWN 1
    #define PLATFORM_NAME "Unknown"
#endif

/* Architecture Detection */
#if defined(__x86_64__) || defined(_M_X64)
    #define ARCH_X64 1
    #define ARCH_NAME "x86_64"
#elif defined(__i386__) || defined(_M_IX86)
    #define ARCH_X86 1
    #define ARCH_NAME "x86"
#elif defined(__aarch64__) || defined(_M_ARM64)
    #define ARCH_ARM64 1
    #define ARCH_NAME "ARM64"
#elif defined(__arm__) || defined(_M_ARM)
    #define ARCH_ARM 1
    #define ARCH_NAME "ARM"
#else
    #define ARCH_UNKNOWN 1
    #define ARCH_NAME "Unknown"
#endif

/* Compiler Detection */
#if defined(__clang__)
    #define COMPILER_CLANG 1
    #define COMPILER_NAME "Clang"
    #define COMPILER_VERSION __clang_major__
#elif defined(__GNUC__)
    #define COMPILER_GCC 1
    #define COMPILER_NAME "GCC"
    #define COMPILER_VERSION __GNUC__
#elif defined(_MSC_VER)
    #define COMPILER_MSVC 1
    #define COMPILER_NAME "MSVC"
    #define COMPILER_VERSION _MSC_VER
#else
    #define COMPILER_UNKNOWN 1
    #define COMPILER_NAME "Unknown"
    #define COMPILER_VERSION 0
#endif

/* Platform-specific includes */
#ifdef PLATFORM_WINDOWS
    #define WIN32_LEAN_AND_MEAN
    #include <windows.h>
    #define PATH_SEPARATOR '\\'
    #define PATH_SEPARATOR_STR "\\"
#else
    #include <unistd.h>
    #include <sys/types.h>
    #define PATH_SEPARATOR '/'
    #define PATH_SEPARATOR_STR "/"
#endif

/* Platform-specific sleep */
static inline void platform_sleep_ms(unsigned int ms) {
#ifdef PLATFORM_WINDOWS
    Sleep(ms);
#else
    usleep(ms * 1000);
#endif
}

/* Platform-specific thread ID */
static inline unsigned long platform_thread_id(void) {
#ifdef PLATFORM_WINDOWS
    return (unsigned long)GetCurrentThreadId();
#elif defined(PLATFORM_LINUX)
    return (unsigned long)syscall(SYS_gettid);
#else
    return (unsigned long)pthread_self();
#endif
}

/* Print platform info */
static inline void print_platform_info(void) {
    printf("Platform: %s\n", PLATFORM_NAME);
    printf("Architecture: %s\n", ARCH_NAME);
    printf("Compiler: %s %d\n", COMPILER_NAME, COMPILER_VERSION);
    printf("Pointer size: %zu bytes\n", sizeof(void*));
}
```

---

*ARCHAEON Arsenal - C Division*
*"C is not a big language, and it is not well served by a big book"*
