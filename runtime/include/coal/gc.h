#ifndef COAL_GC_H
#define COAL_GC_H

#include <stddef.h>

/**
 * Initialize the garbage collector.
 * Called automatically by rt_runtime_init().
 */
void rt_gc_init(void);

/**
 * Allocate memory that may contain pointers.
 * Memory is garbage collected automatically.
 *
 * Parameters:
 *   size - Number of bytes to allocate
 *
 * Returns:
 *   Pointer to allocated memory
 */
void *rt_alloc(size_t size);

/**
 * Allocate memory that does not contain pointers.
 * Used for strings, numbers, and other atomic data.
 *
 * Parameters:
 *   size - Number of bytes to allocate
 *
 * Returns:
 *   Pointer to allocated memory
 */
void *rt_alloc_atomic(size_t size);

#endif
