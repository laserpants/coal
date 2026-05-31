#ifndef COAL_RUNTIME_H
#define COAL_RUNTIME_H

#include <stdbool.h>

/**
 * Initialize the Coal runtime system.
 * Must be called before using any other runtime functions.
 */
void rt_runtime_init(void);

/**
 * Generate a random float in the range [0.0, 1.0).
 *
 * Returns:
 *   Random float value
 */
float rt_float_random(void);

/**
 * Generate a random double in the range [0.0, 1.0).
 *
 * Returns:
 *   Random double value
 */
double rt_double_random(void);

/**
 * Check if a pointer is NULL.
 *
 * Parameters:
 *   ptr - Pointer to check
 *
 * Returns:
 *   true if ptr is NULL, false otherwise
 */
bool rt_is_null(void *ptr);

#endif
