#ifndef COAL_APPLY_H
#define COAL_APPLY_H

#include <stdint.h>

/**
 * Apply arguments to a closure.
 * Handles partial application, exact application, and over-application.
 *
 * Parameters:
 *   closure - Closure to apply arguments to
 *   argc - Number of arguments
 *   args - Array of argument values
 *
 * Returns:
 *   Result value (or new closure for partial application)
 */
void *rt_apply(void *closure, int32_t argc, void **args);

#endif
