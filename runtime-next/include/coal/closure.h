#ifndef COAL_CLOSURE_H
#define COAL_CLOSURE_H

#include <stdint.h>

/**
 * Closure structure containing a function pointer and captured arguments.
 */
typedef struct rt_closure {
    int32_t captured;  /** Number of captured arguments */
    int32_t remaining; /** Number of arguments still needed */
    void *fn;          /** Function pointer */
    void *args[];      /** Flexible array of captured argument values */
} rt_closure_t;

/**
 * Create a new closure.
 *
 * Parameters:
 *   fn - Function pointer
 *   arity - Total number of arguments the function expects
 *
 * Returns:
 *   New closure with no captured arguments
 */
rt_closure_t *rt_closure_new(void *fn, int32_t arity);

/**
 * Extend a closure with additional arguments.
 *
 * Parameters:
 *   closure - Existing closure
 *   argc - Number of arguments to add
 *   args - Array of argument values
 *
 * Returns:
 *   New closure with additional captured arguments
 */
rt_closure_t *rt_closure_extend(rt_closure_t *closure, int32_t argc,
                                void **args);

#endif
