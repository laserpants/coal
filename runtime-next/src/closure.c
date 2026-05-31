#include "coal/closure.h"
#include "coal/gc.h"
#include "coal/panic.h"
#include <stdlib.h>
#include <stdint.h>
#include <stddef.h>
#include <string.h>

/* Verify flexible array member doesn't add to struct size */
_Static_assert(sizeof(rt_closure_t) ==
                   sizeof(int32_t) + sizeof(int32_t) + sizeof(void *),
               "Flexible array should not add to struct size");

rt_closure_t *
rt_closure_new(void *fn, int32_t arity)
{
    /* Check for overflow: arity * sizeof(void*) */
    if (arity < 0 ||
        (size_t) arity > (SIZE_MAX - sizeof(rt_closure_t)) / sizeof(void *)) {
        rt_panic("Closure arity too large");
    }

    size_t size = sizeof(rt_closure_t) + ((size_t) arity * sizeof(void *));
    rt_closure_t *closure = rt_alloc(size);
    if (!closure) {
        rt_panic("Out of memory in rt_closure_new");
    }

    closure->fn = fn;
    closure->captured = 0;
    closure->remaining = arity;

    return closure;
}

rt_closure_t *
rt_closure_extend(rt_closure_t *closure, int32_t argc, void **args)
{
    if (closure == NULL || args == NULL || argc <= 0) {
        rt_panic("Invalid closure_extend parameters");
    }

    int32_t new_captured = closure->captured + argc;
    int32_t new_remaining = closure->remaining - argc;

    /* Check for overflow: new_captured * sizeof(void*) */
    if (new_captured < 0 ||
        (size_t) new_captured >
            (SIZE_MAX - sizeof(rt_closure_t)) / sizeof(void *)) {
        rt_panic("Closure size too large");
    }

    size_t size =
        sizeof(rt_closure_t) + ((size_t) new_captured * sizeof(void *));
    rt_closure_t *new_closure = rt_alloc(size);
    if (!new_closure) {
        rt_panic("Out of memory in rt_closure_extend");
    }

    new_closure->fn = closure->fn;
    new_closure->captured = new_captured;
    new_closure->remaining = new_remaining;

    /* Copy existing args */
    memcpy(new_closure->args, closure->args,
           (size_t) closure->captured * sizeof(void *));

    /* Copy new args */
    memcpy(new_closure->args + closure->captured, args,
           (size_t) argc * sizeof(void *));

    return new_closure;
}
