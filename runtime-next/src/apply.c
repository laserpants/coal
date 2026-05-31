#include "coal/apply.h"
#include "coal/closure.h"
#include "coal/gc.h"
#include "coal/panic.h"
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef void *(*rt_fn_ptr_t)(void **args);

void *
rt_apply(void *closure_ptr, int32_t argc, void **args)
{
    rt_closure_t *closure = closure_ptr;

    if (argc == closure->remaining) {
        /* Exact application: call the function with all arguments */
        int32_t total_args = closure->captured + argc;

        /* Allocate temporary array for all arguments */
        void **all_args = rt_alloc((size_t) total_args * sizeof(void *));
        if (!all_args) {
            rt_panic("Out of memory in rt_apply");
        }

        /* Copy captured arguments */
        memcpy(all_args, closure->args,
               (size_t) closure->captured * sizeof(void *));

        /* Copy new arguments */
        memcpy(all_args + closure->captured, args,
               (size_t) argc * sizeof(void *));

        /* Call the function */
        rt_fn_ptr_t fn = (rt_fn_ptr_t) closure->fn;
        return fn(all_args);
    } else if (argc < closure->remaining) {
        /* Partial application: extend the closure with more arguments */
        return rt_closure_extend(closure, argc, args);
    } else {
        /* Over-application: apply enough args to satisfy, then apply rest to
         * result */
        int32_t needed = closure->remaining;

        /* Create array for first application */
        void **first_args = args;

        /* Apply exactly enough arguments */
        void *result = rt_apply(closure_ptr, needed, first_args);

        /* Apply remaining arguments to the result */
        int32_t remaining_argc = argc - needed;
        void **remaining_args = args + needed;

        return rt_apply(result, remaining_argc, remaining_args);
    }
}
