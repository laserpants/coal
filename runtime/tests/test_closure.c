#include "coal/closure.h"
#include "coal/runtime.h"
#include <stdio.h>
#include <assert.h>

static void
dummy_fn(void)
{
    /* Test function */
}

static void
test_closure_new(void)
{
    rt_closure_t *closure = rt_closure_new((void *) dummy_fn, 3);
    assert(closure != NULL);
    printf("test_closure_new: PASS\n");
}

static void
test_closure_extend(void)
{
    rt_closure_t *closure = rt_closure_new((void *) dummy_fn, 3);

    void *args[2] = {(void *) 1, (void *) 2};
    rt_closure_t *extended = rt_closure_extend(closure, 2, args);

    assert(extended != NULL);
    printf("test_closure_extend: PASS\n");
}

int
main(void)
{
    rt_runtime_init();

    printf("Running closure tests...\n");
    test_closure_new();
    test_closure_extend();
    printf("All closure tests passed!\n");

    return 0;
}
