#include "coal/gc.h"
#include "coal/runtime.h"
#include <stdio.h>
#include <assert.h>

static void
test_gc_init(void)
{
    rt_gc_init();
    printf("test_gc_init: PASS\n");
}

static void
test_gc_alloc(void)
{
    void *ptr = rt_alloc(1024);
    assert(ptr != NULL);
    printf("test_gc_alloc: PASS\n");
}

static void
test_gc_alloc_atomic(void)
{
    void *ptr = rt_alloc_atomic(512);
    assert(ptr != NULL);
    printf("test_gc_alloc_atomic: PASS\n");
}

int
main(void)
{
    rt_runtime_init();

    printf("Running GC tests...\n");
    test_gc_init();
    test_gc_alloc();
    test_gc_alloc_atomic();
    printf("All GC tests passed!\n");

    return 0;
}
