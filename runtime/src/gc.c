#include "coal/gc.h"
#include <stdlib.h>
#include <gc.h>

void
rt_gc_init(void)
{
    GC_INIT();
}

void *
rt_alloc(size_t size)
{
    return GC_malloc(size);
}

void *
rt_alloc_atomic(size_t size)
{
    return GC_malloc_atomic(size);
}
