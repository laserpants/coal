#include "coal/runtime.h"
#include "coal/gc.h"
#include <stdlib.h>
#include <time.h>

void
rt_runtime_init(void)
{
    rt_gc_init();

    /* Seed the random number generator with current time */
    srand((unsigned) time(NULL));
}

float
rt_float_random(void)
{
    return (float) rand() / (float) RAND_MAX;
}

double
rt_double_random(void)
{
    return (double) rand() / (double) RAND_MAX;
}

bool
rt_is_null(void *ptr)
{
    return (ptr == NULL);
}
