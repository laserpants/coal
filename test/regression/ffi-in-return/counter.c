#include <stdint.h>

// Number of times counter_mk has run since the last reset. A poll primitive
// wrapping this call in `return(...)` must run it exactly once, so a single
// poll must leave the counter at 1.
static int32_t g_calls = 0;

int32_t counter_mk(void *_unit)
{
    (void)_unit;
    g_calls++;
    return g_calls;
}

int32_t counter_reset(void *_unit)
{
    (void)_unit;
    g_calls = 0;
    return 0;
}

int32_t counter_get(void *_unit)
{
    (void)_unit;
    return g_calls;
}
