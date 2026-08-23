#include <stdint.h>

// Number of times counter_mk has been called (= number of times the
// let-bound RHS was evaluated since the last reset).
static int32_t g_calls = 0;

int32_t counter_mk(void)
{
    g_calls++;
    return g_calls;
}

int32_t counter_reset(void)
{
    g_calls = 0;
    return 0;
}

int32_t counter_get(void)
{
    return g_calls;
}