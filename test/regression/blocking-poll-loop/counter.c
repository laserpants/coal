#include <stdint.h>

// Regression fixture for Runtime.blocking_poll (see README.md).
//
// counter_bump() returns 0 for the first LIMIT - 1 calls, and a non-zero value
// from the LIMIT-th call onwards. It makes the fixture's `try_fn` yield `None`
// until the limit and `Some(n)` afterwards. Because the fixture's `block_fn`
// never blocks (it returns immediately), a recursive blocking_poll exhausts
// the C stack long before LIMIT; a constant-stack loop reaches it.
#define LIMIT 1000000

static int32_t g_calls = 0;

int32_t counter_bump(void)
{
    g_calls++;
    return g_calls >= LIMIT ? g_calls : 0;
}
