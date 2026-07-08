#include "coal/panic.h"
#include <stdio.h>
#include <stdlib.h>

_Noreturn void
rt_panic(const char *msg)
{
    fprintf(stderr, "PANIC: %s\n", msg);
    abort();
}

_Noreturn void
rt_unreachable(void)
{
    fprintf(stderr, "PANIC: unreachable code reached\n");
    abort();
}
