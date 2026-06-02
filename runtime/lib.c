// #define GC_DEBUG

#include <errno.h>
#include <gc.h>
#include <gmp.h>
#include <inttypes.h>
#include <locale.h>
#include <stdbool.h>
#include <stddef.h> // for ptrdiff_t
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <wchar.h>

void*
exit_failure(void)
{
  exit(EXIT_FAILURE);
}

void*
debug_call_n_bounds(int32_t argn)
{
  fprintf(
    stderr, "DEBUG: call_n called with argN = %d (max allowed = 256)\n", argn);
  return exit_failure();
}
