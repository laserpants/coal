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

#include "hashmap.h"

void*
hashmap_init()
{
  void* p = GC_MALLOC(sizeof(struct hashmap_s));
  hashmap_create(2, (struct hashmap_s*)p);

  return (void*)p;
}

void*
hashmap_insert(void* ptr, char* key, void* value)
{
  hashmap_put((struct hashmap_s*)ptr, key, strlen(key), value);

  return ptr;
}

void*
hashmap_lookup(void* ptr, char* key)
{
  return hashmap_get((struct hashmap_s*)ptr, key, strlen(key));
}

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
