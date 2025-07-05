// #define GC_DEBUG

#include <gc.h>
#include <gmp.h>
#include <inttypes.h>
#include <locale.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <wchar.h>

#include "hashmap/hashmap.h"

/*
 * ////////////////////////////////////////////////////////////////////////////
 * Boehm–Demers–Weiser garbage collector
 *
 * https://github.com/ivmai/bdwgc
 */

void
gc_init()
{
  GC_INIT();
}

void
gc_collect()
{
  GC_gcollect();
}

void*
gc_malloc(long size)
{
  return GC_MALLOC(size);
}

static void
gc_finalizer(void* obj, void* client_data)
{
  static int count = 1;

  printf("free: %d\n", count++);
}

/*
 * ////////////////////////////////////////////////////////////////////////////
 * sheredom/hashmap.h
 *
 * https://github.com/sheredom/hashmap.h
 */

void*
hashmap_init()
{
  void* p = gc_malloc(sizeof(struct hashmap_s));
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

/*
 * ////////////////////////////////////////////////////////////////////////////
 * Bignums
 *
 * https://gmplib.org/
 */

void*
bignum_init(char* str)
{
  mpz_t* big_int = (mpz_t*)gc_malloc(sizeof(mpz_t));

  mpz_init(*big_int);

  if (mpz_set_str(*big_int, str, 10) != 0) {
    return NULL;
  }

  return (void*)big_int;
}

/*
 * ////////////////////////////////////////////////////////////////////////////
 * Various I/O
 *
 */

void
print_bool(bool b)
{
  printf("%s", b ? "true" : "false");
}

void
print_char(wchar_t ch)
{
  setlocale(LC_ALL, "");

  wprintf(L"%lc\n", ch);
}

void
print_string(const char* str)
{
  setlocale(LC_ALL, "");

  const size_t len = mbstowcs(NULL, str, 0) + 1;
  wchar_t* wstr = malloc(len * sizeof(wchar_t));

  mbstowcs(wstr, str, len);
  wprintf(L"%ls\n", wstr);

  free(wstr);
}

void
print_float(float f)
{
  printf("%f\n", f);
}

void
print_double(double d)
{
  printf("%f\n", d);
}

void
print_int32(int32_t n)
{
  printf("%" PRId32 "\n", n);
}

void
print_int64(int64_t n)
{
  printf("%" PRId64 "\n", n);
}

void
print_bignum(mpz_t* big_int)
{
  gmp_printf("%Zd\n", *big_int);
}

/*
 * ////////////////////////////////////////////////////////////////////////////
 * String utilities
 *
 */

char*
int32_to_string(int32_t value)
{
  const size_t buffer_size = 12;

  char* result = GC_malloc(buffer_size);
  if (!result)
    return NULL;

  snprintf(result, buffer_size, "%d", value);

  return result;
}

char*
string_concat(const char* a, const char* b)
{
  if (a == NULL)
    a = "";
  if (b == NULL)
    b = "";

  size_t len_a = strlen(a);
  size_t len_b = strlen(b);

  char* result = GC_malloc(len_a + len_b + 1);

  memcpy(result, a, len_a);
  memcpy(result + len_a, b, len_b);
  result[len_a + len_b] = '\0';

  return result;
}
