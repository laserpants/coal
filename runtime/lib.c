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

int32_t
string_length(const char* s)
{
  int32_t len = 0;
  while (*s) 
  {
    // If the byte is not a continuation byte, it's the start of a new character
    if ((unsigned char)(*s) >> 6 != 0b10)
      len++;
    s++;
  }
  return len;
}

wchar_t
string_head(const char* s)
{
    const unsigned char *us = (const unsigned char *)s;
    wchar_t codepoint = 0;

    if (us[0] < 0x80) {
        return us[0];
    } else if ((us[0] & 0xE0) == 0xC0) {
        codepoint = ((us[0] & 0x1F) << 6) | (us[1] & 0x3F);
    } else if ((us[0] & 0xF0) == 0xE0) {
        codepoint = ((us[0] & 0x0F) << 12) | ((us[1] & 0x3F) << 6) | (us[2] & 0x3F);
    } else if ((us[0] & 0xF8) == 0xF0) {
        codepoint = ((us[0] & 0x07) << 18) | ((us[1] & 0x3F) << 12) |
                    ((us[2] & 0x3F) << 6) | (us[3] & 0x3F);
    }

    return codepoint;
}

char*
string_tail(const char* s)
{
    const unsigned char *us = (const unsigned char *)s;
    size_t skip = 0;

    if (us[0] < 0x80) {
        skip = 1;
    } else if ((us[0] & 0xE0) == 0xC0) {
        skip = 2;
    } else if ((us[0] & 0xF0) == 0xE0) {
        skip = 3;
    } else if ((us[0] & 0xF8) == 0xF0) {
        skip = 4;
    }

    // Return a newly allocated copy of the remaining string
    size_t len = strlen(s + skip);
    char* tail = (char*)malloc(len + 1);
    if (tail != NULL) {
        memcpy(tail, s + skip, len + 1);  // include null terminator
    }

    return tail;
}

char*
string_reverse(const char* s)
{
    const unsigned char* us = (const unsigned char*)s;
    size_t len = strlen(s);

    // Step 1: Collect UTF-8 character slices
    const char** chars = (const char**)malloc(len * sizeof(char*));  // over-allocating
    size_t* char_lens = (size_t*)malloc(len * sizeof(size_t));

    if (!chars || !char_lens) {
        free(chars);
        free(char_lens);
        return NULL;
    }

    size_t char_count = 0;
    size_t i = 0;

    while (i < len) {
        size_t char_len = 1;
        unsigned char c = us[i];
        if ((c & 0x80) == 0x00)        char_len = 1;
        else if ((c & 0xE0) == 0xC0)   char_len = 2;
        else if ((c & 0xF0) == 0xE0)   char_len = 3;
        else if ((c & 0xF8) == 0xF0)   char_len = 4;

        if (i + char_len > len) break; // malformed?

        chars[char_count] = (const char*)(us + i);
        char_lens[char_count] = char_len;
        char_count++;
        i += char_len;
    }

    // Step 2: Compute total size for result
    size_t total_bytes = 0;

    for (size_t j = 0; j < char_count; ++j) {
        total_bytes += char_lens[j];
    }

    char* result = (char*)malloc(total_bytes + 1);

    if (!result) {
        free(chars);
        free(char_lens);
        return NULL;
    }

    // Step 3: Copy characters in reverse order
    size_t out_i = 0;

    for (ssize_t j = (ssize_t)char_count - 1; j >= 0; --j) {
        memcpy(result + out_i, chars[j], char_lens[j]);
        out_i += char_lens[j];
    }

    result[total_bytes] = '\0';

    free(chars);
    free(char_lens);

    return result;
}
