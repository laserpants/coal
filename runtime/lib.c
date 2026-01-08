// #define GC_DEBUG

#include <errno.h>
#include <gc.h>
#include <gmp.h>
#include <inttypes.h>
#include <locale.h>
#include <stdbool.h>
#include <stddef.h> // for ptrdiff_t
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <wchar.h>

#include "hashmap.h"

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

/* add two bignums, return a newly-allocated mpz_t* holding m + n */
mpz_t*
bignum_add(mpz_t* m, mpz_t* n)
{
  if (!m || !n)
    return NULL;

  mpz_t* res = (mpz_t*)gc_malloc(sizeof(mpz_t));
  if (!res)
    return NULL;

  mpz_init(*res);
  mpz_add(*res, *m, *n);
  return res;
}

/* subtract two bignums, return a newly-allocated mpz_t* holding m - n */
mpz_t*
bignum_sub(mpz_t* m, mpz_t* n)
{
  if (!m || !n)
    return NULL;

  mpz_t* res = (mpz_t*)gc_malloc(sizeof(mpz_t));
  if (!res)
    return NULL;

  mpz_init(*res);
  mpz_sub(*res, *m, *n);
  return res;
}

/* multiply two bignums, return a newly-allocated mpz_t* holding m * n */
mpz_t*
bignum_mul(mpz_t* m, mpz_t* n)
{
  if (!m || !n)
    return NULL;

  mpz_t* res = (mpz_t*)gc_malloc(sizeof(mpz_t));
  if (!res)
    return NULL;

  mpz_init(*res);
  mpz_mul(*res, *m, *n);
  return res;
}

/* negate a bignum, return a newly-allocated mpz_t* holding -m */
mpz_t*
bignum_neg(mpz_t* m)
{
  if (!m)
    return NULL;

  mpz_t* res = (mpz_t*)gc_malloc(sizeof(mpz_t));
  if (!res)
    return NULL;

  mpz_init(*res);
  mpz_neg(*res, *m);
  return res;
}

/* compute m % n, return a newly-allocated mpz_t* */
mpz_t*
bignum_mod(mpz_t* m, mpz_t* n)
{
  if (!m || !n)
    return NULL;

  /* modulo by zero is undefined */
  if (mpz_sgn(*n) == 0)
    return NULL;

  mpz_t* res = (mpz_t*)gc_malloc(sizeof(mpz_t));
  if (!res)
    return NULL;

  mpz_init(*res);
  mpz_mod(*res, *m, *n);
  return res;
}

bool
bignum_lt(mpz_t* m, mpz_t* n)
{
  if (!m || !n)
    return NULL;

  return mpz_cmp(*m, *n) < 0;
}

bool
bignum_gt(mpz_t* m, mpz_t* n)
{
  if (!m || !n)
    return NULL;

  return mpz_cmp(*m, *n) > 0;
}

bool
bignum_eq(mpz_t* m, mpz_t* n)
{
  if (!m || !n)
    return NULL;

  return mpz_cmp(*m, *n) == 0;
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
println_bool(bool b)
{
  printf("%s\n", b ? "true" : "false");
}

void
print_char(uint32_t cp)
{
  unsigned char utf8[5] = { 0 };
  int len = 0;
  if (cp <= 0x7F) {
    utf8[0] = (unsigned char)cp;
    len = 1;
  } else if (cp <= 0x7FF) {
    utf8[0] = 0xC0 | (cp >> 6);
    utf8[1] = 0x80 | (cp & 0x3F);
    len = 2;
  } else if (cp <= 0xFFFF) {
    utf8[0] = 0xE0 | (cp >> 12);
    utf8[1] = 0x80 | ((cp >> 6) & 0x3F);
    utf8[2] = 0x80 | (cp & 0x3F);
    len = 3;
  } else if (cp <= 0x10FFFF) {
    utf8[0] = 0xF0 | (cp >> 18);
    utf8[1] = 0x80 | ((cp >> 12) & 0x3F);
    utf8[2] = 0x80 | ((cp >> 6) & 0x3F);
    utf8[3] = 0x80 | (cp & 0x3F);
    len = 4;
  }
  fwrite(utf8, 1, len, stdout);
}

void
println_char(uint32_t cp)
{
  print_char(cp);
  putchar('\n');
}

void
print_string(const char* str)
{
  if (!str)
    return;

  fputs(str, stdout);
}

void
println_string(const char* str)
{
  print_string(str);
  putchar('\n');
}

void
print_float(float f)
{
  printf("%f", f);
}

void
println_float(float f)
{
  printf("%f\n", f);
}

void
print_double(double d)
{
  printf("%.15f", d);
}

void
println_double(double d)
{
  printf("%.15f\n", d);
}

void
print_int32(int32_t n)
{
  printf("%" PRId32, n);
}

void
println_int32(int32_t n)
{
  printf("%" PRId32 "\n", n);
}

void
print_int64(int64_t n)
{
  printf("%" PRId64, n);
}

void
println_int64(int64_t n)
{
  printf("%" PRId64 "\n", n);
}

void
print_bignum(mpz_t* big_int)
{
  gmp_printf("%Zd", *big_int);
}

void
println_bignum(mpz_t* big_int)
{
  gmp_printf("%Zd\n", *big_int);
}

#define READ_STATUS_OK 0
#define READ_STATUS_FILE_NOT_FOUND 1
#define READ_STATUS_IO_ERROR 2
#define READ_STATUS_OUT_OF_MEMORY 3

typedef struct
{
  int32_t status; /* 0 = OK, non-zero = error */
  void* value;    /* valid iff status == 0 */
} result_t;

int32_t
result_status(result_t* r)
{
  return r->status;
}

char*
result_value(result_t* r)
{
  return (char*)r->value;
}

result_t*
read_file(const char* filename)
{
  result_t* res = gc_malloc(sizeof(result_t));
  if (!res) {
    /* Failure: runtime out of memory */
    return NULL;
  }

  FILE* file = fopen(filename, "rb");
  if (!file) {
    res->value = NULL;
    if (errno == ENOENT) {
      res->status = READ_STATUS_FILE_NOT_FOUND;
    } else {
      res->status = READ_STATUS_IO_ERROR;
    }
    return res;
  }

  /* Move to end to determine file size */
  if (fseek(file, 0, SEEK_END) != 0) {
    fclose(file);
    res->status = READ_STATUS_IO_ERROR;
    res->value = NULL;
    return res;
  }

  long length = ftell(file);
  if (length < 0) {
    fclose(file);
    res->status = READ_STATUS_IO_ERROR;
    res->value = NULL;
    return res;
  }

  rewind(file);

  /* Allocate buffer (+1 for null terminator) */
  char* buffer = gc_malloc((size_t)length + 1);
  if (!buffer) {
    fclose(file);
    res->status = READ_STATUS_OUT_OF_MEMORY;
    res->value = NULL;
    return res;
  }

  /* Read file contents */
  size_t read_size = fread(buffer, 1, (size_t)length, file);
  fclose(file);

  if (read_size != (size_t)length) {
    res->status = READ_STATUS_IO_ERROR;
    res->value = NULL;
    return res;
  }

  buffer[length] = '\0';

  /* Success */
  res->status = READ_STATUS_OK;
  res->value = buffer;
  return res;
}

int
write_file(const char* filename, const char* data)
{
  if (!filename || !data) {
    return -1;
  }

  FILE* file = fopen(filename, "wb");
  if (!file) {
    return -1;
  }

  size_t length = strlen(data);
  size_t written = fwrite(data, 1, length, file);

  if (written != length) {
    fclose(file);
    return -1;
  }

  fclose(file);

  return 0;
}

char*
readln(void)
{
  size_t capacity = 128;
  size_t length = 0;

  char* buffer = gc_malloc(capacity);
  if (!buffer) {
    return NULL;
  }

  int c;
  while ((c = fgetc(stdin)) != EOF) {
    if (c == '\n') {
      break;
    }

    // Grow buffer if needed
    if (length + 1 >= capacity) {
      size_t new_capacity = capacity * 2;
      char* new_buffer = gc_malloc(new_capacity);
      if (!new_buffer) {
        return NULL;
      }
      memcpy(new_buffer, buffer, length);
      buffer = new_buffer;
      capacity = new_capacity;
    }

    buffer[length++] = (char)c;
  }

  // If nothing was read and we hit EOF, return NULL
  if (length == 0 && c == EOF) {
    return NULL;
  }

  // Null-terminate
  buffer[length] = '\0';

  return buffer;
}

/*
 * ////////////////////////////////////////////////////////////////////////////
 * Type conversions
 *
 */

int32_t
float_to_int32(float f)
{
  int32_t n = (int32_t)f;

  return n;
}

int32_t
double_to_int32(double d)
{
  int32_t n = (int32_t)d;

  return n;
}

int64_t
float_to_int64(float f)
{
  int64_t n = (int64_t)f;

  return n;
}

int64_t
double_to_int64(double d)
{
  int64_t n = (int64_t)d;

  return n;
}

float*
int32_to_float(int32_t n)
{
  float* p = gc_malloc(sizeof(float));

  *p = (float)n;

  return p;
}

double*
int32_to_double(int32_t n)
{
  double* p = gc_malloc(sizeof(double));

  *p = (double)n;

  return p;
}

float*
int64_to_float(int64_t n)
{
  float* p = gc_malloc(sizeof(float));

  *p = (float)n;

  return p;
}

double*
int64_to_double(int64_t n)
{
  double* p = gc_malloc(sizeof(double));

  *p = (double)n;

  return p;
}

mpz_t*
int32_to_bignum(int32_t n)
{
  mpz_t* result = (mpz_t*)gc_malloc(sizeof(mpz_t));
  if (!result)
    return NULL;

  mpz_init(*result);
  mpz_set_si(*result, (long)n); /* mpz_set_si handles negative values too */
  return result;
}

mpz_t*
int64_to_bignum(int64_t n)
{
  mpz_t* result = (mpz_t*)gc_malloc(sizeof(mpz_t));
  if (!result)
    return NULL;

  mpz_init(*result);
  mpz_set_si(*result, (long)n);
  return result;
}

int32_t
bignum_to_int32(const mpz_t* n)
{
  return (int32_t)mpz_get_si(*n);
}

int64_t
bignum_to_int64(const mpz_t* n)
{
  return (int64_t)mpz_get_si(*n);
}

float*
bignum_to_float(const mpz_t* n)
{
  float* result = (float*)gc_malloc(sizeof(float));
  if (!result)
    return NULL;

  *result = (float)mpz_get_d(*n);
  return result;
}

double*
bignum_to_double(const mpz_t* n)
{
  double* result = (double*)gc_malloc(sizeof(double));
  if (!result)
    return NULL;

  *result = mpz_get_d(*n);
  return result;
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

  char* result = gc_malloc(buffer_size);
  if (!result)
    return NULL;

  snprintf(result, buffer_size, "%d", value);

  return result;
}

char*
float_to_string(float value)
{
  const size_t buffer_size = 32; // enough to hold float with precision

  char* result = gc_malloc(buffer_size);

  if (!result)
    return NULL;

  snprintf(result, buffer_size, "%g", value);

  return result;
}

char*
double_to_string(double value)
{
  const size_t buffer_size = 64; // enough to hold double with precision

  char* result = gc_malloc(buffer_size);

  if (!result)
    return NULL;

  snprintf(result, buffer_size, "%g", value);

  return result;
}

char*
char_to_string(uint32_t cp)
{
  char* out;

  if (cp <= 0x7F) {
    // 1-byte sequence
    out = gc_malloc(2);
    out[0] = (char)cp;
    out[1] = '\0';
  } else if (cp <= 0x7FF) {
    // 2-byte sequence
    out = gc_malloc(3);
    out[0] = (char)(0xC0 | ((cp >> 6) & 0x1F));
    out[1] = (char)(0x80 | (cp & 0x3F));
    out[2] = '\0';
  } else if (cp <= 0xFFFF) {
    // Exclude UTF-16 surrogate range (invalid in UTF-8)
    if (cp >= 0xD800 && cp <= 0xDFFF)
      return NULL;

    out = gc_malloc(4);
    out[0] = (char)(0xE0 | ((cp >> 12) & 0x0F));
    out[1] = (char)(0x80 | ((cp >> 6) & 0x3F));
    out[2] = (char)(0x80 | (cp & 0x3F));
    out[3] = '\0';
  } else if (cp <= 0x10FFFF) {
    // 4-byte sequence
    out = gc_malloc(5);
    out[0] = (char)(0xF0 | ((cp >> 18) & 0x07));
    out[1] = (char)(0x80 | ((cp >> 12) & 0x3F));
    out[2] = (char)(0x80 | ((cp >> 6) & 0x3F));
    out[3] = (char)(0x80 | (cp & 0x3F));
    out[4] = '\0';
  } else {
    // Out of Unicode range
    return NULL;
  }

  return out;
}

char*
bool_to_string(bool b)
{
  return b ? "true" : "false";
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

  char* result = gc_malloc(len_a + len_b + 1);

  memcpy(result, a, len_a);
  memcpy(result + len_a, b, len_b);
  result[len_a + len_b] = '\0';

  return result;
}

int32_t
string_length(const char* s)
{
  int32_t len = 0;
  while (*s) {
    // If the byte is not a continuation byte, it's the start of a new character
    if ((unsigned char)(*s) >> 6 != 0b10)
      len++;
    s++;
  }
  return len;
}

uint32_t
string_head(const char* s)
{
  const unsigned char* us = (const unsigned char*)s;
  uint32_t codepoint = 0;
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
  const unsigned char* us = (const unsigned char*)s;
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
  char* tail = (char*)gc_malloc(len + 1);
  if (tail != NULL) {
    memcpy(tail, s + skip, len + 1); // include null terminator
  }
  return tail;
}

char*
string_reverse(const char* s)
{
  const unsigned char* us = (const unsigned char*)s;
  size_t len = strlen(s);

  // Step 1: Collect UTF-8 character slices
  const char** chars =
    (const char**)malloc(len * sizeof(char*)); // over-allocating
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
    if ((c & 0x80) == 0x00)
      char_len = 1;
    else if ((c & 0xE0) == 0xC0)
      char_len = 2;
    else if ((c & 0xF0) == 0xE0)
      char_len = 3;
    else if ((c & 0xF8) == 0xF0)
      char_len = 4;

    if (i + char_len > len)
      break; // malformed?

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

  char* result = (char*)gc_malloc(total_bytes + 1);

  if (!result) {
    free(chars);
    free(char_lens);
    return NULL;
  }

  // Step 3: Copy characters in reverse order
  size_t out_i = 0;

  for (ptrdiff_t j = (ptrdiff_t)char_count - 1; j >= 0; --j) {
    memcpy(result + out_i, chars[j], char_lens[j]);
    out_i += char_lens[j];
  }

  result[total_bytes] = '\0';

  free(chars);
  free(char_lens);

  return result;
}

// Simple check for common Unicode whitespace characters in UTF-8
static bool
is_utf8_whitespace(const unsigned char* s, size_t* char_len)
{
  unsigned char c = s[0];

  if ((c & 0x80) == 0x00) {
    // ASCII range
    *char_len = 1;
    return c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\v' ||
           c == '\f';
  } else if ((c & 0xE0) == 0xC0) {
    // 2-byte UTF-8
    *char_len = 2;
    if (c == 0xC2 && (s[1] == 0xA0))
      return true; // Non-breaking space (U+00A0)
  } else if ((c & 0xF0) == 0xE0) {
    *char_len = 3;
    if (c == 0xE1 && s[1] == 0x9A && s[2] == 0x80)
      return true; // OGHAM SPACE MARK (U+1680)
    if (c == 0xE2) {
      // U+2000 to U+200A
      if (s[1] == 0x80 && (s[2] >= 0x80 && s[2] <= 0x8A))
        return true;
      if (s[1] == 0x80 && s[2] == 0xA8)
        return true; // Hair space (U+200A)
      if (s[1] == 0x80 && s[2] == 0xA9)
        return true; // Narrow no-break space (U+202F)
      if (s[1] == 0x81 && s[2] == 0x9F)
        return true; // Medium mathematical space (U+205F)
    }
  } else if ((c & 0xF8) == 0xF0) {
    *char_len = 4;
    if (c == 0xF0 && s[1] == 0x9F && s[2] == 0xA4 && s[3] == 0x8F)
      return true; // Emoji or extended whitespace (rare)
  }

  // Not a known whitespace
  return false;
}

char*
string_remove_whitespace(const char* s)
{
  const unsigned char* us = (const unsigned char*)s;
  size_t len = strlen(s);

  // Allocate output buffer same size as input (worst case)
  char* result = (char*)gc_malloc(len + 1);
  if (!result)
    return NULL;

  size_t i = 0;
  size_t out_i = 0;

  while (i < len) {
    size_t char_len = 1;
    if (is_utf8_whitespace(us + i, &char_len)) {
      i += char_len; // Skip whitespace
    } else {
      memcpy(result + out_i, us + i, char_len);
      i += char_len;
      out_i += char_len;
    }
  }

  result[out_i] = '\0';
  return result;
}

static inline int
utf8_char_len(unsigned char c)
{
  if (c < 0x80)
    return 1;
  else if ((c & 0xE0) == 0xC0)
    return 2;
  else if ((c & 0xF0) == 0xE0)
    return 3;
  else if ((c & 0xF8) == 0xF0)
    return 4;
  else
    return 1; // fallback for invalid UTF-8
}

bool
string_compare(const char* a, const char* b)
{
  while (*a && *b) {
    uint32_t ca = string_head(a);
    uint32_t cb = string_head(b);

    if (ca != cb)
      return false;

    // advance each pointer
    a += utf8_char_len((unsigned char)a[0]);
    b += utf8_char_len((unsigned char)b[0]);
  }

  // both must end simultaneously
  return *a == '\0' && *b == '\0';
}

/*
 * ////////////////////////////////////////////////////////////////////////////
 * Misc.
 *
 */

bool
is_null(void* ptr)
{
  return (ptr == NULL);
}

int32_t
int32_mod(int32_t m, int32_t n)
{
  return m % n;
}

int64_t
int64_mod(int64_t m, int64_t n)
{
  return m % n;
}

double*
double_random(void)
{
  double* p = gc_malloc(sizeof(double));

  *p = (double)rand() / (double)RAND_MAX;

  return p;
}

void
init(void)
{
  srand((unsigned)time(NULL));
  gc_init();
}
