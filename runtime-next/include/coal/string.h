#ifndef COAL_STRING_H
#define COAL_STRING_H

#include <stdint.h>
#include <stdbool.h>
#include <gmp.h>

typedef struct rt_bignum rt_bignum_t;
typedef struct rt_string rt_string_t;

/**
 * Create a new string from a C string.
 *
 * Parameters:
 *   s - Null-terminated C string
 *
 * Returns:
 *   New UTF-8 string
 */
rt_string_t *rt_string_new(const char *s);

/**
 * Concatenate two strings.
 *
 * Returns:
 *   New string containing a followed by b
 */
rt_string_t *rt_string_concat(const rt_string_t *a, const rt_string_t *b);

/**
 * Get the byte length of a string.
 *
 * Returns:
 *   Number of bytes (not characters)
 */
int64_t rt_string_length(const rt_string_t *s);

/**
 * Get the underlying C string data.
 *
 * Returns:
 *   Pointer to null-terminated C string
 */
char *rt_string_data(const rt_string_t *s);

/**
 * Compare two strings for equality.
 *
 * Returns:
 *   true if strings are equal
 */
bool rt_string_equal(const rt_string_t *a, const rt_string_t *b);

/**
 * Reverse a string.
 * Properly handles multi-byte UTF-8 characters.
 *
 * Returns:
 *   New string with characters in reverse order
 */
rt_string_t *rt_string_reverse(const rt_string_t *s);

/**
 * Convert a boolean to a string.
 *
 * Returns:
 *   "true" or "false"
 */
rt_string_t *rt_bool_to_string(bool b);

/**
 * Convert a 32-bit integer to a string.
 */
rt_string_t *rt_int32_to_string(int32_t n);

/**
 * Convert a 64-bit integer to a string.
 */
rt_string_t *rt_int64_to_string(int64_t n);

/**
 * Convert a float to a string.
 */
rt_string_t *rt_float_to_string(float f);

/**
 * Convert a double to a string.
 */
rt_string_t *rt_double_to_string(double d);

/**
 * Convert a bignum to a string.
 */
rt_string_t *rt_bignum_to_string(mpz_ptr n);

/**
 * Convert a Unicode code point to a string.
 */
rt_string_t *rt_char_to_string(uint32_t cp);

/**
 * Get the first character of a string.
 *
 * Returns:
 *   First Unicode code point, or 0 if string is empty
 */
int32_t rt_string_head(const rt_string_t *s);

/**
 * Get all but the first character of a string.
 *
 * Returns:
 *   New string without the first character
 */
rt_string_t *rt_string_tail(const rt_string_t *s);

/**
 * Remove all whitespace from a string.
 *
 * Returns:
 *   New string with whitespace removed
 */
rt_string_t *rt_string_remove_whitespace(const rt_string_t *s);

#endif
