#ifndef COAL_BIGNUM_H
#define COAL_BIGNUM_H

#include <stdbool.h>
#include <stdint.h>
#include <gmp.h>

typedef struct rt_bignum rt_bignum_t;

/**
 * Create a bignum from a 64-bit integer.
 *
 * Parameters:
 *   n - Integer value
 *
 * Returns:
 *   New bignum
 */
rt_bignum_t *rt_bignum_from_i64(int64_t n);

/**
 * Create a bignum from a decimal string.
 *
 * Parameters:
 *   s - String representation of number
 *
 * Returns:
 *   New bignum
 */
rt_bignum_t *rt_bignum_new(const char *s);

/**
 * Add two bignums.
 *
 * Returns:
 *   a + b
 */
rt_bignum_t *rt_bignum_add(const rt_bignum_t *a, const rt_bignum_t *b);

/**
 * Subtract two bignums.
 *
 * Returns:
 *   a - b
 */
rt_bignum_t *rt_bignum_sub(const rt_bignum_t *a, const rt_bignum_t *b);

/**
 * Multiply two bignums.
 *
 * Returns:
 *   a * b
 */
rt_bignum_t *rt_bignum_mul(const rt_bignum_t *a, const rt_bignum_t *b);

/**
 * Divide two bignums.
 *
 * Returns:
 *   a / b (truncated toward zero)
 */
rt_bignum_t *rt_bignum_div(const rt_bignum_t *a, const rt_bignum_t *b);

/**
 * Negate a bignum.
 *
 * Parameters:
 *   n - Bignum to negate
 *
 * Returns:
 *   -n
 */
rt_bignum_t *rt_bignum_neg(const rt_bignum_t *n);

/**
 * Compute the modulo of two bignums.
 *
 * Parameters:
 *   m - Dividend bignum
 *   n - Divisor bignum
 *
 * Returns:
 *   m mod n (result is always non-negative)
 */
rt_bignum_t *rt_bignum_mod(const rt_bignum_t *m, const rt_bignum_t *n);

/**
 * Compare two bignums.
 *
 * Returns:
 *   Negative if a < b, zero if a == b, positive if a > b
 */
int rt_bignum_cmp(const rt_bignum_t *a, const rt_bignum_t *b);

/**
 * Test if one bignum is less than another.
 *
 * Returns:
 *   true if a < b
 */
bool rt_bignum_lt(const rt_bignum_t *a, const rt_bignum_t *b);

/**
 * Test if one bignum is greater than another.
 *
 * Returns:
 *   true if a > b
 */
bool rt_bignum_gt(const rt_bignum_t *a, const rt_bignum_t *b);

/**
 * Test if two bignums are equal.
 *
 * Returns:
 *   true if a == b
 */
bool rt_bignum_eq(const rt_bignum_t *a, const rt_bignum_t *b);

/**
 * Convert a bignum to a C string.
 *
 * Returns:
 *   String representation in base 10
 */
char *rt_bignum_to_cstring(const rt_bignum_t *n);

/**
 * Convert a 32-bit signed integer to a bignum.
 *
 * Parameters:
 *   n - Integer value to convert
 *
 * Returns:
 *   New bignum representing n
 */
rt_bignum_t *rt_int32_to_bignum(int32_t n);

/**
 * Convert a 64-bit signed integer to a bignum.
 *
 * Parameters:
 *   n - Integer value to convert
 *
 * Returns:
 *   New bignum representing n
 */
rt_bignum_t *rt_int64_to_bignum(int64_t n);

/**
 * Convert a bignum to a 32-bit signed integer.
 *
 * Parameters:
 *   n - Bignum to convert
 *
 * Returns:
 *   32-bit integer value (truncated if bignum is out of range)
 */
int32_t rt_bignum_to_int32(const rt_bignum_t *n);

/**
 * Convert a bignum to a 64-bit signed integer.
 *
 * Parameters:
 *   n - Bignum to convert
 *
 * Returns:
 *   64-bit integer value (truncated if bignum is out of range)
 */
int64_t rt_bignum_to_int64(const rt_bignum_t *n);

/**
 * Convert a bignum to a single-precision floating point.
 *
 * Parameters:
 *   n - Bignum to convert
 *
 * Returns:
 *   Float approximation of the bignum value
 */
float rt_bignum_to_float(const rt_bignum_t *n);

/**
 * Convert a bignum to a double-precision floating point.
 *
 * Parameters:
 *   n - Bignum to convert
 *
 * Returns:
 *   Double approximation of the bignum value
 */
double rt_bignum_to_double(const rt_bignum_t *n);

/**
 * Get the underlying GMP value.
 *
 * Returns:
 *   Pointer to mpz_t value
 */
mpz_ptr rt_bignum_value(const rt_bignum_t *n);

#endif
