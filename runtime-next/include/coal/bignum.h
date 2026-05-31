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
 * Get the underlying GMP value.
 *
 * Returns:
 *   Pointer to mpz_t value
 */
mpz_ptr rt_bignum_value(const rt_bignum_t *n);

#endif
