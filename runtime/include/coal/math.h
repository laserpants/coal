#ifndef COAL_MATH_H
#define COAL_MATH_H

#include <stdint.h>

/**
 * Compute the modulo of two 32-bit integers.
 * Result has the same sign as the divisor.
 *
 * Returns:
 *   m mod n
 */
int32_t rt_int32_mod(int32_t m, int32_t n);

/**
 * Compute the modulo of two 64-bit integers.
 * Result has the same sign as the divisor.
 *
 * Returns:
 *   m mod n
 */
int64_t rt_int64_mod(int64_t m, int64_t n);

#endif
