#ifndef COAL_CHAR_H
#define COAL_CHAR_H

#include <stdint.h>
#include <stdbool.h>

typedef struct rt_string rt_string_t;

/**
 * Compare two Unicode code points.
 *
 * Returns:
 *   Negative if a < b, zero if a == b, positive if a > b
 */
int rt_char_cmp(uint32_t a, uint32_t b);

/**
 * Check if a code point is a digit.
 *
 * Returns:
 *   true if the character is a digit
 */
bool rt_char_is_digit(uint32_t cp);

/**
 * Check if a code point is alphabetic.
 *
 * Returns:
 *   true if the character is alphabetic
 */
bool rt_char_is_alpha(uint32_t cp);

/**
 * Check if a code point is whitespace.
 *
 * Returns:
 *   true if the character is whitespace
 */
bool rt_char_is_whitespace(uint32_t cp);

/**
 * Check if a code point is uppercase.
 *
 * Returns:
 *   true if the character is uppercase
 */
bool rt_char_is_upper(uint32_t cp);

/**
 * Check if a code point is lowercase.
 *
 * Returns:
 *   true if the character is lowercase
 */
bool rt_char_is_lower(uint32_t cp);

/**
 * Convert a code point to uppercase.
 *
 * Returns:
 *   Uppercase version of the character
 */
uint32_t rt_char_to_upper(uint32_t cp);

/**
 * Convert a code point to lowercase.
 *
 * Returns:
 *   Lowercase version of the character
 */
uint32_t rt_char_to_lower(uint32_t cp);

#endif
