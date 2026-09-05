#ifndef COAL_VALUE_API_H
#define COAL_VALUE_API_H

#include "coal/value.h"
#include "coal/io.h"
#include "coal/bignum.h"
#include "coal/string.h"
#include "coal/char.h"
#include "coal/closure.h"
#include "coal/record.h"
#include "coal/apply.h"
#include "coal/runtime.h"
#include "coal/math.h"
#include "coal/panic.h"

/**
 * Runtime API wrappers for code generation.
 *
 * These functions wrap the native runtime API, automatically handling
 * boxing and unboxing of values. All wrapper functions use the coal_ prefix
 * and take/return rt_value_t, making LLVM code generation simpler.
 */

/* ============================================================================
 * Type conversions
 * ============================================================================
 */

/** Convert boxed float to int32 (truncates) */
extern rt_value_t coal_float_to_int32(rt_value_t v);
/** Convert boxed float to int64 (truncates) */
extern rt_value_t coal_float_to_int64(rt_value_t v);
/** Convert boxed double to int32 (truncates) */
extern rt_value_t coal_double_to_int32(rt_value_t v);
/** Convert boxed double to int64 (truncates) */
extern rt_value_t coal_double_to_int64(rt_value_t v);
/** Convert boxed int32 to float */
extern rt_value_t coal_int32_to_float(rt_value_t v);
/** Convert boxed int32 to double */
extern rt_value_t coal_int32_to_double(rt_value_t v);
/** Convert boxed int64 to float */
extern rt_value_t coal_int64_to_float(rt_value_t v);
/** Convert boxed int64 to double */
extern rt_value_t coal_int64_to_double(rt_value_t v);
/** Convert boxed float to double */
extern rt_value_t coal_float_to_double(rt_value_t v);
/** Convert boxed double to float */
extern rt_value_t coal_double_to_float(rt_value_t v);
/** Convert boxed int32 to bignum */
extern rt_value_t coal_int32_to_bignum(rt_value_t v);
/** Convert boxed int64 to bignum */
extern rt_value_t coal_int64_to_bignum(rt_value_t v);
/** Convert boxed int32 to int64 (sign-extends) */
extern rt_value_t coal_int32_to_int64(rt_value_t v);

/* ============================================================================
 * I/O operations
 * ============================================================================
 */

/** Print boxed int32 */
extern void coal_print_int32(rt_value_t v);
/** Print boxed int64 */
extern void coal_print_int64(rt_value_t v);
/** Print boxed string */
extern void coal_print_string(rt_value_t v);
/** Print boxed character */
extern void coal_print_char(rt_value_t v);
/** Print boxed boolean */
extern void coal_print_bool(rt_value_t v);
/** Print boxed float */
extern void coal_print_float(rt_value_t v);
/** Print boxed double */
extern void coal_print_double(rt_value_t v);
/** Print boxed bignum */
extern void coal_print_bignum(rt_value_t v);
/** Print boxed int32 with newline */
extern void coal_println_int32(rt_value_t v);
/** Print boxed int64 with newline */
extern void coal_println_int64(rt_value_t v);
/** Print boxed string with newline */
extern void coal_println_string(rt_value_t v);
/** Print boxed boolean with newline */
extern void coal_println_bool(rt_value_t v);
/** Print boxed character with newline */
extern void coal_println_char(rt_value_t v);
/** Print boxed float with newline */
extern void coal_println_float(rt_value_t v);
/** Print boxed double with newline */
extern void coal_println_double(rt_value_t v);
/** Print boxed bignum with newline */
extern void coal_println_bignum(rt_value_t v);
/** Read a line from stdin, returns boxed string */
extern rt_value_t coal_readln(void);

/* ============================================================================
 * Parsing operations
 * ============================================================================
 */

/** Parse boxed string to int32, returns boxed int32 or NULL on failure */
extern rt_value_t coal_parse_int32(rt_value_t v);
/** Parse boxed string to int64, returns boxed int64 or NULL on failure */
extern rt_value_t coal_parse_int64(rt_value_t v);
/** Parse boxed string to float, returns boxed float or NULL on failure */
extern rt_value_t coal_parse_float(rt_value_t v);
/** Parse boxed string to double, returns boxed double or NULL on failure */
extern rt_value_t coal_parse_double(rt_value_t v);

/* ============================================================================
 * Bignum operations
 * ============================================================================
 */

/** Create a bignum from a decimal string */
extern rt_value_t coal_bignum_init(rt_value_t v);
/** Create bignum from boxed int64 */
extern rt_value_t coal_bignum_from_i64(rt_value_t v);
/** Add two boxed bignums */
extern rt_value_t coal_bignum_add(rt_value_t a, rt_value_t b);
/** Subtract two boxed bignums */
extern rt_value_t coal_bignum_sub(rt_value_t a, rt_value_t b);
/** Multiply two boxed bignums */
extern rt_value_t coal_bignum_mul(rt_value_t a, rt_value_t b);
/** Divide two boxed bignums */
extern rt_value_t coal_bignum_div(rt_value_t a, rt_value_t b);
/** Negate a boxed bignum */
extern rt_value_t coal_bignum_neg(rt_value_t v);
/** Compute modulo of two boxed bignums */
extern rt_value_t coal_bignum_mod(rt_value_t m, rt_value_t n);
/** Compare two boxed bignums */
extern rt_value_t coal_bignum_cmp(rt_value_t a, rt_value_t b);
/** Test if boxed bignum a < b */
extern rt_value_t coal_bignum_lt(rt_value_t a, rt_value_t b);
/** Test if boxed bignum a > b */
extern rt_value_t coal_bignum_gt(rt_value_t a, rt_value_t b);
/** Test if boxed bignum a == b */
extern rt_value_t coal_bignum_eq(rt_value_t a, rt_value_t b);
/** Convert boxed bignum to int32 */
extern rt_value_t coal_bignum_to_int32(rt_value_t v);
/** Convert boxed bignum to int64 */
extern rt_value_t coal_bignum_to_int64(rt_value_t v);
/** Convert boxed bignum to float */
extern rt_value_t coal_bignum_to_float(rt_value_t v);
/** Convert boxed bignum to double */
extern rt_value_t coal_bignum_to_double(rt_value_t v);

/* ============================================================================
 * String operations
 * ============================================================================
 */

/** Concatenate two boxed strings */
extern rt_value_t coal_string_concat(rt_value_t a, rt_value_t b);
/** Get length of boxed string */
extern rt_value_t coal_string_length(rt_value_t v);
/** Compare two boxed strings character-by-character */
extern rt_value_t coal_string_compare(rt_value_t a, rt_value_t b);
/** Reverse a boxed string */
extern rt_value_t coal_string_reverse(rt_value_t v);
/** Get first character of boxed string */
extern rt_value_t coal_string_head(rt_value_t v);
/** Get all but first character of boxed string */
extern rt_value_t coal_string_tail(rt_value_t v);
/** Remove whitespace from boxed string */
extern rt_value_t coal_string_remove_whitespace(rt_value_t v);
/** Convert boxed boolean to string */
extern rt_value_t coal_bool_to_string(rt_value_t v);
/** Convert boxed int32 to string */
extern rt_value_t coal_int32_to_string(rt_value_t v);
/** Convert boxed int64 to string */
extern rt_value_t coal_int64_to_string(rt_value_t v);
/** Convert boxed float to string */
extern rt_value_t coal_float_to_string(rt_value_t v);
/** Convert boxed double to string */
extern rt_value_t coal_double_to_string(rt_value_t v);
/** Convert boxed bignum to string */
extern rt_value_t coal_bignum_to_string(rt_value_t v);
/** Convert boxed character to string */
extern rt_value_t coal_char_to_string(rt_value_t v);

/* ============================================================================
 * Character operations
 * ============================================================================
 */

/** Compare two boxed characters */
extern rt_value_t coal_char_cmp(rt_value_t a, rt_value_t b);
/** Test if boxed character is a digit */
extern rt_value_t coal_char_is_digit(rt_value_t v);
/** Test if boxed character is alphabetic */
extern rt_value_t coal_char_is_alpha(rt_value_t v);
/** Test if boxed character is whitespace */
extern rt_value_t coal_char_is_whitespace(rt_value_t v);
/** Test if boxed character is uppercase */
extern rt_value_t coal_char_is_upper(rt_value_t v);
/** Test if boxed character is lowercase */
extern rt_value_t coal_char_is_lower(rt_value_t v);
/** Convert boxed character to uppercase */
extern rt_value_t coal_char_to_upper(rt_value_t v);
/** Convert boxed character to lowercase */
extern rt_value_t coal_char_to_lower(rt_value_t v);

/* ============================================================================
 * Closure and function application
 * ============================================================================
 */

/** Create a new boxed closure */
extern rt_value_t coal_closure_new(void *fn, rt_value_t arity);
/** Extend boxed closure with arguments */
extern rt_value_t coal_closure_extend(rt_value_t closure, rt_value_t argc,
                                      void **args);

/* ============================================================================
 * Math operations
 * ============================================================================
 */

/** Compute modulo of two boxed int32 values */
extern rt_value_t coal_int32_mod(rt_value_t m, rt_value_t n);
/** Compute modulo of two boxed int64 values */
extern rt_value_t coal_int64_mod(rt_value_t m, rt_value_t n);

/* ============================================================================
 * Random number generation
 * ============================================================================
 */

/** Generate random boxed float in [0.0, 1.0) */
extern rt_value_t coal_float_random(void);
/** Generate random boxed double in [0.0, 1.0) */
extern rt_value_t coal_double_random(void);

/* ============================================================================
 * Panic
 * ============================================================================ */

/** Terminate the program with a panic message from a boxed string */
extern _Noreturn void coal_panic(rt_value_t v);

/* ============================================================================
 * File I/O
 * ============================================================================
 */

/** Read file, returns boxed result */
extern rt_value_t coal_read_file(rt_value_t filename);
/** Write file, returns boxed result */
extern rt_value_t coal_write_file(rt_value_t filename, rt_value_t data);
/** Get status from boxed result */
extern rt_value_t coal_result_status(rt_value_t result);
/** Get value from boxed result */
extern rt_value_t coal_result_value(rt_value_t result);

#endif
