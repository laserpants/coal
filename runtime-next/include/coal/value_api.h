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
rt_value_t
coal_float_to_int32(rt_value_t v)
{
    return rt_int32_box((int32_t) rt_float_unbox(v));
}

/** Convert boxed float to int64 (truncates) */
rt_value_t
coal_float_to_int64(rt_value_t v)
{
    return rt_int64_box((int64_t) rt_float_unbox(v));
}

/** Convert boxed double to int32 (truncates) */
rt_value_t
coal_double_to_int32(rt_value_t v)
{
    return rt_int32_box((int32_t) rt_double_unbox(v));
}

/** Convert boxed double to int64 (truncates) */
rt_value_t
coal_double_to_int64(rt_value_t v)
{
    return rt_int64_box((int64_t) rt_double_unbox(v));
}

/** Convert boxed int32 to float */
rt_value_t
coal_int32_to_float(rt_value_t v)
{
    return rt_float_box((float) rt_int32_unbox(v));
}

/** Convert boxed int32 to double */
rt_value_t
coal_int32_to_double(rt_value_t v)
{
    return rt_double_box((double) rt_int32_unbox(v));
}

/** Convert boxed int64 to float */
rt_value_t
coal_int64_to_float(rt_value_t v)
{
    return rt_float_box((float) rt_int64_unbox(v));
}

/** Convert boxed int64 to double */
rt_value_t
coal_int64_to_double(rt_value_t v)
{
    return rt_double_box((double) rt_int64_unbox(v));
}

/** Convert boxed float to double */
rt_value_t
coal_float_to_double(rt_value_t v)
{
    return rt_double_box((double) rt_float_unbox(v));
}

/** Convert boxed double to float */
rt_value_t
coal_double_to_float(rt_value_t v)
{
    return rt_float_box((float) rt_double_unbox(v));
}

/** Convert boxed int32 to bignum */
rt_value_t
coal_int32_to_bignum(rt_value_t v)
{
    return rt_bignum_box(rt_int32_to_bignum(rt_int32_unbox(v)));
}

/** Convert boxed int64 to bignum */
rt_value_t
coal_int64_to_bignum(rt_value_t v)
{
    return rt_bignum_box(rt_int64_to_bignum(rt_int64_unbox(v)));
}

/* ============================================================================
 * I/O operations
 * ============================================================================
 */

/** Print boxed int32 */
void
coal_print_int32(rt_value_t v)
{
    rt_print_int32(rt_int32_unbox(v));
}

/** Print boxed int64 */
void
coal_print_int64(rt_value_t v)
{
    rt_print_int64(rt_int64_unbox(v));
}

/** Print boxed string */
void
coal_print_string(rt_value_t v)
{
    rt_print_string(rt_string_data(rt_string_unbox(v)));
}

/** Print boxed character */
void
coal_print_char(rt_value_t v)
{
    rt_print_char(rt_char_unbox(v));
}

/** Print boxed boolean */
void
coal_print_bool(rt_value_t v)
{
    rt_print_bool(rt_bool_unbox(v));
}

/** Print boxed float */
void
coal_print_float(rt_value_t v)
{
    rt_print_float(rt_float_unbox(v));
}

/** Print boxed double */
void
coal_print_double(rt_value_t v)
{
    rt_print_double(rt_double_unbox(v));
}

/** Print boxed bignum */
void
coal_print_bignum(rt_value_t v)
{
    rt_print_bignum(rt_bignum_value(rt_bignum_unbox(v)));
}

/** Print boxed int32 with newline */
void
coal_println_int32(rt_value_t v)
{
    rt_println_int32(rt_int32_unbox(v));
}

/** Print boxed int64 with newline */
void
coal_println_int64(rt_value_t v)
{
    rt_println_int64(rt_int64_unbox(v));
}

/** Print boxed boolean with newline */
void
coal_println_bool(rt_value_t v)
{
    rt_println_bool(rt_bool_unbox(v));
}

/** Print boxed character with newline */
void
coal_println_char(rt_value_t v)
{
    rt_println_char(rt_char_unbox(v));
}

/** Print boxed float with newline */
void
coal_println_float(rt_value_t v)
{
    rt_println_float(rt_float_unbox(v));
}

/** Print boxed double with newline */
void
coal_println_double(rt_value_t v)
{
    rt_println_double(rt_double_unbox(v));
}

/** Print boxed bignum with newline */
void
coal_println_bignum(rt_value_t v)
{
    rt_println_bignum(rt_bignum_value(rt_bignum_unbox(v)));
}

/** Read a line from stdin, returns boxed string */
rt_value_t
coal_readln(void)
{
    return rt_string_box(rt_string_new(rt_readln()));
}

/* ============================================================================
 * Bignum operations
 * ============================================================================
 */

/** Create a bignum from a decimal string */
rt_value_t
coal_bignum_init(char *s)
{
    return rt_bignum_box(rt_bignum_new(s));
}

/** Create bignum from boxed int64 */
rt_value_t
coal_bignum_from_i64(rt_value_t v)
{
    return rt_bignum_box(rt_bignum_from_i64(rt_int64_unbox(v)));
}

/** Add two boxed bignums */
rt_value_t
coal_bignum_add(rt_value_t a, rt_value_t b)
{
    return rt_bignum_box(rt_bignum_add(rt_bignum_unbox(a), rt_bignum_unbox(b)));
}

/** Subtract two boxed bignums */
rt_value_t
coal_bignum_sub(rt_value_t a, rt_value_t b)
{
    return rt_bignum_box(rt_bignum_sub(rt_bignum_unbox(a), rt_bignum_unbox(b)));
}

/** Multiply two boxed bignums */
rt_value_t
coal_bignum_mul(rt_value_t a, rt_value_t b)
{
    return rt_bignum_box(rt_bignum_mul(rt_bignum_unbox(a), rt_bignum_unbox(b)));
}

/** Divide two boxed bignums */
rt_value_t
coal_bignum_div(rt_value_t a, rt_value_t b)
{
    return rt_bignum_box(rt_bignum_div(rt_bignum_unbox(a), rt_bignum_unbox(b)));
}

/** Negate a boxed bignum */
rt_value_t
coal_bignum_neg(rt_value_t v)
{
    return rt_bignum_box(rt_bignum_neg(rt_bignum_unbox(v)));
}

/** Compute modulo of two boxed bignums */
rt_value_t
coal_bignum_mod(rt_value_t m, rt_value_t n)
{
    return rt_bignum_box(rt_bignum_mod(rt_bignum_unbox(m), rt_bignum_unbox(n)));
}

/** Compare two boxed bignums */
rt_value_t
coal_bignum_cmp(rt_value_t a, rt_value_t b)
{
    return rt_int32_box(rt_bignum_cmp(rt_bignum_unbox(a), rt_bignum_unbox(b)));
}

/** Test if boxed bignum a < b */
rt_value_t
coal_bignum_lt(rt_value_t a, rt_value_t b)
{
    return rt_bool_box(rt_bignum_lt(rt_bignum_unbox(a), rt_bignum_unbox(b)));
}

/** Test if boxed bignum a > b */
rt_value_t
coal_bignum_gt(rt_value_t a, rt_value_t b)
{
    return rt_bool_box(rt_bignum_gt(rt_bignum_unbox(a), rt_bignum_unbox(b)));
}

/** Test if boxed bignum a == b */
rt_value_t
coal_bignum_eq(rt_value_t a, rt_value_t b)
{
    return rt_bool_box(rt_bignum_eq(rt_bignum_unbox(a), rt_bignum_unbox(b)));
}

/** Convert boxed bignum to int32 */
rt_value_t
coal_bignum_to_int32(rt_value_t v)
{
    return rt_int32_box(rt_bignum_to_int32(rt_bignum_unbox(v)));
}

/** Convert boxed bignum to int64 */
rt_value_t
coal_bignum_to_int64(rt_value_t v)
{
    return rt_int64_box(rt_bignum_to_int64(rt_bignum_unbox(v)));
}

/** Convert boxed bignum to float */
rt_value_t
coal_bignum_to_float(rt_value_t v)
{
    return rt_float_box(rt_bignum_to_float(rt_bignum_unbox(v)));
}

/** Convert boxed bignum to double */
rt_value_t
coal_bignum_to_double(rt_value_t v)
{
    return rt_double_box(rt_bignum_to_double(rt_bignum_unbox(v)));
}

/* ============================================================================
 * String operations
 * ============================================================================
 */

/** Concatenate two boxed strings */
rt_value_t
coal_string_concat(rt_value_t a, rt_value_t b)
{
    return rt_string_box(
        rt_string_concat(rt_string_unbox(a), rt_string_unbox(b)));
}

/** Get length of boxed string */
rt_value_t
coal_string_length(rt_value_t v)
{
    return rt_int64_box(rt_string_length(rt_string_unbox(v)));
}

/** Test if two boxed strings are equal */
rt_value_t
coal_string_equal(rt_value_t a, rt_value_t b)
{
    return rt_bool_box(rt_string_equal(rt_string_unbox(a), rt_string_unbox(b)));
}

/** Reverse a boxed string */
rt_value_t
coal_string_reverse(rt_value_t v)
{
    return rt_string_box(rt_string_reverse(rt_string_unbox(v)));
}

/** Get first character of boxed string */
rt_value_t
coal_string_head(rt_value_t v)
{
    return rt_int32_box(rt_string_head(rt_string_unbox(v)));
}

/** Get all but first character of boxed string */
rt_value_t
coal_string_tail(rt_value_t v)
{
    return rt_string_box(rt_string_tail(rt_string_unbox(v)));
}

/** Remove whitespace from boxed string */
rt_value_t
coal_string_remove_whitespace(rt_value_t v)
{
    return rt_string_box(rt_string_remove_whitespace(rt_string_unbox(v)));
}

/** Convert boxed boolean to string */
rt_value_t
coal_bool_to_string(rt_value_t v)
{
    return rt_string_box(rt_bool_to_string(rt_bool_unbox(v)));
}

/** Convert boxed int32 to string */
rt_value_t
coal_int32_to_string(rt_value_t v)
{
    return rt_string_box(rt_int32_to_string(rt_int32_unbox(v)));
}

/** Convert boxed int64 to string */
rt_value_t
coal_int64_to_string(rt_value_t v)
{
    return rt_string_box(rt_int64_to_string(rt_int64_unbox(v)));
}

/** Convert boxed float to string */
rt_value_t
coal_float_to_string(rt_value_t v)
{
    return rt_string_box(rt_float_to_string(rt_float_unbox(v)));
}

/** Convert boxed double to string */
rt_value_t
coal_double_to_string(rt_value_t v)
{
    return rt_string_box(rt_double_to_string(rt_double_unbox(v)));
}

/** Convert boxed bignum to string */
rt_value_t
coal_bignum_to_string(rt_value_t v)
{
    return rt_string_box(
        rt_bignum_to_string(rt_bignum_value(rt_bignum_unbox(v))));
}

/** Convert boxed character to string */
rt_value_t
coal_char_to_string(rt_value_t v)
{
    return rt_string_box(rt_char_to_string(rt_char_unbox(v)));
}

/* ============================================================================
 * Character operations
 * ============================================================================
 */

/** Compare two boxed characters */
rt_value_t
coal_char_cmp(rt_value_t a, rt_value_t b)
{
    return rt_int32_box(rt_char_cmp(rt_char_unbox(a), rt_char_unbox(b)));
}

/** Test if boxed character is a digit */
rt_value_t
coal_char_is_digit(rt_value_t v)
{
    return rt_bool_box(rt_char_is_digit(rt_char_unbox(v)));
}

/** Test if boxed character is alphabetic */
rt_value_t
coal_char_is_alpha(rt_value_t v)
{
    return rt_bool_box(rt_char_is_alpha(rt_char_unbox(v)));
}

/** Test if boxed character is whitespace */
rt_value_t
coal_char_is_whitespace(rt_value_t v)
{
    return rt_bool_box(rt_char_is_whitespace(rt_char_unbox(v)));
}

/** Test if boxed character is uppercase */
rt_value_t
coal_char_is_upper(rt_value_t v)
{
    return rt_bool_box(rt_char_is_upper(rt_char_unbox(v)));
}

/** Test if boxed character is lowercase */
rt_value_t
coal_char_is_lower(rt_value_t v)
{
    return rt_bool_box(rt_char_is_lower(rt_char_unbox(v)));
}

/** Convert boxed character to uppercase */
rt_value_t
coal_char_to_upper(rt_value_t v)
{
    return rt_char_box(rt_char_to_upper(rt_char_unbox(v)));
}

/** Convert boxed character to lowercase */
rt_value_t
coal_char_to_lower(rt_value_t v)
{
    return rt_char_box(rt_char_to_lower(rt_char_unbox(v)));
}

/* ============================================================================
 * Closure and function application
 * ============================================================================
 */

/** Create a new boxed closure */
rt_value_t
coal_closure_new(void *fn, rt_value_t arity)
{
    return rt_closure_box(rt_closure_new(fn, rt_int32_unbox(arity)));
}

/** Extend boxed closure with arguments */
rt_value_t
coal_closure_extend(rt_value_t closure, rt_value_t argc, void **args)
{
    return rt_closure_box(rt_closure_extend(rt_closure_unbox(closure),
                                            rt_int32_unbox(argc), args));
}

/** Apply arguments to boxed closure */
rt_value_t
coal_apply(rt_value_t closure, rt_value_t argc, void **args)
{
    return (rt_value_t) rt_apply(rt_closure_unbox(closure),
                                 rt_int32_unbox(argc), args);
}

/* ============================================================================
 * Math operations
 * ============================================================================
 */

/** Compute modulo of two boxed int32 values */
rt_value_t
coal_int32_mod(rt_value_t m, rt_value_t n)
{
    return rt_int32_box(rt_int32_mod(rt_int32_unbox(m), rt_int32_unbox(n)));
}

/** Compute modulo of two boxed int64 values */
rt_value_t
coal_int64_mod(rt_value_t m, rt_value_t n)
{
    return rt_int64_box(rt_int64_mod(rt_int64_unbox(m), rt_int64_unbox(n)));
}

/* ============================================================================
 * Random number generation
 * ============================================================================
 */

/** Generate random boxed float in [0.0, 1.0) */
rt_value_t
coal_float_random(void)
{
    return rt_float_box(rt_float_random());
}

/** Generate random boxed double in [0.0, 1.0) */
rt_value_t
coal_double_random(void)
{
    return rt_double_box(rt_double_random());
}

/* ============================================================================
 * File I/O
 * ============================================================================
 */

/** Read file, returns boxed result */
rt_value_t
coal_read_file(rt_value_t filename)
{
    return rt_ptr_box(rt_read_file(rt_string_data(rt_string_unbox(filename))));
}

/** Write file, returns boxed result */
rt_value_t
coal_write_file(rt_value_t filename, rt_value_t data)
{
    return rt_ptr_box(rt_write_file(rt_string_data(rt_string_unbox(filename)),
                                    rt_string_data(rt_string_unbox(data))));
}

/** Get status from boxed result */
rt_value_t
coal_result_status(rt_value_t result)
{
    return rt_int32_box(rt_result_status((rt_result_t *) rt_ptr_unbox(result)));
}

/** Get value from boxed result */
rt_value_t
coal_result_value(rt_value_t result)
{
    char *val = rt_result_value((rt_result_t *) rt_ptr_unbox(result));
    return val ? rt_string_box(rt_string_new(val)) : rt_ptr_box(NULL);
}

#endif
