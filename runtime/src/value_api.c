#include "coal/value_api.h"
#include <errno.h>
#include <stdlib.h>

/* ============================================================================
 * Type conversions
 * ============================================================================
 */

rt_value_t
coal_float_to_int32(rt_value_t v)
{
    return rt_int32_box((int32_t) rt_float_unbox(v));
}

rt_value_t
coal_float_to_int64(rt_value_t v)
{
    return rt_int64_box((int64_t) rt_float_unbox(v));
}

rt_value_t
coal_double_to_int32(rt_value_t v)
{
    return rt_int32_box((int32_t) rt_double_unbox(v));
}

rt_value_t
coal_double_to_int64(rt_value_t v)
{
    return rt_int64_box((int64_t) rt_double_unbox(v));
}

rt_value_t
coal_int32_to_float(rt_value_t v)
{
    return rt_float_box((float) rt_int32_unbox(v));
}

rt_value_t
coal_int32_to_double(rt_value_t v)
{
    return rt_double_box((double) rt_int32_unbox(v));
}

rt_value_t
coal_int64_to_float(rt_value_t v)
{
    return rt_float_box((float) rt_int64_unbox(v));
}

rt_value_t
coal_int64_to_double(rt_value_t v)
{
    return rt_double_box((double) rt_int64_unbox(v));
}

rt_value_t
coal_float_to_double(rt_value_t v)
{
    return rt_double_box((double) rt_float_unbox(v));
}

rt_value_t
coal_double_to_float(rt_value_t v)
{
    return rt_float_box((float) rt_double_unbox(v));
}

rt_value_t
coal_int32_to_bignum(rt_value_t v)
{
    return rt_bignum_box(rt_int32_to_bignum(rt_int32_unbox(v)));
}

rt_value_t
coal_int64_to_bignum(rt_value_t v)
{
    return rt_bignum_box(rt_int64_to_bignum(rt_int64_unbox(v)));
}

/* ============================================================================
 * I/O operations
 * ============================================================================
 */

void
coal_print_int32(rt_value_t v)
{
    rt_print_int32(rt_int32_unbox(v));
}

void
coal_print_int64(rt_value_t v)
{
    rt_print_int64(rt_int64_unbox(v));
}

void
coal_print_string(rt_value_t v)
{
    rt_print_string(rt_string_data(rt_string_unbox(v)));
}

void
coal_print_char(rt_value_t v)
{
    rt_print_char(rt_char_unbox(v));
}

void
coal_print_bool(rt_value_t v)
{
    rt_print_bool(rt_bool_unbox(v));
}

void
coal_print_float(rt_value_t v)
{
    rt_print_float(rt_float_unbox(v));
}

void
coal_print_double(rt_value_t v)
{
    rt_print_double(rt_double_unbox(v));
}

void
coal_print_bignum(rt_value_t v)
{
    rt_print_bignum(rt_bignum_value(rt_bignum_unbox(v)));
}

void
coal_println_int32(rt_value_t v)
{
    rt_println_int32(rt_int32_unbox(v));
}

void
coal_println_int64(rt_value_t v)
{
    rt_println_int64(rt_int64_unbox(v));
}

void
coal_println_string(rt_value_t v)
{
    rt_println_string(rt_string_data(rt_string_unbox(v)));
}

void
coal_println_bool(rt_value_t v)
{
    rt_println_bool(rt_bool_unbox(v));
}

void
coal_println_char(rt_value_t v)
{
    rt_println_char(rt_char_unbox(v));
}

void
coal_println_float(rt_value_t v)
{
    rt_println_float(rt_float_unbox(v));
}

void
coal_println_double(rt_value_t v)
{
    rt_println_double(rt_double_unbox(v));
}

void
coal_println_bignum(rt_value_t v)
{
    rt_println_bignum(rt_bignum_value(rt_bignum_unbox(v)));
}

rt_value_t
coal_readln(void)
{
    return rt_string_box(rt_string_new(rt_readln()));
}

/* ============================================================================
 * Bignum operations
 * ============================================================================
 */

rt_value_t
coal_bignum_init(rt_value_t v)
{
    return rt_bignum_box(rt_bignum_new(rt_string_data(rt_string_unbox(v))));
}

rt_value_t
coal_parse_int32(rt_value_t v)
{
    const char *str = rt_string_data(rt_string_unbox(v));
    char *endptr;
    errno = 0;
    long val = strtol(str, &endptr, 10);

    /* Check for conversion errors */
    if (errno != 0 || *endptr != '\0' || val < INT32_MIN || val > INT32_MAX) {
        return rt_ptr_box(NULL);
    }

    return rt_int32_box((int32_t) val);
}

rt_value_t
coal_parse_int64(rt_value_t v)
{
    const char *str = rt_string_data(rt_string_unbox(v));
    char *endptr;
    errno = 0;
    long long val = strtoll(str, &endptr, 10);

    /* Check for conversion errors */
    if (errno != 0 || *endptr != '\0') {
        return rt_ptr_box(NULL);
    }

    return rt_int64_box((int64_t) val);
}

rt_value_t
coal_parse_float(rt_value_t v)
{
    const char *str = rt_string_data(rt_string_unbox(v));
    char *endptr;
    errno = 0;
    float val = strtof(str, &endptr);

    /* Check for conversion errors */
    if (errno != 0 || *endptr != '\0') {
        return rt_ptr_box(NULL);
    }

    return rt_float_box(val);
}

rt_value_t
coal_parse_double(rt_value_t v)
{
    const char *str = rt_string_data(rt_string_unbox(v));
    char *endptr;
    errno = 0;
    double val = strtod(str, &endptr);

    /* Check for conversion errors */
    if (errno != 0 || *endptr != '\0') {
        return rt_ptr_box(NULL);
    }

    return rt_double_box(val);
}

/* ============================================================================
 * Parsing operations
 * ============================================================================
 */

rt_value_t coal_parse_int32(rt_value_t v);

rt_value_t coal_parse_int64(rt_value_t v);

rt_value_t coal_parse_float(rt_value_t v);

rt_value_t coal_parse_double(rt_value_t v);

rt_value_t
coal_bignum_from_i64(rt_value_t v)
{
    return rt_bignum_box(rt_bignum_from_i64(rt_int64_unbox(v)));
}

rt_value_t
coal_bignum_add(rt_value_t a, rt_value_t b)
{
    return rt_bignum_box(rt_bignum_add(rt_bignum_unbox(a), rt_bignum_unbox(b)));
}

rt_value_t
coal_bignum_sub(rt_value_t a, rt_value_t b)
{
    return rt_bignum_box(rt_bignum_sub(rt_bignum_unbox(a), rt_bignum_unbox(b)));
}

rt_value_t
coal_bignum_mul(rt_value_t a, rt_value_t b)
{
    return rt_bignum_box(rt_bignum_mul(rt_bignum_unbox(a), rt_bignum_unbox(b)));
}

rt_value_t
coal_bignum_div(rt_value_t a, rt_value_t b)
{
    return rt_bignum_box(rt_bignum_div(rt_bignum_unbox(a), rt_bignum_unbox(b)));
}

rt_value_t
coal_bignum_neg(rt_value_t v)
{
    return rt_bignum_box(rt_bignum_neg(rt_bignum_unbox(v)));
}

rt_value_t
coal_bignum_mod(rt_value_t m, rt_value_t n)
{
    return rt_bignum_box(rt_bignum_mod(rt_bignum_unbox(m), rt_bignum_unbox(n)));
}

rt_value_t
coal_bignum_cmp(rt_value_t a, rt_value_t b)
{
    return rt_int32_box(rt_bignum_cmp(rt_bignum_unbox(a), rt_bignum_unbox(b)));
}

rt_value_t
coal_bignum_lt(rt_value_t a, rt_value_t b)
{
    return rt_bool_box(rt_bignum_lt(rt_bignum_unbox(a), rt_bignum_unbox(b)));
}

rt_value_t
coal_bignum_gt(rt_value_t a, rt_value_t b)
{
    return rt_bool_box(rt_bignum_gt(rt_bignum_unbox(a), rt_bignum_unbox(b)));
}

rt_value_t
coal_bignum_eq(rt_value_t a, rt_value_t b)
{
    return rt_bool_box(rt_bignum_eq(rt_bignum_unbox(a), rt_bignum_unbox(b)));
}

rt_value_t
coal_bignum_to_int32(rt_value_t v)
{
    return rt_int32_box(rt_bignum_to_int32(rt_bignum_unbox(v)));
}

rt_value_t
coal_bignum_to_int64(rt_value_t v)
{
    return rt_int64_box(rt_bignum_to_int64(rt_bignum_unbox(v)));
}

rt_value_t
coal_bignum_to_float(rt_value_t v)
{
    return rt_float_box(rt_bignum_to_float(rt_bignum_unbox(v)));
}

rt_value_t
coal_bignum_to_double(rt_value_t v)
{
    return rt_double_box(rt_bignum_to_double(rt_bignum_unbox(v)));
}

/* ============================================================================
 * String operations
 * ============================================================================
 */

rt_value_t
coal_string_concat(rt_value_t a, rt_value_t b)
{
    return rt_string_box(
        rt_string_concat(rt_string_unbox(a), rt_string_unbox(b)));
}

rt_value_t
coal_string_length(rt_value_t v)
{
    return rt_int64_box(rt_string_length(rt_string_unbox(v)));
}

rt_value_t
coal_string_compare(rt_value_t a, rt_value_t b)
{
    return rt_bool_box(
        rt_string_compare(rt_string_unbox(a), rt_string_unbox(b)));
}

rt_value_t
coal_string_reverse(rt_value_t v)
{
    return rt_string_box(rt_string_reverse(rt_string_unbox(v)));
}

rt_value_t
coal_string_head(rt_value_t v)
{
    return rt_int32_box(rt_string_head(rt_string_unbox(v)));
}

rt_value_t
coal_string_tail(rt_value_t v)
{
    return rt_string_box(rt_string_tail(rt_string_unbox(v)));
}

rt_value_t
coal_string_remove_whitespace(rt_value_t v)
{
    return rt_string_box(rt_string_remove_whitespace(rt_string_unbox(v)));
}

rt_value_t
coal_bool_to_string(rt_value_t v)
{
    return rt_string_box(rt_bool_to_string(rt_bool_unbox(v)));
}

rt_value_t
coal_int32_to_string(rt_value_t v)
{
    return rt_string_box(rt_int32_to_string(rt_int32_unbox(v)));
}

rt_value_t
coal_int64_to_string(rt_value_t v)
{
    return rt_string_box(rt_int64_to_string(rt_int64_unbox(v)));
}

rt_value_t
coal_float_to_string(rt_value_t v)
{
    return rt_string_box(rt_float_to_string(rt_float_unbox(v)));
}

rt_value_t
coal_double_to_string(rt_value_t v)
{
    return rt_string_box(rt_double_to_string(rt_double_unbox(v)));
}

rt_value_t
coal_bignum_to_string(rt_value_t v)
{
    return rt_string_box(
        rt_bignum_to_string(rt_bignum_value(rt_bignum_unbox(v))));
}

rt_value_t
coal_char_to_string(rt_value_t v)
{
    return rt_string_box(rt_char_to_string(rt_char_unbox(v)));
}

/* ============================================================================
 * Character operations
 * ============================================================================
 */

rt_value_t
coal_char_cmp(rt_value_t a, rt_value_t b)
{
    return rt_int32_box(rt_char_cmp(rt_char_unbox(a), rt_char_unbox(b)));
}

rt_value_t
coal_char_is_digit(rt_value_t v)
{
    return rt_bool_box(rt_char_is_digit(rt_char_unbox(v)));
}

rt_value_t
coal_char_is_alpha(rt_value_t v)
{
    return rt_bool_box(rt_char_is_alpha(rt_char_unbox(v)));
}

rt_value_t
coal_char_is_whitespace(rt_value_t v)
{
    return rt_bool_box(rt_char_is_whitespace(rt_char_unbox(v)));
}

rt_value_t
coal_char_is_upper(rt_value_t v)
{
    return rt_bool_box(rt_char_is_upper(rt_char_unbox(v)));
}

rt_value_t
coal_char_is_lower(rt_value_t v)
{
    return rt_bool_box(rt_char_is_lower(rt_char_unbox(v)));
}

rt_value_t
coal_char_to_upper(rt_value_t v)
{
    return rt_char_box(rt_char_to_upper(rt_char_unbox(v)));
}

rt_value_t
coal_char_to_lower(rt_value_t v)
{
    return rt_char_box(rt_char_to_lower(rt_char_unbox(v)));
}

/* ============================================================================
 * Closure and function application
 * ============================================================================
 */

rt_value_t
coal_closure_new(void *fn, rt_value_t arity)
{
    return rt_closure_box(rt_closure_new(fn, rt_int32_unbox(arity)));
}

rt_value_t
coal_closure_extend(rt_value_t closure, rt_value_t argc, void **args)
{
    return rt_closure_box(rt_closure_extend(rt_closure_unbox(closure),
                                            rt_int32_unbox(argc), args));
}

/* ============================================================================
 * Math operations
 * ============================================================================
 */

rt_value_t
coal_int32_mod(rt_value_t m, rt_value_t n)
{
    return rt_int32_box(rt_int32_mod(rt_int32_unbox(m), rt_int32_unbox(n)));
}

rt_value_t
coal_int64_mod(rt_value_t m, rt_value_t n)
{
    return rt_int64_box(rt_int64_mod(rt_int64_unbox(m), rt_int64_unbox(n)));
}

/* ============================================================================
 * Random number generation
 * ============================================================================
 */

rt_value_t
coal_float_random(void)
{
    return rt_float_box(rt_float_random());
}

rt_value_t
coal_double_random(void)
{
    return rt_double_box(rt_double_random());
}

/* ============================================================================
 * File I/O
 * ============================================================================
 */

rt_value_t
coal_read_file(rt_value_t filename)
{
    return rt_ptr_box(rt_read_file(rt_string_data(rt_string_unbox(filename))));
}

rt_value_t
coal_write_file(rt_value_t filename, rt_value_t data)
{
    return rt_ptr_box(rt_write_file(rt_string_data(rt_string_unbox(filename)),
                                    rt_string_data(rt_string_unbox(data))));
}

rt_value_t
coal_result_status(rt_value_t result)
{
    return rt_int32_box(rt_result_status((rt_result_t *) rt_ptr_unbox(result)));
}

rt_value_t
coal_result_value(rt_value_t result)
{
    char *val = rt_result_value((rt_result_t *) rt_ptr_unbox(result));
    return val ? rt_string_box(rt_string_new(val)) : rt_ptr_box(NULL);
}
