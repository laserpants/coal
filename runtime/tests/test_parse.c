/*
 * Tests for coal_parse_int32 and coal_parse_int64.
 *
 * Both return the parsed value as a heap-boxed bignum (or NULL on failure).
 * The bignum carrier exists because boxed integers are untagged immediates:
 * a boxed 0 is bit-identical to the NULL failure sentinel, so carrying the
 * value in a boxed int32 would make a successful parse of "0"
 * indistinguishable from a parse failure.
 */

#include "coal/bignum.h"
#include "coal/string.h"
#include "coal/value.h"
#include "coal/value_api.h"
#include <assert.h>
#include <stdint.h>
#include <stdio.h>

static rt_value_t
box_string(const char *s)
{
    return rt_string_box(rt_string_new(s));
}

/* Parse with the given function and return the parsed int32, asserting that
 * parsing succeeded. */
static int32_t
parse_int32_ok(const char *s)
{
    rt_value_t r = coal_parse_int32(box_string(s));
    assert(r != NULL);
    return rt_bignum_to_int32(rt_bignum_unbox(r));
}

static int64_t
parse_int64_ok(const char *s)
{
    rt_value_t r = coal_parse_int64(box_string(s));
    assert(r != NULL);
    return rt_bignum_to_int64(rt_bignum_unbox(r));
}

static void
test_parse_int32_zero(void)
{
    printf("test_parse_int32_zero: ");

    /* Regression: "0" used to come back as None because rt_int32_box(0) is
     * the raw word 0, i.e. the same bit pattern as the NULL error sentinel. */
    assert(parse_int32_ok("0") == 0);
    assert(parse_int32_ok("00") == 0);
    assert(parse_int32_ok("-0") == 0);
    assert(parse_int32_ok("+0") == 0);

    printf("PASS\n");
}

static void
test_parse_int32_values(void)
{
    printf("test_parse_int32_values: ");

    assert(parse_int32_ok("007") == 7);
    assert(parse_int32_ok("10") == 10);
    assert(parse_int32_ok("-42") == -42);
    assert(parse_int32_ok("-2147483648") == INT32_MIN);
    assert(parse_int32_ok("2147483647") == INT32_MAX);

    printf("PASS\n");
}

static void
test_parse_int32_failures(void)
{
    printf("test_parse_int32_failures: ");

    /* Failures must be reported as NULL, and must stay NULL regardless of the
     * value that would have been parsed. */
    assert(coal_parse_int32(box_string("")) == NULL);
    assert(coal_parse_int32(box_string(" ")) == NULL);
    assert(coal_parse_int32(box_string("+")) == NULL);
    assert(coal_parse_int32(box_string("abc")) == NULL);
    assert(coal_parse_int32(box_string("12abc")) == NULL);
    assert(coal_parse_int32(box_string("2147483648")) == NULL);
    assert(coal_parse_int32(box_string("-2147483649")) == NULL);
    assert(coal_parse_int32(box_string("99999999999999999999")) == NULL);

    printf("PASS\n");
}

static void
test_parse_int64_zero(void)
{
    printf("test_parse_int64_zero: ");

    assert(parse_int64_ok("0") == 0);
    assert(parse_int64_ok("00") == 0);
    assert(parse_int64_ok("-0") == 0);

    printf("PASS\n");
}

static void
test_parse_int64_values(void)
{
    printf("test_parse_int64_values: ");

    /* 4611686018427387904 is 1 << 62: a legitimate value that a 64-bit
     * "sentinel" scheme would have made unparseable. */
    assert(parse_int64_ok("4611686018427387904") == (int64_t) 4611686018427387904LL);
    assert(parse_int64_ok("9223372036854775807") == INT64_MAX);
    assert(parse_int64_ok("-9223372036854775808") == INT64_MIN);
    assert(parse_int64_ok("007") == 7);

    printf("PASS\n");
}

static void
test_parse_int64_failures(void)
{
    printf("test_parse_int64_failures: ");

    assert(coal_parse_int64(box_string("")) == NULL);
    assert(coal_parse_int64(box_string(" ")) == NULL);
    assert(coal_parse_int64(box_string("+")) == NULL);
    assert(coal_parse_int64(box_string("abc")) == NULL);
    assert(coal_parse_int64(box_string("9223372036854775808")) == NULL);
    assert(coal_parse_int64(box_string("-9223372036854775809")) == NULL);

    printf("PASS\n");
}

int
main(void)
{
    printf("Running parse tests...\n");

    test_parse_int32_zero();
    test_parse_int32_values();
    test_parse_int32_failures();
    test_parse_int64_zero();
    test_parse_int64_values();
    test_parse_int64_failures();

    printf("All parse tests passed!\n");
    return 0;
}
