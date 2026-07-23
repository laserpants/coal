#include "coal/bignum.h"
#include "coal/runtime.h"
#include <stdio.h>
#include <assert.h>
#include <string.h>

static void
test_bignum_from_i64(void)
{
    rt_bignum_t *bn = rt_bignum_from_i64(42);
    assert(bn != NULL);

    char *str = rt_bignum_to_cstring(bn);
    assert(strcmp(str, "42") == 0);

    printf("test_bignum_from_i64: PASS\n");
}

static void
test_bignum_from_string(void)
{
    rt_bignum_t *bn = rt_bignum_new("123456789");
    assert(bn != NULL);

    char *str = rt_bignum_to_cstring(bn);
    assert(strcmp(str, "123456789") == 0);

    printf("test_bignum_from_string: PASS\n");
}

static void
test_bignum_add(void)
{
    rt_bignum_t *a = rt_bignum_from_i64(10);
    rt_bignum_t *b = rt_bignum_from_i64(20);
    rt_bignum_t *result = rt_bignum_add(a, b);

    char *str = rt_bignum_to_cstring(result);
    assert(strcmp(str, "30") == 0);

    printf("test_bignum_add: PASS\n");
}

static void
test_bignum_sub(void)
{
    rt_bignum_t *a = rt_bignum_from_i64(50);
    rt_bignum_t *b = rt_bignum_from_i64(30);
    rt_bignum_t *result = rt_bignum_sub(a, b);

    char *str = rt_bignum_to_cstring(result);
    assert(strcmp(str, "20") == 0);

    printf("test_bignum_sub: PASS\n");
}

static void
test_bignum_mul(void)
{
    rt_bignum_t *a = rt_bignum_from_i64(7);
    rt_bignum_t *b = rt_bignum_from_i64(6);
    rt_bignum_t *result = rt_bignum_mul(a, b);

    char *str = rt_bignum_to_cstring(result);
    assert(strcmp(str, "42") == 0);

    printf("test_bignum_mul: PASS\n");
}

static void
test_bignum_div(void)
{
    rt_bignum_t *a = rt_bignum_from_i64(100);
    rt_bignum_t *b = rt_bignum_from_i64(5);
    rt_bignum_t *result = rt_bignum_div(a, b);

    char *str = rt_bignum_to_cstring(result);
    assert(strcmp(str, "20") == 0);

    printf("test_bignum_div: PASS\n");
}

static void
test_bignum_neg(void)
{
    rt_bignum_t *a = rt_bignum_from_i64(42);
    rt_bignum_t *result = rt_bignum_neg(a);

    char *str = rt_bignum_to_cstring(result);
    assert(strcmp(str, "-42") == 0);

    rt_bignum_t *b = rt_bignum_from_i64(-99);
    rt_bignum_t *result2 = rt_bignum_neg(b);

    char *str2 = rt_bignum_to_cstring(result2);
    assert(strcmp(str2, "99") == 0);

    printf("test_bignum_neg: PASS\n");
}

static void
test_bignum_mod(void)
{
    rt_bignum_t *a = rt_bignum_from_i64(17);
    rt_bignum_t *b = rt_bignum_from_i64(5);
    rt_bignum_t *result = rt_bignum_mod(a, b);

    char *str = rt_bignum_to_cstring(result);
    assert(strcmp(str, "2") == 0);

    rt_bignum_t *c = rt_bignum_from_i64(100);
    rt_bignum_t *d = rt_bignum_from_i64(7);
    rt_bignum_t *result2 = rt_bignum_mod(c, d);

    char *str2 = rt_bignum_to_cstring(result2);
    assert(strcmp(str2, "2") == 0);

    printf("test_bignum_mod: PASS\n");
}

static void
test_bignum_cmp(void)
{
    rt_bignum_t *a = rt_bignum_from_i64(10);
    rt_bignum_t *b = rt_bignum_from_i64(20);
    rt_bignum_t *c = rt_bignum_from_i64(10);

    assert(rt_bignum_cmp(a, b) < 0);
    assert(rt_bignum_cmp(b, a) > 0);
    assert(rt_bignum_cmp(a, c) == 0);

    printf("test_bignum_cmp: PASS\n");
}

static void
test_bignum_lt(void)
{
    rt_bignum_t *a = rt_bignum_from_i64(10);
    rt_bignum_t *b = rt_bignum_from_i64(20);
    rt_bignum_t *c = rt_bignum_from_i64(10);

    assert(rt_bignum_lt(a, b) == true);
    assert(rt_bignum_lt(b, a) == false);
    assert(rt_bignum_lt(a, c) == false);

    printf("test_bignum_lt: PASS\n");
}

static void
test_bignum_gt(void)
{
    rt_bignum_t *a = rt_bignum_from_i64(10);
    rt_bignum_t *b = rt_bignum_from_i64(20);
    rt_bignum_t *c = rt_bignum_from_i64(10);

    assert(rt_bignum_gt(b, a) == true);
    assert(rt_bignum_gt(a, b) == false);
    assert(rt_bignum_gt(a, c) == false);

    printf("test_bignum_gt: PASS\n");
}

static void
test_bignum_eq(void)
{
    rt_bignum_t *a = rt_bignum_from_i64(10);
    rt_bignum_t *b = rt_bignum_from_i64(20);
    rt_bignum_t *c = rt_bignum_from_i64(10);

    assert(rt_bignum_eq(a, c) == true);
    assert(rt_bignum_eq(a, b) == false);
    assert(rt_bignum_eq(b, a) == false);

    printf("test_bignum_eq: PASS\n");
}

static void
test_int32_to_bignum(void)
{
    rt_bignum_t *bn1 = rt_int32_to_bignum(42);
    char *str1 = rt_bignum_to_cstring(bn1);
    assert(strcmp(str1, "42") == 0);

    rt_bignum_t *bn2 = rt_int32_to_bignum(-123);
    char *str2 = rt_bignum_to_cstring(bn2);
    assert(strcmp(str2, "-123") == 0);

    rt_bignum_t *bn3 = rt_int32_to_bignum(0);
    char *str3 = rt_bignum_to_cstring(bn3);
    assert(strcmp(str3, "0") == 0);

    printf("test_int32_to_bignum: PASS\n");
}

static void
test_int64_to_bignum(void)
{
    rt_bignum_t *bn1 = rt_int64_to_bignum(9223372036854775807LL);
    char *str1 = rt_bignum_to_cstring(bn1);
    assert(strcmp(str1, "9223372036854775807") == 0);

    rt_bignum_t *bn2 = rt_int64_to_bignum(-9223372036854775807LL);
    char *str2 = rt_bignum_to_cstring(bn2);
    assert(strcmp(str2, "-9223372036854775807") == 0);

    printf("test_int64_to_bignum: PASS\n");
}

int
main(void)
{
    rt_runtime_init();

    printf("Running bignum tests...\n");
    test_bignum_from_i64();
    test_bignum_from_string();
    test_bignum_add();
    test_bignum_sub();
    test_bignum_mul();
    test_bignum_div();
    test_bignum_neg();
    test_bignum_mod();
    test_bignum_cmp();
    test_bignum_lt();
    test_bignum_gt();
    test_bignum_eq();
    test_int32_to_bignum();
    test_int64_to_bignum();
    printf("All bignum tests passed!\n");

    return 0;
}
