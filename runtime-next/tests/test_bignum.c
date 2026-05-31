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
    test_bignum_cmp();
    test_bignum_lt();
    test_bignum_gt();
    test_bignum_eq();
    printf("All bignum tests passed!\n");

    return 0;
}
