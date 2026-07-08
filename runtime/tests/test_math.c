#include "coal/math.h"
#include "coal/value_api.h"
#include "coal/runtime.h"
#include <stdio.h>
#include <assert.h>

static void
test_int32_mod_positive(void)
{
    assert(rt_int32_mod(10, 3) == 1);
    assert(rt_int32_mod(20, 7) == 6);
    assert(rt_int32_mod(100, 10) == 0);
    assert(rt_int32_mod(15, 4) == 3);
    assert(rt_int32_mod(7, 2) == 1);
    printf("test_int32_mod_positive: PASS\n");
}

static void
test_int32_mod_negative(void)
{
    /* C11 standard: result has sign of dividend (m) */
    assert(rt_int32_mod(-10, 3) == -1);
    assert(rt_int32_mod(10, -3) == 1);
    assert(rt_int32_mod(-10, -3) == -1);
    assert(rt_int32_mod(-7, 2) == -1);
    printf("test_int32_mod_negative: PASS\n");
}

static void
test_int32_mod_edge_cases(void)
{
    assert(rt_int32_mod(0, 5) == 0);
    assert(rt_int32_mod(5, 5) == 0);
    assert(rt_int32_mod(3, 10) == 3); /* m < n */
    assert(rt_int32_mod(1, 1) == 0);
    printf("test_int32_mod_edge_cases: PASS\n");
}

static void
test_int64_mod_positive(void)
{
    assert(rt_int64_mod(1000000000LL, 7LL) == 6LL);
    assert(rt_int64_mod(9223372036854775806LL, 10LL) == 6LL);
    assert(rt_int64_mod(100LL, 11LL) == 1LL);
    assert(rt_int64_mod(50LL, 3LL) == 2LL);
    printf("test_int64_mod_positive: PASS\n");
}

static void
test_int64_mod_negative(void)
{
    assert(rt_int64_mod(-1000000000LL, 7LL) == -6LL);
    assert(rt_int64_mod(1000000000LL, -7LL) == 6LL);
    assert(rt_int64_mod(-1000000000LL, -7LL) == -6LL);
    printf("test_int64_mod_negative: PASS\n");
}

static void
test_int64_mod_edge_cases(void)
{
    assert(rt_int64_mod(0LL, 100LL) == 0LL);
    assert(rt_int64_mod(7LL, 7LL) == 0LL);
    assert(rt_int64_mod(3LL, 100LL) == 3LL);
    printf("test_int64_mod_edge_cases: PASS\n");
}

static void
test_int32_mod_wrapper(void)
{
    rt_value_t m = rt_int32_box(17);
    rt_value_t n = rt_int32_box(5);
    rt_value_t result = coal_int32_mod(m, n);
    assert(rt_int32_unbox(result) == 2);

    m = rt_int32_box(100);
    n = rt_int32_box(9);
    result = coal_int32_mod(m, n);
    assert(rt_int32_unbox(result) == 1);

    m = rt_int32_box(-17);
    n = rt_int32_box(5);
    result = coal_int32_mod(m, n);
    assert(rt_int32_unbox(result) == -2);

    printf("test_int32_mod_wrapper: PASS\n");
}

static void
test_int64_mod_wrapper(void)
{
    rt_value_t m = rt_int64_box(1000000007LL);
    rt_value_t n = rt_int64_box(13LL);
    rt_value_t result = coal_int64_mod(m, n);
    assert(rt_int64_unbox(result) == 6LL);

    m = rt_int64_box(123456789LL);
    n = rt_int64_box(100LL);
    result = coal_int64_mod(m, n);
    assert(rt_int64_unbox(result) == 89LL);

    m = rt_int64_box(-1000000007LL);
    n = rt_int64_box(13LL);
    result = coal_int64_mod(m, n);
    assert(rt_int64_unbox(result) == -6LL);

    printf("test_int64_mod_wrapper: PASS\n");
}

int
main(void)
{
    rt_runtime_init();

    printf("Running math tests...\n");

    test_int32_mod_positive();
    test_int32_mod_negative();
    test_int32_mod_edge_cases();
    test_int64_mod_positive();
    test_int64_mod_negative();
    test_int64_mod_edge_cases();
    test_int32_mod_wrapper();
    test_int64_mod_wrapper();

    printf("All math tests passed!\n");
    return 0;
}
