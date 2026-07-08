#include "coal/runtime.h"
#include "coal/value_api.h"
#include <stdio.h>
#include <assert.h>

/* Test rt_float_random returns values in valid range */
static void
test_float_random_range(void)
{
    /* Generate multiple random floats and verify range */
    for (int i = 0; i < 100; i++) {
        float r = rt_float_random();
        assert(r >= 0.0f);
        assert(r <= 1.0f);
    }

    printf("test_float_random_range: PASS (100 values in [0.0, 1.0])\n");
}

/* Test rt_double_random returns values in valid range */
static void
test_double_random_range(void)
{
    /* Generate multiple random doubles and verify range */
    for (int i = 0; i < 100; i++) {
        double r = rt_double_random();
        assert(r >= 0.0);
        assert(r <= 1.0);
    }

    printf("test_double_random_range: PASS (100 values in [0.0, 1.0])\n");
}

/* Test that multiple calls produce different values (statistical test) */
static void
test_float_random_variation(void)
{
    /* Generate 10 values and check they're not all the same */
    float first = rt_float_random();
    int different_count = 0;

    for (int i = 0; i < 10; i++) {
        float r = rt_float_random();
        if (r != first) {
            different_count++;
        }
    }

    /* At least one value should be different (very high probability) */
    assert(different_count > 0);

    printf("test_float_random_variation: PASS (%d/10 values differ)\n",
           different_count);
}

/* Test that multiple calls produce different values (statistical test) */
static void
test_double_random_variation(void)
{
    /* Generate 10 values and check they're not all the same */
    double first = rt_double_random();
    int different_count = 0;

    for (int i = 0; i < 10; i++) {
        double r = rt_double_random();
        if (r != first) {
            different_count++;
        }
    }

    /* At least one value should be different (very high probability) */
    assert(different_count > 0);

    printf("test_double_random_variation: PASS (%d/10 values differ)\n",
           different_count);
}

/* Test distribution is roughly uniform (statistical test) */
static void
test_float_random_distribution(void)
{
    /* Count how many values fall in each half of [0, 1] */
    int lower_half = 0; /* [0.0, 0.5) */
    int upper_half = 0; /* [0.5, 1.0] */

    for (int i = 0; i < 1000; i++) {
        float r = rt_float_random();
        if (r < 0.5f) {
            lower_half++;
        } else {
            upper_half++;
        }
    }

    /* With 1000 samples, both halves should have some values
     * (very high probability) */
    assert(lower_half > 0);
    assert(upper_half > 0);

    /* Check roughly balanced (allow 35-65% in each half) */
    assert(lower_half >= 350 && lower_half <= 650);
    assert(upper_half >= 350 && upper_half <= 650);

    printf("test_float_random_distribution: PASS (lower=%d, upper=%d)\n",
           lower_half, upper_half);
}

/* Test distribution is roughly uniform (statistical test) */
static void
test_double_random_distribution(void)
{
    /* Count how many values fall in each half of [0, 1] */
    int lower_half = 0; /* [0.0, 0.5) */
    int upper_half = 0; /* [0.5, 1.0] */

    for (int i = 0; i < 1000; i++) {
        double r = rt_double_random();
        if (r < 0.5) {
            lower_half++;
        } else {
            upper_half++;
        }
    }

    /* With 1000 samples, both halves should have some values */
    assert(lower_half > 0);
    assert(upper_half > 0);

    /* Check roughly balanced (allow 35-65% in each half) */
    assert(lower_half >= 350 && lower_half <= 650);
    assert(upper_half >= 350 && upper_half <= 650);

    printf("test_double_random_distribution: PASS (lower=%d, upper=%d)\n",
           lower_half, upper_half);
}

/* Test wrapper function coal_float_random */
static void
test_float_random_wrapper(void)
{
    /* Generate values using wrapper and verify range */
    for (int i = 0; i < 50; i++) {
        rt_value_t r_boxed = coal_float_random();
        float r = rt_float_unbox(r_boxed);
        assert(r >= 0.0f);
        assert(r <= 1.0f);
    }

    printf("test_float_random_wrapper: PASS (50 boxed values in range)\n");
}

/* Test wrapper function coal_double_random */
static void
test_double_random_wrapper(void)
{
    /* Generate values using wrapper and verify range */
    for (int i = 0; i < 50; i++) {
        rt_value_t r_boxed = coal_double_random();
        double r = rt_double_unbox(r_boxed);
        assert(r >= 0.0);
        assert(r <= 1.0);
    }

    printf("test_double_random_wrapper: PASS (50 boxed values in range)\n");
}

/* Test that wrapper produces different values */
static void
test_wrapper_variation(void)
{
    rt_value_t first_boxed = coal_float_random();
    float first = rt_float_unbox(first_boxed);

    int different_count = 0;
    for (int i = 0; i < 10; i++) {
        rt_value_t r_boxed = coal_float_random();
        float r = rt_float_unbox(r_boxed);
        if (r != first) {
            different_count++;
        }
    }

    assert(different_count > 0);

    printf("test_wrapper_variation: PASS (%d/10 wrapped values differ)\n",
           different_count);
}

int
main(void)
{
    rt_runtime_init();

    printf("Running random number generation tests...\n");

    /* Basic range tests */
    test_float_random_range();
    test_double_random_range();

    /* Variation tests */
    test_float_random_variation();
    test_double_random_variation();

    /* Distribution tests */
    test_float_random_distribution();
    test_double_random_distribution();

    /* Wrapper tests */
    test_float_random_wrapper();
    test_double_random_wrapper();
    test_wrapper_variation();

    printf("All random number generation tests passed!\n");

    return 0;
}
