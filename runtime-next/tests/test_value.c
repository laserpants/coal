#include "coal/value_api.h"
#include "coal/runtime.h"
#include <stdio.h>
#include <assert.h>
#include <string.h>

static void
test_int32_box_unbox(void)
{
    int32_t original = 42;
    rt_value_t boxed = rt_int32_box(original);
    int32_t unboxed = rt_int32_unbox(boxed);

    assert(original == unboxed);
    printf("test_int32_box_unbox: PASS (42)\n");
}

static void
test_int64_box_unbox(void)
{
    int64_t original = 9223372036854775807LL;
    rt_value_t boxed = rt_int64_box(original);
    int64_t unboxed = rt_int64_unbox(boxed);

    assert(original == unboxed);
    printf("test_int64_box_unbox: PASS (max int64)\n");
}

static void
test_bool_box_unbox(void)
{
    bool original_true = true;
    rt_value_t boxed_true = rt_bool_box(original_true);
    bool unboxed_true = rt_bool_unbox(boxed_true);

    assert(original_true == unboxed_true);

    bool original_false = false;
    rt_value_t boxed_false = rt_bool_box(original_false);
    bool unboxed_false = rt_bool_unbox(boxed_false);

    assert(original_false == unboxed_false);
    printf("test_bool_box_unbox: PASS (true/false)\n");
}

static void
test_ptr_box_unbox(void)
{
    int dummy = 123;
    void *original = &dummy;
    rt_value_t boxed = rt_ptr_box(original);
    void *unboxed = rt_ptr_unbox(boxed);

    assert(original == unboxed);
    printf("test_ptr_box_unbox: PASS (pointer)\n");
}

static void
test_value_array(void)
{
    /* Simulate passing mixed values through rt_value_t array */
    rt_value_t values[4];

    values[0] = rt_int32_box(100);
    values[1] = rt_int64_box(-500LL);
    values[2] = rt_bool_box(true);
    values[3] = rt_ptr_box((void *) 0x1234);

    /* Unbox and verify */
    assert(rt_int32_unbox(values[0]) == 100);
    assert(rt_int64_unbox(values[1]) == -500LL);
    assert(rt_bool_unbox(values[2]) == true);
    assert(rt_ptr_unbox(values[3]) == (void *) 0x1234);

    printf("test_value_array: PASS (mixed type array)\n");
}

static void
test_with_io_functions(void)
{
    /* Demonstrate typical LLVM codegen pattern */
    rt_value_t val = rt_int32_box(42);

    printf("test_with_io_functions: ");
    rt_print_int32(rt_int32_unbox(val));
    printf(" - PASS\n");
}

static void
test_bignum_box_unbox(void)
{
    rt_bignum_t *original = rt_bignum_from_i64(12345);
    rt_value_t boxed = rt_bignum_box(original);
    rt_bignum_t *unboxed = rt_bignum_unbox(boxed);

    assert(original == unboxed);
    assert(rt_bignum_cmp(original, unboxed) == 0);
    printf("test_bignum_box_unbox: PASS (bignum)\n");
}

static void
test_string_box_unbox(void)
{
    rt_string_t *original = rt_string_new("Hello, World!");
    rt_value_t boxed = rt_string_box(original);
    rt_string_t *unboxed = rt_string_unbox(boxed);

    assert(original == unboxed);
    assert(rt_string_length(original) == rt_string_length(unboxed));
    printf("test_string_box_unbox: PASS (string)\n");
}

static void
test_closure_box_unbox(void)
{
    /* Create a closure with a dummy function pointer */
    void *dummy_fn = (void *) 0x1000;
    rt_closure_t *original = rt_closure_new(dummy_fn, 2);
    rt_value_t boxed = rt_closure_box(original);
    rt_closure_t *unboxed = rt_closure_unbox(boxed);

    assert(original == unboxed);
    printf("test_closure_box_unbox: PASS (closure)\n");
}

static void
test_float_box_unbox(void)
{
    float original = 3.14159f;
    rt_value_t boxed = rt_float_box(original);
    float unboxed = rt_float_unbox(boxed);

    assert(original == unboxed);
    printf("test_float_box_unbox: PASS (float)\n");
}

static void
test_double_box_unbox(void)
{
    double original = 2.718281828459045;
    rt_value_t boxed = rt_double_box(original);
    double unboxed = rt_double_unbox(boxed);

    assert(original == unboxed);
    printf("test_double_box_unbox: PASS (double)\n");
}

static void
test_mixed_heap_and_primitive_array(void)
{
    /* Create values of different types */
    rt_bignum_t *bn = rt_bignum_from_i64(999);
    rt_string_t *str = rt_string_new("test");

    /* Store in rt_value_t array */
    rt_value_t values[6];
    values[0] = rt_int32_box(42);
    values[1] = rt_bignum_box(bn);
    values[2] = rt_string_box(str);
    values[3] = rt_bool_box(true);
    values[4] = rt_float_box(1.5f);
    values[5] = rt_double_box(2.75);

    /* Unbox and verify */
    assert(rt_int32_unbox(values[0]) == 42);
    assert(rt_bignum_unbox(values[1]) == bn);
    assert(rt_string_unbox(values[2]) == str);
    assert(rt_bool_unbox(values[3]) == true);
    assert(rt_float_unbox(values[4]) == 1.5f);
    assert(rt_double_unbox(values[5]) == 2.75);

    printf("test_mixed_heap_and_primitive_array: PASS (mixed types)\n");
}

static void
test_float_to_int_conversions(void)
{
    rt_value_t f = rt_float_box(42.7f);
    rt_value_t i32 = coal_float_to_int32(f);
    rt_value_t i64 = coal_float_to_int64(f);

    assert(rt_int32_unbox(i32) == 42);
    assert(rt_int64_unbox(i64) == 42);

    rt_value_t d = rt_double_box(123.9);
    i32 = coal_double_to_int32(d);
    i64 = coal_double_to_int64(d);

    assert(rt_int32_unbox(i32) == 123);
    assert(rt_int64_unbox(i64) == 123);

    printf("test_float_to_int_conversions: PASS (truncation)\n");
}

static void
test_int_to_float_conversions(void)
{
    rt_value_t i32 = rt_int32_box(100);
    rt_value_t f = coal_int32_to_float(i32);
    rt_value_t d = coal_int32_to_double(i32);

    assert(rt_float_unbox(f) == 100.0f);
    assert(rt_double_unbox(d) == 100.0);

    rt_value_t i64 = rt_int64_box(999999);
    f = coal_int64_to_float(i64);
    d = coal_int64_to_double(i64);

    assert(rt_float_unbox(f) == 999999.0f);
    assert(rt_double_unbox(d) == 999999.0);

    printf("test_int_to_float_conversions: PASS (promotion)\n");
}

static void
test_negative_conversions(void)
{
    rt_value_t f = rt_float_box(-42.5f);
    rt_value_t i32 = coal_float_to_int32(f);
    assert(rt_int32_unbox(i32) == -42);

    rt_value_t i64 = rt_int64_box(-999);
    rt_value_t d = coal_int64_to_double(i64);
    assert(rt_double_unbox(d) == -999.0);

    printf("test_negative_conversions: PASS (negative values)\n");
}

static void
test_float_double_conversions(void)
{
    rt_value_t f = rt_float_box(3.14159f);
    rt_value_t d = coal_float_to_double(f);
    assert(rt_double_unbox(d) > 3.14 && rt_double_unbox(d) < 3.15);

    rt_value_t d2 = rt_double_box(2.718281828459045);
    rt_value_t f2 = coal_double_to_float(d2);
    assert(rt_float_unbox(f2) > 2.71f && rt_float_unbox(f2) < 2.72f);

    printf("test_float_double_conversions: PASS (float <-> double)\n");
}

/* Tests for wrapper functions */

static void
test_io_wrappers(void)
{
    printf("test_io_wrappers: Testing IO wrappers...\n");

    rt_value_t i32 = rt_int32_box(42);
    rt_value_t f = rt_float_box(3.14f);
    rt_value_t b = rt_bool_box(true);

    printf("  ");
    coal_print_int32(i32);
    printf(" ");
    coal_print_float(f);
    printf(" ");
    coal_print_bool(b);
    printf(" - PASS\n");
}

static void
test_bignum_wrappers(void)
{
    rt_value_t a = rt_bignum_box(rt_bignum_from_i64(10));
    rt_value_t b = rt_bignum_box(rt_bignum_from_i64(20));

    /* Test arithmetic operations */
    rt_value_t sum = coal_bignum_add(a, b);
    rt_bignum_t *sum_bn = rt_bignum_unbox(sum);
    char *sum_str = rt_bignum_to_cstring(sum_bn);
    assert(strcmp(sum_str, "30") == 0);

    rt_value_t product = coal_bignum_mul(a, b);
    rt_bignum_t *prod_bn = rt_bignum_unbox(product);
    char *prod_str = rt_bignum_to_cstring(prod_bn);
    assert(strcmp(prod_str, "200") == 0);

    /* Test modulo */
    rt_value_t c = rt_bignum_box(rt_bignum_from_i64(17));
    rt_value_t d = rt_bignum_box(rt_bignum_from_i64(5));
    rt_value_t mod = coal_bignum_mod(c, d);
    rt_bignum_t *mod_bn = rt_bignum_unbox(mod);
    char *mod_str = rt_bignum_to_cstring(mod_bn);
    assert(strcmp(mod_str, "2") == 0);

    /* Test negation */
    rt_value_t neg_a = coal_bignum_neg(a);
    rt_bignum_t *neg_bn = rt_bignum_unbox(neg_a);
    char *neg_str = rt_bignum_to_cstring(neg_bn);
    assert(strcmp(neg_str, "-10") == 0);

    rt_value_t neg_neg_a = coal_bignum_neg(neg_a);
    rt_bignum_t *neg_neg_bn = rt_bignum_unbox(neg_neg_a);
    char *neg_neg_str = rt_bignum_to_cstring(neg_neg_bn);
    assert(strcmp(neg_neg_str, "10") == 0);

    /* Test comparisons */
    rt_value_t lt = coal_bignum_lt(a, b);
    assert(rt_bool_unbox(lt) == true);

    rt_value_t gt = coal_bignum_gt(b, a);
    assert(rt_bool_unbox(gt) == true);

    rt_value_t eq = coal_bignum_eq(a, a);
    assert(rt_bool_unbox(eq) == true);

    printf("test_bignum_wrappers: PASS (arithmetic & comparisons)\n");
}

static void
test_string_wrappers(void)
{
    rt_value_t str1 = rt_string_box(rt_string_new("Hello, "));
    rt_value_t str2 = rt_string_box(rt_string_new("World!"));

    rt_value_t concat = coal_string_concat(str1, str2);
    rt_string_t *result = rt_string_unbox(concat);
    assert(strcmp(rt_string_data(result), "Hello, World!") == 0);

    rt_value_t len = coal_string_length(concat);
    assert(rt_int64_unbox(len) == 13);

    // Test string equality
    rt_value_t str3 = rt_string_box(rt_string_new("Hello, World!"));
    rt_value_t equal = coal_string_equal(concat, str3);
    assert(rt_bool_unbox(equal) == true);

    rt_value_t not_equal = coal_string_equal(str1, str2);
    assert(rt_bool_unbox(not_equal) == false);

    // Test string reverse
    rt_value_t str4 = rt_string_box(rt_string_new("Hello"));
    rt_value_t reversed = coal_string_reverse(str4);
    rt_string_t *rev_result = rt_string_unbox(reversed);
    assert(strcmp(rt_string_data(rev_result), "olleH") == 0);

    // Test bool to string
    rt_value_t true_val = rt_bool_box(true);
    rt_value_t true_str = coal_bool_to_string(true_val);
    rt_string_t *true_result = rt_string_unbox(true_str);
    assert(strcmp(rt_string_data(true_result), "true") == 0);

    rt_value_t false_val = rt_bool_box(false);
    rt_value_t false_str = coal_bool_to_string(false_val);
    rt_string_t *false_result = rt_string_unbox(false_str);
    assert(strcmp(rt_string_data(false_result), "false") == 0);

    // Test int32_to_string
    rt_value_t int32_val = rt_int32_box(42);
    rt_value_t int32_str = coal_int32_to_string(int32_val);
    rt_string_t *int32_result = rt_string_unbox(int32_str);
    assert(strcmp(rt_string_data(int32_result), "42") == 0);

    // Test int64_to_string
    rt_value_t int64_val = rt_int64_box(-9999);
    rt_value_t int64_str = coal_int64_to_string(int64_val);
    rt_string_t *int64_result = rt_string_unbox(int64_str);
    assert(strcmp(rt_string_data(int64_result), "-9999") == 0);

    // Test float_to_string
    rt_value_t float_val = rt_float_box(3.14f);
    rt_value_t float_str = coal_float_to_string(float_val);
    rt_string_t *float_result = rt_string_unbox(float_str);
    assert(rt_string_data(float_result) != NULL);

    // Test double_to_string
    rt_value_t double_val = rt_double_box(2.718);
    rt_value_t double_str = coal_double_to_string(double_val);
    rt_string_t *double_result = rt_string_unbox(double_str);
    assert(rt_string_data(double_result) != NULL);

    printf("test_string_wrappers: PASS (concat, length, equality, reverse & "
           "conversions)\n");
}

static void
test_char_wrappers(void)
{
    rt_value_t digit = rt_char_box('5');
    rt_value_t letter = rt_char_box('A');
    rt_value_t lower = rt_char_box('a');

    /* Test predicates */
    rt_value_t is_digit = coal_char_is_digit(digit);
    assert(rt_bool_unbox(is_digit) == true);

    rt_value_t is_alpha = coal_char_is_alpha(letter);
    assert(rt_bool_unbox(is_alpha) == true);

    rt_value_t is_upper = coal_char_is_upper(letter);
    assert(rt_bool_unbox(is_upper) == true);

    /* Test conversions */
    rt_value_t upper = coal_char_to_upper(lower);
    assert(rt_char_unbox(upper) == 'A');

    rt_value_t lower2 = coal_char_to_lower(letter);
    assert(rt_char_unbox(lower2) == 'a');

    printf("test_char_wrappers: PASS (predicates & conversions)\n");
}

static void
test_bignum_conversion_wrappers(void)
{
    /* Test bignum to int32 */
    rt_value_t bn1 = rt_bignum_box(rt_bignum_from_i64(42));
    rt_value_t i32 = coal_bignum_to_int32(bn1);
    assert(rt_int32_unbox(i32) == 42);

    /* Test bignum to int64 */
    rt_value_t bn2 = rt_bignum_box(rt_bignum_from_i64(123456789));
    rt_value_t i64 = coal_bignum_to_int64(bn2);
    assert(rt_int64_unbox(i64) == 123456789);

    /* Test bignum to float */
    rt_value_t bn3 = rt_bignum_box(rt_bignum_from_i64(100));
    rt_value_t f = coal_bignum_to_float(bn3);
    assert(rt_float_unbox(f) == 100.0f);

    /* Test bignum to double */
    rt_value_t bn4 = rt_bignum_box(rt_bignum_from_i64(999));
    rt_value_t d = coal_bignum_to_double(bn4);
    assert(rt_double_unbox(d) == 999.0);

    /* Test with negative values */
    rt_value_t bn5 = rt_bignum_box(rt_bignum_from_i64(-42));
    rt_value_t i32_neg = coal_bignum_to_int32(bn5);
    assert(rt_int32_unbox(i32_neg) == -42);

    rt_value_t f_neg = coal_bignum_to_float(bn5);
    assert(rt_float_unbox(f_neg) == -42.0f);

    /* Test with large bignum */
    rt_value_t bn_large = rt_bignum_box(rt_bignum_new("123456789012345"));
    rt_value_t d_large = coal_bignum_to_double(bn_large);
    assert(rt_double_unbox(d_large) == 123456789012345.0);

    /* Test int32 to bignum */
    rt_value_t i32_val = rt_int32_box(42);
    rt_value_t bn_from_i32 = coal_int32_to_bignum(i32_val);
    rt_bignum_t *bn_i32 = rt_bignum_unbox(bn_from_i32);
    char *str_i32 = rt_bignum_to_cstring(bn_i32);
    assert(strcmp(str_i32, "42") == 0);

    /* Test int64 to bignum */
    rt_value_t i64_val = rt_int64_box(-999999);
    rt_value_t bn_from_i64 = coal_int64_to_bignum(i64_val);
    rt_bignum_t *bn_i64 = rt_bignum_unbox(bn_from_i64);
    char *str_i64 = rt_bignum_to_cstring(bn_i64);
    assert(strcmp(str_i64, "-999999") == 0);

    printf("test_bignum_conversion_wrappers: PASS (bignum to primitives)\n");
}

int
main(void)
{
    rt_runtime_init();

    printf("Running value box/unbox tests...\n");
    test_int32_box_unbox();
    test_int64_box_unbox();
    test_bool_box_unbox();
    test_ptr_box_unbox();
    test_value_array();
    test_with_io_functions();
    test_bignum_box_unbox();
    test_string_box_unbox();
    test_closure_box_unbox();
    test_float_box_unbox();
    test_double_box_unbox();
    test_mixed_heap_and_primitive_array();
    test_float_to_int_conversions();
    test_int_to_float_conversions();
    test_negative_conversions();
    test_float_double_conversions();

    printf("\nRunning wrapper tests...\n");
    test_io_wrappers();
    test_bignum_wrappers();
    test_bignum_conversion_wrappers();
    test_string_wrappers();
    test_char_wrappers();

    printf("All value tests passed!\n");

    return 0;
}
