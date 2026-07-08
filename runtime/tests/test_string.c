#include "coal/string.h"
#include "coal/char.h"
#include "coal/bignum.h"
#include "coal/runtime.h"
#include "coal/value_api.h"
#include <stdio.h>
#include <assert.h>
#include <string.h>

static void
test_string_new(void)
{
    rt_string_t *str = rt_string_new("Hello, World!");
    assert(str != NULL);
    assert(rt_string_length(str) == 13);
    assert(strcmp(rt_string_data(str), "Hello, World!") == 0);

    printf("test_string_new: PASS\n");
}

static void
test_string_concat(void)
{
    rt_string_t *a = rt_string_new("Hello, ");
    rt_string_t *b = rt_string_new("World!");
    rt_string_t *result = rt_string_concat(a, b);

    assert(rt_string_length(result) == 13);
    assert(strcmp(rt_string_data(result), "Hello, World!") == 0);

    printf("test_string_concat: PASS\n");
}

static void
test_string_length(void)
{
    rt_string_t *str = rt_string_new("Test");
    assert(rt_string_length(str) == 4);

    printf("test_string_length: PASS\n");
}

static void
test_char_to_string(void)
{
    // Test ASCII character
    rt_string_t *str1 = rt_char_to_string(0x41); // 'A'
    assert(rt_string_length(str1) == 1);
    assert(strcmp(rt_string_data(str1), "A") == 0);

    // Test 2-byte UTF-8 character
    rt_string_t *str2 = rt_char_to_string(0xE9); // é
    assert(rt_string_length(str2) == 2);
    assert(strcmp(rt_string_data(str2), "é") == 0);

    // Test 3-byte UTF-8 character
    rt_string_t *str3 = rt_char_to_string(0x263A); // ☺
    assert(rt_string_length(str3) == 3);
    assert(strcmp(rt_string_data(str3), "☺") == 0);

    // Test 4-byte UTF-8 character (emoji)
    rt_string_t *str4 = rt_char_to_string(0x1F600); // 😀
    assert(rt_string_length(str4) == 4);
    assert(strcmp(rt_string_data(str4), "😀") == 0);

    printf("test_char_to_string: PASS\n");
}

static void
test_string_compare(void)
{
    /* Test equal strings */
    rt_string_t *a1 = rt_string_new("Hello");
    rt_string_t *a2 = rt_string_new("Hello");
    assert(rt_string_compare(a1, a2) == true);

    /* Test different strings */
    rt_string_t *b1 = rt_string_new("Hello");
    rt_string_t *b2 = rt_string_new("World");
    assert(rt_string_compare(b1, b2) == false);

    /* Test different lengths */
    rt_string_t *c1 = rt_string_new("Hello");
    rt_string_t *c2 = rt_string_new("Hello!");
    assert(rt_string_compare(c1, c2) == false);

    /* Test empty strings */
    rt_string_t *d1 = rt_string_new("");
    rt_string_t *d2 = rt_string_new("");
    assert(rt_string_compare(d1, d2) == true);

    /* Test UTF-8 strings with multibyte characters */
    rt_string_t *e1 = rt_string_new("Hello 😀");
    rt_string_t *e2 = rt_string_new("Hello 😀");
    assert(rt_string_compare(e1, e2) == true);

    /* Test different UTF-8 strings */
    rt_string_t *f1 = rt_string_new("Hello 😀");
    rt_string_t *f2 = rt_string_new("Hello 😁");
    assert(rt_string_compare(f1, f2) == false);

    /* Test strings with different multibyte characters at start */
    rt_string_t *g1 = rt_string_new("café");
    rt_string_t *g2 = rt_string_new("cafe");
    assert(rt_string_compare(g1, g2) == false);

    /* Test identical multibyte strings */
    rt_string_t *h1 = rt_string_new("こんにちは"); /* Japanese: Hello */
    rt_string_t *h2 = rt_string_new("こんにちは");
    assert(rt_string_compare(h1, h2) == true);

    /* Test different multibyte strings */
    rt_string_t *i1 = rt_string_new("こんにちは");
    rt_string_t *i2 = rt_string_new("さようなら"); /* Japanese: Goodbye */
    assert(rt_string_compare(i1, i2) == false);

    /* Test with coal_string_compare wrapper */
    rt_value_t v1 = rt_string_box(rt_string_new("test"));
    rt_value_t v2 = rt_string_box(rt_string_new("test"));
    rt_value_t v3 = rt_string_box(rt_string_new("other"));
    assert(rt_bool_unbox(coal_string_compare(v1, v2)) == true);
    assert(rt_bool_unbox(coal_string_compare(v1, v3)) == false);

    printf("test_string_compare: PASS\n");
}

static void
test_string_reverse(void)
{
    // Test ASCII string
    rt_string_t *s1 = rt_string_new("Hello");
    rt_string_t *r1 = rt_string_reverse(s1);
    assert(strcmp(rt_string_data(r1), "olleH") == 0);
    assert(rt_string_length(r1) == 5);

    // Test empty string
    rt_string_t *s2 = rt_string_new("");
    rt_string_t *r2 = rt_string_reverse(s2);
    assert(strcmp(rt_string_data(r2), "") == 0);
    assert(rt_string_length(r2) == 0);

    // Test single character
    rt_string_t *s3 = rt_string_new("A");
    rt_string_t *r3 = rt_string_reverse(s3);
    assert(strcmp(rt_string_data(r3), "A") == 0);

    // Test UTF-8 multibyte characters
    rt_string_t *s4 = rt_string_new("Hello 😀");
    rt_string_t *r4 = rt_string_reverse(s4);
    assert(strcmp(rt_string_data(r4), "😀 olleH") == 0);

    // Test multiple UTF-8 characters
    rt_string_t *s5 = rt_string_new("A☺B");
    rt_string_t *r5 = rt_string_reverse(s5);
    assert(strcmp(rt_string_data(r5), "B☺A") == 0);

    printf("test_string_reverse: PASS\n");
}

static void
test_bool_to_string(void)
{
    // Test true
    rt_string_t *true_str = rt_bool_to_string(true);
    assert(strcmp(rt_string_data(true_str), "true") == 0);
    assert(rt_string_length(true_str) == 4);

    // Test false
    rt_string_t *false_str = rt_bool_to_string(false);
    assert(strcmp(rt_string_data(false_str), "false") == 0);
    assert(rt_string_length(false_str) == 5);

    printf("test_bool_to_string: PASS\n");
}

static void
test_to_string_conversions(void)
{
    // Test int32_to_string
    rt_string_t *int32_str = rt_int32_to_string(42);
    assert(strcmp(rt_string_data(int32_str), "42") == 0);

    rt_string_t *int32_neg = rt_int32_to_string(-123);
    assert(strcmp(rt_string_data(int32_neg), "-123") == 0);

    // Test int64_to_string
    rt_string_t *int64_str = rt_int64_to_string(9223372036854775807LL);
    assert(strcmp(rt_string_data(int64_str), "9223372036854775807") == 0);

    // Test float_to_string
    rt_string_t *float_str = rt_float_to_string(3.14159f);
    assert(rt_string_data(float_str) != NULL);

    // Test double_to_string
    rt_string_t *double_str = rt_double_to_string(2.718281828459045);
    assert(rt_string_data(double_str) != NULL);

    // Test bignum_to_string
    rt_bignum_t *bn = rt_bignum_new("123456789012345678901234567890");
    rt_string_t *bn_str = rt_bignum_to_string(rt_bignum_value(bn));
    assert(strcmp(rt_string_data(bn_str), "123456789012345678901234567890") ==
           0);

    // Test char_to_string
    rt_string_t *char_str = rt_char_to_string(0x1F600); // 😀
    assert(strcmp(rt_string_data(char_str), "😀") == 0);

    printf("test_to_string_conversions: PASS\n");
}

static void
test_string_head(void)
{
    /* Test ASCII character */
    rt_string_t *s1 = rt_string_new("Hello");
    assert(rt_string_head(s1) == 'H');

    /* Test UTF-8 2-byte character (é = U+00E9) */
    rt_string_t *s2 = rt_string_new("éllo");
    assert(rt_string_head(s2) == 0xE9);

    /* Test UTF-8 3-byte character (☺ = U+263A) */
    rt_string_t *s3 = rt_string_new("☺ABC");
    assert(rt_string_head(s3) == 0x263A);

    /* Test UTF-8 4-byte character (😀 = U+1F600) */
    rt_string_t *s4 = rt_string_new("😀XYZ");
    assert(rt_string_head(s4) == 0x1F600);

    /* Test empty string */
    rt_string_t *s5 = rt_string_new("");
    assert(rt_string_head(s5) == 0);

    /* Test single ASCII character */
    rt_string_t *s6 = rt_string_new("X");
    assert(rt_string_head(s6) == 'X');

    printf("test_string_head: PASS\n");
}

static void
test_string_tail(void)
{
    /* Test ASCII string */
    rt_string_t *s1 = rt_string_new("Hello");
    rt_string_t *t1 = rt_string_tail(s1);
    assert(strcmp(rt_string_data(t1), "ello") == 0);
    assert(rt_string_length(t1) == 4);

    /* Test UTF-8 2-byte character */
    rt_string_t *s2 = rt_string_new("éllo");
    rt_string_t *t2 = rt_string_tail(s2);
    assert(strcmp(rt_string_data(t2), "llo") == 0);

    /* Test UTF-8 3-byte character */
    rt_string_t *s3 = rt_string_new("☺ABC");
    rt_string_t *t3 = rt_string_tail(s3);
    assert(strcmp(rt_string_data(t3), "ABC") == 0);

    /* Test UTF-8 4-byte character (emoji) */
    rt_string_t *s4 = rt_string_new("😀XYZ");
    rt_string_t *t4 = rt_string_tail(s4);
    assert(strcmp(rt_string_data(t4), "XYZ") == 0);

    /* Test single character */
    rt_string_t *s5 = rt_string_new("X");
    rt_string_t *t5 = rt_string_tail(s5);
    assert(strcmp(rt_string_data(t5), "") == 0);
    assert(rt_string_length(t5) == 0);

    /* Test empty string */
    rt_string_t *s6 = rt_string_new("");
    rt_string_t *t6 = rt_string_tail(s6);
    assert(strcmp(rt_string_data(t6), "") == 0);
    assert(rt_string_length(t6) == 0);

    printf("test_string_tail: PASS\n");
}

static void
test_string_remove_whitespace(void)
{
    /* Test ASCII spaces */
    rt_string_t *s1 = rt_string_new("Hello World");
    rt_string_t *r1 = rt_string_remove_whitespace(s1);
    assert(strcmp(rt_string_data(r1), "HelloWorld") == 0);

    /* Test multiple types of whitespace */
    rt_string_t *s2 = rt_string_new("Hello\t\n\r World");
    rt_string_t *r2 = rt_string_remove_whitespace(s2);
    assert(strcmp(rt_string_data(r2), "HelloWorld") == 0);

    /* Test leading and trailing whitespace */
    rt_string_t *s3 = rt_string_new("  Hello  ");
    rt_string_t *r3 = rt_string_remove_whitespace(s3);
    assert(strcmp(rt_string_data(r3), "Hello") == 0);

    /* Test all whitespace */
    rt_string_t *s4 = rt_string_new("   \t\n\r   ");
    rt_string_t *r4 = rt_string_remove_whitespace(s4);
    assert(strcmp(rt_string_data(r4), "") == 0);

    /* Test no whitespace */
    rt_string_t *s5 = rt_string_new("HelloWorld");
    rt_string_t *r5 = rt_string_remove_whitespace(s5);
    assert(strcmp(rt_string_data(r5), "HelloWorld") == 0);

    /* Test empty string */
    rt_string_t *s6 = rt_string_new("");
    rt_string_t *r6 = rt_string_remove_whitespace(s6);
    assert(strcmp(rt_string_data(r6), "") == 0);

    /* Test UTF-8 non-breaking space (U+00A0) */
    rt_string_t *s7 = rt_string_new("Hello\xC2\xA0World");
    rt_string_t *r7 = rt_string_remove_whitespace(s7);
    assert(strcmp(rt_string_data(r7), "HelloWorld") == 0);

    /* Test with UTF-8 characters and whitespace */
    rt_string_t *s8 = rt_string_new("Hello 😀 World");
    rt_string_t *r8 = rt_string_remove_whitespace(s8);
    assert(strcmp(rt_string_data(r8), "Hello😀World") == 0);

    printf("test_string_remove_whitespace: PASS\n");
}

static void
test_string_wrappers(void)
{
    /* Test coal_string_head wrapper */
    rt_value_t str1 = rt_string_box(rt_string_new("Hello"));
    rt_value_t head = coal_string_head(str1);
    assert(rt_int32_unbox(head) == 'H');

    rt_value_t str2 = rt_string_box(rt_string_new("😀XYZ"));
    rt_value_t head2 = coal_string_head(str2);
    assert(rt_int32_unbox(head2) == 0x1F600);

    /* Test coal_string_tail wrapper */
    rt_value_t str3 = rt_string_box(rt_string_new("Hello"));
    rt_value_t tail = coal_string_tail(str3);
    rt_string_t *tail_str = rt_string_unbox(tail);
    assert(strcmp(rt_string_data(tail_str), "ello") == 0);

    /* Test coal_string_remove_whitespace wrapper */
    rt_value_t str4 = rt_string_box(rt_string_new("Hello World"));
    rt_value_t no_ws = coal_string_remove_whitespace(str4);
    rt_string_t *no_ws_str = rt_string_unbox(no_ws);
    assert(strcmp(rt_string_data(no_ws_str), "HelloWorld") == 0);

    printf("test_string_wrappers: PASS\n");
}

int
main(void)
{
    rt_runtime_init();

    printf("Running string tests...\n");
    test_string_new();
    test_string_concat();
    test_string_length();
    test_char_to_string();
    test_string_compare();
    test_string_reverse();
    test_bool_to_string();
    test_to_string_conversions();
    test_string_head();
    test_string_tail();
    test_string_remove_whitespace();
    test_string_wrappers();
    printf("All string tests passed!\n");

    return 0;
}
