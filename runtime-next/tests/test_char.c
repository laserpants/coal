#include "coal/char.h"
#include "coal/string.h"
#include "coal/runtime.h"
#include <stdio.h>
#include <assert.h>
#include <string.h>

static void
test_char_cmp(void)
{
    assert(rt_char_cmp('A', 'A') == 0);
    assert(rt_char_cmp('A', 'B') < 0);
    assert(rt_char_cmp('B', 'A') > 0);

    printf("test_char_cmp: PASS\n");
}

static void
test_char_is_digit(void)
{
    assert(rt_char_is_digit('0') == true);
    assert(rt_char_is_digit('9') == true);
    assert(rt_char_is_digit('a') == false);

    printf("test_char_is_digit: PASS\n");
}

static void
test_char_is_alpha(void)
{
    assert(rt_char_is_alpha('a') == true);
    assert(rt_char_is_alpha('Z') == true);
    assert(rt_char_is_alpha('5') == false);

    printf("test_char_is_alpha: PASS\n");
}

static void
test_char_is_whitespace(void)
{
    assert(rt_char_is_whitespace(' ') == true);
    assert(rt_char_is_whitespace('\n') == true);
    assert(rt_char_is_whitespace('a') == false);

    printf("test_char_is_whitespace: PASS\n");
}

static void
test_char_case(void)
{
    assert(rt_char_is_upper('A') == true);
    assert(rt_char_is_lower('a') == true);
    assert(rt_char_is_upper('a') == false);
    assert(rt_char_is_lower('A') == false);
    assert(rt_char_to_upper('a') == 'A');
    assert(rt_char_to_lower('A') == 'a');
    assert(rt_char_to_upper('A') == 'A');
    assert(rt_char_to_lower('a') == 'a');

    printf("test_char_case: PASS\n");
}

static void
test_char_to_string_ascii(void)
{
    rt_string_t *s = rt_char_to_string('H');
    assert(rt_string_length(s) == 1);
    assert(strcmp(rt_string_data(s), "H") == 0);

    printf("test_char_to_string_ascii: PASS\n");
}

static void
test_char_to_string_multibyte(void)
{
    /* U+00E9 LATIN SMALL LETTER E WITH ACUTE: 0xC3 0xA9 in UTF-8 */
    rt_string_t *s = rt_char_to_string(0x00E9U);
    assert(rt_string_length(s) == 2);
    assert((unsigned char) rt_string_data(s)[0] == 0xC3);
    assert((unsigned char) rt_string_data(s)[1] == 0xA9);

    printf("test_char_to_string_multibyte: PASS\n");
}

static void
test_char_to_string_emoji(void)
{
    /* U+1F600 GRINNING FACE: 0xF0 0x9F 0x98 0x80 in UTF-8 */
    rt_string_t *s = rt_char_to_string(0x1F600U);
    assert(rt_string_length(s) == 4);
    assert((unsigned char) rt_string_data(s)[0] == 0xF0);
    assert((unsigned char) rt_string_data(s)[1] == 0x9F);
    assert((unsigned char) rt_string_data(s)[2] == 0x98);
    assert((unsigned char) rt_string_data(s)[3] == 0x80);

    printf("test_char_to_string_emoji: PASS\n");
}

int
main(void)
{
    rt_runtime_init();

    printf("Running char tests...\n");
    test_char_cmp();
    test_char_is_digit();
    test_char_is_alpha();
    test_char_is_whitespace();
    test_char_case();
    test_char_to_string_ascii();
    test_char_to_string_multibyte();
    test_char_to_string_emoji();
    printf("All char tests passed!\n");

    return 0;
}
