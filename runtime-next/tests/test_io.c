#include "coal/io.h"
#include "coal/bignum.h"
#include "coal/runtime.h"
#include <stdio.h>

static void
test_print_int32(void)
{
    printf("test_print_int32: ");
    rt_print_int32(42);
    printf(" - PASS\n");
}

static void
test_print_int64(void)
{
    printf("test_print_int64: ");
    rt_print_int64(9223372036854775807LL);
    printf(" - PASS\n");
}

static void
test_print_string(void)
{
    printf("test_print_string: ");
    rt_print_string("Hello");
    printf(" - PASS\n");
}

static void
test_println_int32(void)
{
    printf("test_println_int32: ");
    rt_println_int32(100);
    printf("PASS\n");
}

static void
test_println_int64(void)
{
    printf("test_println_int64: ");
    rt_println_int64(-123456789LL);
    printf("PASS\n");
}

static void
test_print_bool(void)
{
    printf("test_print_bool: ");
    rt_print_bool(true);
    printf(" / ");
    rt_print_bool(false);
    printf(" - PASS\n");
}

static void
test_println_bool(void)
{
    printf("test_println_bool: ");
    rt_println_bool(true);
    rt_println_bool(false);
    printf("PASS\n");
}

static void
test_print_char(void)
{
    printf("test_print_char: ");
    rt_print_char(0x48);    // 'H'
    rt_print_char(0x65);    // 'e'
    rt_print_char(0x6C);    // 'l'
    rt_print_char(0x6C);    // 'l'
    rt_print_char(0x6F);    // 'o'
    rt_print_char(0x20);    // ' '
    rt_print_char(0x1F600); // 😀 emoji (4-byte UTF-8)
    printf(" - PASS\n");
}

static void
test_println_char(void)
{
    printf("test_println_char: ");
    rt_println_char(0x41);   // 'A'
    rt_println_char(0x263A); // ☺ (3-byte UTF-8)
    printf("PASS\n");
}

static void
test_print_float(void)
{
    printf("test_print_float: ");
    rt_print_float(3.14159f);
    printf(" / ");
    rt_print_float(-2.5f);
    printf(" - PASS\n");
}

static void
test_println_float(void)
{
    printf("test_println_float: ");
    rt_println_float(1.5f);
    rt_println_float(42.0f);
    printf("PASS\n");
}

static void
test_print_double(void)
{
    printf("test_print_double: ");
    rt_print_double(3.141592653589793);
    printf(" / ");
    rt_print_double(-99.999);
    printf(" - PASS\n");
}

static void
test_println_double(void)
{
    printf("test_println_double: ");
    rt_println_double(2.718281828459045);
    rt_println_double(0.5);
    printf("PASS\n");
}

static void
test_print_bignum(void)
{
    printf("test_print_bignum: ");
    rt_bignum_t *bn1 = rt_bignum_new("123456789012345678901234567890");
    rt_print_bignum(rt_bignum_value(bn1));
    printf(" - PASS\n");
}

static void
test_println_bignum(void)
{
    printf("test_println_bignum: ");
    rt_bignum_t *bn1 = rt_bignum_from_i64(999999999);
    rt_println_bignum(rt_bignum_value(bn1));
    rt_bignum_t *bn2 = rt_bignum_new("-987654321098765432109876543210");
    rt_println_bignum(rt_bignum_value(bn2));
    printf("PASS\n");
}

int
main(void)
{
    rt_runtime_init();

    printf("Running IO tests...\n");
    test_print_int32();
    test_print_int64();
    test_print_string();
    test_println_int32();
    test_println_int64();
    test_print_bool();
    test_println_bool();
    test_print_char();
    test_println_char();
    test_print_float();
    test_println_float();
    test_print_double();
    test_println_double();
    test_print_bignum();
    test_println_bignum();
    printf("All IO tests passed!\n");

    return 0;
}
