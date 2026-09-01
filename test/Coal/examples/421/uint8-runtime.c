#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <stdint.h>

int32_t uint8_and(int32_t a, int32_t b)
{
    return a & b;
}

int32_t uint8_or(int32_t a, int32_t b)
{
    return a | b;
}

int32_t uint8_xor(int32_t a, int32_t b)
{
    return a ^ b;
}

int32_t uint8_not(int32_t a)
{
    return ~a;
}

int32_t uint8_shift_left(int32_t a, int32_t n)
{
    if (n < 0 || n >= 8) return 0;
    return (a << n) & 0xFF;
}

int32_t uint8_shift_right(int32_t a, int32_t n)
{
    if (n < 0 || n >= 8) return 0;
    /* a is always in 0..255, so the arithmetic shift equals a logical shift */
    return a >> n;
}
