#include "coal/bignum.h"
#include "coal/gc.h"
#include "coal/panic.h"
#include <gmp.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

typedef struct rt_bignum {
    mpz_t value;
} rt_bignum_t;

rt_bignum_t *
rt_bignum_from_i64(int64_t n)
{
    rt_bignum_t *bn = rt_alloc(sizeof(rt_bignum_t));
    if (!bn) {
        rt_panic("Out of memory in rt_bignum_from_i64");
    }
    mpz_init_set_si(bn->value, n);
    return bn;
}

rt_bignum_t *
rt_bignum_new(const char *s)
{
    if (!s) {
        rt_panic("NULL string in rt_bignum_new");
    }

    rt_bignum_t *bn = rt_alloc(sizeof(rt_bignum_t));
    if (!bn) {
        rt_panic("Out of memory in rt_bignum_new");
    }

    if (mpz_init_set_str(bn->value, s, 10) != 0) {
        return NULL;
    }

    return bn;
}

rt_bignum_t *
rt_bignum_add(const rt_bignum_t *a, const rt_bignum_t *b)
{
    if (!a || !b) {
        rt_panic("NULL bignum in rt_bignum_add");
    }

    rt_bignum_t *result = rt_alloc(sizeof(rt_bignum_t));
    if (!result) {
        rt_panic("Out of memory in rt_bignum_add");
    }
    mpz_init(result->value);
    mpz_add(result->value, a->value, b->value);
    return result;
}

rt_bignum_t *
rt_bignum_sub(const rt_bignum_t *a, const rt_bignum_t *b)
{
    if (!a || !b) {
        rt_panic("NULL bignum in rt_bignum_sub");
    }

    rt_bignum_t *result = rt_alloc(sizeof(rt_bignum_t));
    if (!result) {
        rt_panic("Out of memory in rt_bignum_sub");
    }
    mpz_init(result->value);
    mpz_sub(result->value, a->value, b->value);
    return result;
}

rt_bignum_t *
rt_bignum_mul(const rt_bignum_t *a, const rt_bignum_t *b)
{
    if (!a || !b) {
        rt_panic("NULL bignum in rt_bignum_mul");
    }

    rt_bignum_t *result = rt_alloc(sizeof(rt_bignum_t));
    if (!result) {
        rt_panic("Out of memory in rt_bignum_mul");
    }
    mpz_init(result->value);
    mpz_mul(result->value, a->value, b->value);
    return result;
}

rt_bignum_t *
rt_bignum_div(const rt_bignum_t *a, const rt_bignum_t *b)
{
    if (!a || !b) {
        rt_panic("NULL bignum in rt_bignum_div");
    }

    rt_bignum_t *result = rt_alloc(sizeof(rt_bignum_t));
    if (!result) {
        rt_panic("Out of memory in rt_bignum_div");
    }
    mpz_init(result->value);
    mpz_tdiv_q(result->value, a->value, b->value);
    return result;
}

rt_bignum_t *
rt_bignum_neg(const rt_bignum_t *n)
{
    if (!n) {
        rt_panic("NULL bignum in rt_bignum_neg");
    }

    rt_bignum_t *result = rt_alloc(sizeof(rt_bignum_t));
    if (!result) {
        rt_panic("Out of memory in rt_bignum_neg");
    }
    mpz_init(result->value);
    mpz_neg(result->value, n->value);
    return result;
}

rt_bignum_t *
rt_bignum_mod(const rt_bignum_t *m, const rt_bignum_t *n)
{
    if (!m || !n) {
        rt_panic("NULL bignum in rt_bignum_mod");
    }

    rt_bignum_t *result = rt_alloc(sizeof(rt_bignum_t));
    if (!result) {
        rt_panic("Out of memory in rt_bignum_mod");
    }
    mpz_init(result->value);
    mpz_mod(result->value, m->value, n->value);
    return result;
}

int
rt_bignum_cmp(const rt_bignum_t *a, const rt_bignum_t *b)
{
    return mpz_cmp(a->value, b->value);
}

bool
rt_bignum_lt(const rt_bignum_t *a, const rt_bignum_t *b)
{
    return rt_bignum_cmp(a, b) < 0;
}

bool
rt_bignum_gt(const rt_bignum_t *a, const rt_bignum_t *b)
{
    return rt_bignum_cmp(a, b) > 0;
}

bool
rt_bignum_eq(const rt_bignum_t *a, const rt_bignum_t *b)
{
    return rt_bignum_cmp(a, b) == 0;
}

char *
rt_bignum_to_cstring(const rt_bignum_t *n)
{
    if (!n) {
        rt_panic("NULL bignum in rt_bignum_to_cstring");
    }
    return mpz_get_str(NULL, 10, n->value);
}

rt_bignum_t *
rt_int32_to_bignum(int32_t n)
{
    rt_bignum_t *bn = rt_alloc(sizeof(rt_bignum_t));
    if (!bn) {
        rt_panic("Out of memory in rt_int32_to_bignum");
    }
    mpz_init_set_si(bn->value, n);
    return bn;
}

rt_bignum_t *
rt_int64_to_bignum(int64_t n)
{
    rt_bignum_t *bn = rt_alloc(sizeof(rt_bignum_t));
    if (!bn) {
        rt_panic("Out of memory in rt_int64_to_bignum");
    }
    mpz_init_set_si(bn->value, n);
    return bn;
}

/*
 * Low 64 bits of the two's-complement truncation of n, i.e. the value of
 * n mod 2^64. mpz_get_si cannot be used for this: for positive values it
 * masks the result with LONG_MAX, which silently drops bit 63 (2^63 would
 * become 0). Out-of-range values are truncated, per the contract documented
 * in coal/bignum.h.
 */
static uint64_t
bignum_trunc_u64(const rt_bignum_t *n)
{
    mpz_t magnitude;
    uint64_t bits = 0;

    mpz_init(magnitude);
    mpz_abs(magnitude, n->value);
    mpz_tdiv_r_2exp(magnitude, magnitude, 64);
    /* magnitude < 2^64, so at most one 8-byte word is written into bits. */
    mpz_export(&bits, NULL, -1, sizeof bits, 0, 0, magnitude);
    mpz_clear(magnitude);

    if (mpz_sgn(n->value) < 0) {
        bits = (uint64_t) 0 - bits;
    }
    return bits;
}

int32_t
rt_bignum_to_int32(const rt_bignum_t *n)
{
    if (!n) {
        rt_panic("NULL bignum in rt_bignum_to_int32");
    }
    return (int32_t) (uint32_t) bignum_trunc_u64(n);
}

int64_t
rt_bignum_to_int64(const rt_bignum_t *n)
{
    if (!n) {
        rt_panic("NULL bignum in rt_bignum_to_int64");
    }
    return (int64_t) bignum_trunc_u64(n);
}

float
rt_bignum_to_float(const rt_bignum_t *n)
{
    if (!n) {
        rt_panic("NULL bignum in rt_bignum_to_float");
    }
    return (float) mpz_get_d(n->value);
}

double
rt_bignum_to_double(const rt_bignum_t *n)
{
    if (!n) {
        rt_panic("NULL bignum in rt_bignum_to_double");
    }
    return mpz_get_d(n->value);
}

mpz_ptr
rt_bignum_value(const rt_bignum_t *n)
{
    if (!n) {
        rt_panic("NULL bignum in rt_bignum_value");
    }
    return (mpz_ptr) n->value;
}
