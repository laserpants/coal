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
    mpz_init_set_str(bn->value, s, 10);
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

mpz_ptr
rt_bignum_value(const rt_bignum_t *n)
{
    if (!n) {
        rt_panic("NULL bignum in rt_bignum_value");
    }
    return (mpz_ptr) n->value;
}
