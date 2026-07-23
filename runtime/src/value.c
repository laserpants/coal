#include "coal/value.h"
#include "coal/gc.h"
#include "coal/panic.h"

typedef struct rt_float {
    float value;
} rt_float_t;

typedef struct rt_double {
    double value;
} rt_double_t;

/* ============================================================================
 * Primitive type boxing
 * ============================================================================
 */

rt_value_t
rt_int32_box(int32_t n)
{
    return (void *) (uintptr_t) n;
}

int32_t
rt_int32_unbox(rt_value_t v)
{
    return (int32_t) (uintptr_t) v;
}

rt_value_t
rt_int64_box(int64_t n)
{
    return (void *) (uintptr_t) n;
}

int64_t
rt_int64_unbox(rt_value_t v)
{
    return (int64_t) (uintptr_t) v;
}

rt_value_t
rt_bool_box(bool b)
{
    return (void *) (uintptr_t) b;
}

bool
rt_bool_unbox(rt_value_t v)
{
    return (bool) (uintptr_t) v;
}

rt_value_t
rt_char_box(uint32_t cp)
{
    return (void *) (uintptr_t) cp;
}

uint32_t
rt_char_unbox(rt_value_t v)
{
    return (uint32_t) (uintptr_t) v;
}

rt_value_t
rt_ptr_box(void *ptr)
{
    return ptr;
}

void *
rt_ptr_unbox(rt_value_t v)
{
    return v;
}

/* ============================================================================
 * Heap-allocated type boxing
 * ============================================================================
 */

rt_value_t
rt_bignum_box(rt_bignum_t *bn)
{
    return (rt_value_t) bn;
}

rt_bignum_t *
rt_bignum_unbox(rt_value_t v)
{
    return (rt_bignum_t *) v;
}

rt_value_t
rt_string_box(rt_string_t *str)
{
    return (rt_value_t) str;
}

rt_string_t *
rt_string_unbox(rt_value_t v)
{
    return (rt_string_t *) v;
}

rt_value_t
rt_closure_box(rt_closure_t *closure)
{
    return (rt_value_t) closure;
}

rt_closure_t *
rt_closure_unbox(rt_value_t v)
{
    return (rt_closure_t *) v;
}

rt_value_t
rt_record_box(rt_record_t *record)
{
    return (rt_value_t) record;
}

rt_record_t *
rt_record_unbox(rt_value_t v)
{
    return (rt_record_t *) v;
}

rt_value_t
rt_float_box(float f)
{
    rt_float_t *boxed = rt_alloc_atomic(sizeof(rt_float_t));
    if (!boxed) {
        rt_panic("Out of memory in rt_float_box");
    }
    boxed->value = f;
    return (rt_value_t) boxed;
}

float
rt_float_unbox(rt_value_t v)
{
    rt_float_t *boxed = v;
    return boxed->value;
}

rt_value_t
rt_double_box(double d)
{
    rt_double_t *boxed = rt_alloc_atomic(sizeof(rt_double_t));
    if (!boxed) {
        rt_panic("Out of memory in rt_double_box");
    }
    boxed->value = d;
    return (rt_value_t) boxed;
}

double
rt_double_unbox(rt_value_t v)
{
    rt_double_t *boxed = v;
    return boxed->value;
}
