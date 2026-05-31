#include "coal/value.h"
#include "coal/gc.h"
#include "coal/panic.h"

typedef struct rt_float {
    float value;
} rt_float_t;

typedef struct rt_double {
    double value;
} rt_double_t;

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
