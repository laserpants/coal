#ifndef COAL_VALUE_H
#define COAL_VALUE_H

#include <stdint.h>
#include <stdbool.h>

/**
 * Value representation and boxing.
 *
 * All runtime values use the rt_value_t type, which can represent:
 * - Inline values: int32, int64, bool, char (stored directly)
 * - Heap values: float, double, bignum, string, closure, record (pointers)
 */

/**
 * Boxed value type.
 * Can hold integers, booleans, characters, or pointers.
 */
typedef void *rt_value_t;

/* ============================================================================
 * Forward declarations
 * ============================================================================
 */

typedef struct rt_bignum rt_bignum_t;
typedef struct rt_string rt_string_t;
typedef struct rt_closure rt_closure_t;
typedef struct rt_record rt_record_t;
typedef struct rt_float rt_float_t;
typedef struct rt_double rt_double_t;

/* ============================================================================
 * Primitive type boxing
 * ============================================================================
 */

/** Box int32 into a value (no allocation) */
extern rt_value_t rt_int32_box(int32_t n);
/** Unbox int32 from a value */
extern int32_t rt_int32_unbox(rt_value_t v);

/** Box int64 into a value (no allocation) */
extern rt_value_t rt_int64_box(int64_t n);
/** Unbox int64 from a value */
extern int64_t rt_int64_unbox(rt_value_t v);

/** Box boolean into a value (no allocation) */
extern rt_value_t rt_bool_box(bool b);
/** Unbox boolean from a value */
extern bool rt_bool_unbox(rt_value_t v);

/** Box Unicode codepoint into a value (no allocation) */
extern rt_value_t rt_char_box(uint32_t cp);
/** Unbox Unicode codepoint from a value */
extern uint32_t rt_char_unbox(rt_value_t v);

/** Box raw pointer into a value */
extern rt_value_t rt_ptr_box(void *ptr);
/** Unbox raw pointer from a value */
extern void *rt_ptr_unbox(rt_value_t v);

/* ============================================================================
 * Heap-allocated type boxing
 * ============================================================================
 */

/** Box bignum pointer into a value */
extern rt_value_t rt_bignum_box(rt_bignum_t *bn);
/** Unbox bignum pointer from a value */
extern rt_bignum_t *rt_bignum_unbox(rt_value_t v);

/** Box string pointer into a value */
extern rt_value_t rt_string_box(rt_string_t *str);
/** Unbox string pointer from a value */
extern rt_string_t *rt_string_unbox(rt_value_t v);

/** Box closure pointer into a value */
extern rt_value_t rt_closure_box(rt_closure_t *closure);
/** Unbox closure pointer from a value */
extern rt_closure_t *rt_closure_unbox(rt_value_t v);

/** Box record pointer into a value */
extern rt_value_t rt_record_box(rt_record_t *record);
/** Unbox record pointer from a value */
extern rt_record_t *rt_record_unbox(rt_value_t v);

/* ============================================================================
 * Float/double boxing (requires heap allocation)
 * ============================================================================
 */

/**
 * Box a float into a value.
 * Requires heap allocation.
 *
 * Parameters:
 *   f - Float value to box
 *
 * Returns:
 *   Boxed value
 */
rt_value_t rt_float_box(float f);

/**
 * Unbox a float from a value.
 *
 * Parameters:
 *   v - Boxed value
 *
 * Returns:
 *   Float value
 */
float rt_float_unbox(rt_value_t v);

/**
 * Box a double into a value.
 * Requires heap allocation.
 *
 * Parameters:
 *   d - Double value to box
 *
 * Returns:
 *   Boxed value
 */
rt_value_t rt_double_box(double d);

/**
 * Unbox a double from a value.
 *
 * Parameters:
 *   v - Boxed value
 *
 * Returns:
 *   Double value
 */
double rt_double_unbox(rt_value_t v);

#endif
