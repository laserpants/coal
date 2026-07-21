#include "coal/record.h"
#include "coal/runtime.h"
#include "coal/value.h"
#include <stdio.h>
#include <assert.h>

static void
test_empty_record(void)
{
    rt_record_t *record = rt_record_empty();

    assert(record == NULL);
    assert(rt_record_has(record, "any_field") == false);

    printf("test_empty_record: PASS\n");
}

static void
test_single_field(void)
{
    rt_record_t *record = rt_record_empty();
    record = rt_record_extend(record, "name", rt_string_box(NULL));

    assert(record != NULL);
    assert(rt_record_has(record, "name") == true);
    assert(rt_record_has(record, "age") == false);

    printf("test_single_field: PASS\n");
}

static void
test_multiple_fields(void)
{
    rt_record_t *record = rt_record_empty();

    rt_value_t name_val = rt_int32_box(100);
    rt_value_t age_val = rt_int32_box(25);
    rt_value_t active_val = rt_bool_box(true);

    record = rt_record_extend(record, "name", name_val);
    record = rt_record_extend(record, "age", age_val);
    record = rt_record_extend(record, "active", active_val);

    assert(rt_record_has(record, "name") == true);
    assert(rt_record_has(record, "age") == true);
    assert(rt_record_has(record, "active") == true);
    assert(rt_record_has(record, "missing") == false);

    printf("test_multiple_fields: PASS\n");
}

static void
test_lookup_values(void)
{
    rt_record_t *record = rt_record_empty();

    rt_value_t val1 = rt_int32_box(42);
    rt_value_t val2 = rt_int64_box(9999999999LL);
    rt_value_t val3 = rt_bool_box(false);

    record = rt_record_extend(record, "x", val1);
    record = rt_record_extend(record, "y", val2);
    record = rt_record_extend(record, "flag", val3);

    rt_value_t retrieved_x = rt_record_lookup(record, "x");
    rt_value_t retrieved_y = rt_record_lookup(record, "y");
    rt_value_t retrieved_flag = rt_record_lookup(record, "flag");

    assert(rt_int32_unbox(retrieved_x) == 42);
    assert(rt_int64_unbox(retrieved_y) == 9999999999LL);
    assert(rt_bool_unbox(retrieved_flag) == false);

    printf("test_lookup_values: PASS\n");
}

static void
test_shadowing(void)
{
    // Later fields with the same name should shadow earlier ones
    rt_record_t *record = rt_record_empty();

    record = rt_record_extend(record, "x", rt_int32_box(10));
    record = rt_record_extend(record, "x", rt_int32_box(20));

    rt_value_t result = rt_record_lookup(record, "x");

    // Should get the most recently added value (20)
    assert(rt_int32_unbox(result) == 20);

    printf("test_shadowing: PASS (field shadowing)\n");
}

static void
test_extend_preserves_base(void)
{
    rt_record_t *record1 = rt_record_empty();
    record1 = rt_record_extend(record1, "a", rt_int32_box(1));
    record1 = rt_record_extend(record1, "b", rt_int32_box(2));

    rt_record_t *record2 = rt_record_extend(record1, "c", rt_int32_box(3));

    // record2 should have all three fields
    assert(rt_record_has(record2, "a") == true);
    assert(rt_record_has(record2, "b") == true);
    assert(rt_record_has(record2, "c") == true);

    // record1 should still only have a and b (immutability)
    assert(rt_record_has(record1, "a") == true);
    assert(rt_record_has(record1, "b") == true);
    assert(rt_record_has(record1, "c") == false);

    printf("test_extend_preserves_base: PASS (immutability)\n");
}

int
main(void)
{
    rt_runtime_init();

    printf("Running record tests...\n");
    test_empty_record();
    test_single_field();
    test_multiple_fields();
    test_lookup_values();
    test_shadowing();
    test_extend_preserves_base();
    printf("All record tests passed!\n");

    return 0;
}
