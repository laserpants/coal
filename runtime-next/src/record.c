#include "coal/record.h"
#include "coal/gc.h"
#include "coal/panic.h"
#include <stdbool.h>
#include <stddef.h>
#include <string.h>

// Record structure using a simple linked list
typedef struct rt_record {
    const char *field;
    rt_value_t value;
    struct rt_record *next;
} rt_record_t;

rt_record_t *
rt_record_empty(void)
{
    return NULL;
}

rt_record_t *
rt_record_extend(rt_record_t *base, const char *field, rt_value_t value)
{
    if (!field) {
        rt_panic("NULL field name in rt_record_extend");
    }

    rt_record_t *new_record = rt_alloc(sizeof(rt_record_t));
    if (!new_record) {
        rt_panic("Out of memory in rt_record_extend");
    }

    /* Duplicate the field name to ensure it's GC-managed */
    size_t field_len = strlen(field) + 1;
    char *field_copy = rt_alloc_atomic(field_len);
    if (!field_copy) {
        rt_panic("Out of memory in rt_record_extend (field copy)");
    }
    memcpy(field_copy, field, field_len);

    new_record->field = field_copy;
    new_record->value = value;
    new_record->next = base;

    return new_record;
}

rt_value_t
rt_record_lookup(rt_record_t *record, const char *field)
{
    if (!field) {
        rt_panic("NULL field name in rt_record_lookup");
    }

    rt_record_t *current = record;

    while (current != NULL) {
        if (strcmp(current->field, field) == 0) {
            return current->value;
        }
        current = current->next;
    }

    /* Field not found - this indicates a compiler bug */
    rt_panic("Record field not found (compiler bug)");
}

bool
rt_record_has(rt_record_t *record, const char *field)
{
    if (!field) {
        return false;
    }

    rt_record_t *current = record;

    while (current != NULL) {
        if (strcmp(current->field, field) == 0) {
            return true;
        }
        current = current->next;
    }

    return false;
}
