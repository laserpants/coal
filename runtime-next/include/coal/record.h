#ifndef COAL_RECORD_H
#define COAL_RECORD_H

#include "value.h"
#include <stdbool.h>

typedef struct rt_record rt_record_t;

/**
 * Create an empty record.
 *
 * Returns:
 *   Empty record
 */
rt_record_t *rt_record_empty(void);

/**
 * Extend a record with a new field.
 * Does not modify the original record.
 *
 * Parameters:
 *   base - Existing record (or NULL for empty)
 *   field - Field name
 *   value - Field value
 *
 * Returns:
 *   New record with the additional field
 */
rt_record_t *rt_record_extend(rt_record_t *base, const char *field,
                              rt_value_t value);

/**
 * Look up a field value in a record.
 *
 * Parameters:
 *   record - Record to search
 *   field - Field name to find
 *
 * Returns:
 *   Field value
 *
 * Panics:
 *   If field is not found
 */
rt_value_t rt_record_lookup(rt_record_t *record, const char *field);

/**
 * Check if a record has a field.
 *
 * Parameters:
 *   record - Record to search
 *   field - Field name to check
 *
 * Returns:
 *   true if field exists
 */
bool rt_record_has(rt_record_t *record, const char *field);

#endif
