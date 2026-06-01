#ifndef COAL_IO_H
#define COAL_IO_H

#include <stdint.h>
#include <stdbool.h>
#include <gmp.h>
#include "coal/string.h"

/**
 * Print a 32-bit integer to stdout.
 */
void rt_print_int32(int32_t n);

/**
 * Print a 64-bit integer to stdout.
 */
void rt_print_int64(int64_t n);

/**
 * Print a string to stdout.
 */
void rt_print_string(const char *s);

/**
 * Print a C string to stdout.
 */
void rt_print_string(const char *s);

/**
 * Print a Unicode character to stdout.
 * Encoded as UTF-8.
 */
void rt_print_char(uint32_t cp);

/**
 * Print a boolean to stdout.
 * Prints "true" or "false".
 */
void rt_print_bool(bool b);

/**
 * Print a float to stdout.
 */
void rt_print_float(float f);

/**
 * Print a double to stdout.
 */
void rt_print_double(double d);

/**
 * Print a bignum to stdout.
 */
void rt_print_bignum(mpz_t n);

/**
 * Print a 32-bit integer followed by newline.
 */
void rt_println_int32(int32_t n);

/**
 * Print a 64-bit integer followed by newline.
 */
void rt_println_int64(int64_t n);

/**
 * Print a string followed by newline.
 */
void rt_println_string(const char *s);

/**
 * Print a boolean followed by newline.
 */
void rt_println_bool(bool b);

/**
 * Print a Unicode character followed by newline.
 */
void rt_println_char(uint32_t cp);

/**
 * Print a float followed by newline.
 */
void rt_println_float(float f);

/**
 * Print a double followed by newline.
 */
void rt_println_double(double d);

/**
 * Print a bignum followed by newline.
 */
void rt_println_bignum(mpz_t n);

/**
 * Read a line from stdin.
 *
 * Returns:
 *   String without the newline character
 */
char *rt_readln(void);

/* File I/O result codes */
#define RT_READ_STATUS_OK 0
#define RT_READ_STATUS_FILE_NOT_FOUND 1
#define RT_READ_STATUS_IO_ERROR 2
#define RT_READ_STATUS_OUT_OF_MEMORY 3

#define RT_WRITE_STATUS_OK 0
#define RT_WRITE_STATUS_INVALID_INPUT 1
#define RT_WRITE_STATUS_IO_ERROR 2

/* Result type for file operations */
typedef struct rt_result {
    int32_t status; /* 0 = OK, non-zero = error */
    void *value;    /* valid iff status == 0 */
} rt_result_t;

/* Result accessors */
static inline int32_t
rt_result_status(rt_result_t *r)
{
    return r->status;
}

static inline char *
rt_result_value(rt_result_t *r)
{
    return (char *) r->value;
}

/**
 * Read the entire contents of a file.
 *
 * Parameters:
 *   filename - Path to file to read
 *
 * Returns:
 *   Result containing file contents as a null-terminated string,
 *   or error status if the operation failed
 */
rt_result_t *rt_read_file(const char *filename);

/**
 * Write data to a file.
 * Creates the file if it doesn't exist, overwrites if it does.
 *
 * Parameters:
 *   filename - Path to file to write
 *   data - Null-terminated string to write
 *
 * Returns:
 *   Result with status indicating success or failure
 */
rt_result_t *rt_write_file(const char *filename, const char *data);

#endif
