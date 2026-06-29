#include "coal/io.h"
#include "coal/gc.h"
#include "coal/panic.h"

int32_t
rt_result_status(rt_result_t *r)
{
    return r->status;
}

char *
rt_result_value(rt_result_t *r)
{
    return (char *) r->value;
}
#include <stdio.h>
#include <inttypes.h>
#include <stdbool.h>
#include <string.h>
#include <gmp.h>
#include <errno.h>

/* UTF-8 encoding constants */
#define UTF8_1BYTE_MAX 0x7F
#define UTF8_2BYTE_MAX 0x7FF
#define UTF8_3BYTE_MAX 0xFFFF
#define UTF8_4BYTE_MAX 0x10FFFF
#define UTF8_2BYTE_PREFIX 0xC0
#define UTF8_3BYTE_PREFIX 0xE0
#define UTF8_4BYTE_PREFIX 0xF0
#define UTF8_CONTINUATION 0x80
#define UTF8_CONTINUATION_MASK 0x3F

void
rt_print_int32(int32_t n)
{
    printf("%" PRId32, n);
}

void
rt_print_int64(int64_t n)
{
    printf("%" PRId64, n);
}

void
rt_print_string(const char *s)
{
    if (!s) {
        rt_panic("NULL string in rt_print_string");
    }
    printf("%s", s);
}

void
rt_print_char(uint32_t cp)
{
    /* Validate Unicode codepoint range */
    if (cp > UTF8_4BYTE_MAX) {
        rt_panic("Invalid Unicode codepoint in rt_print_char");
    }

    if (cp <= UTF8_1BYTE_MAX) {
        putchar((int) cp);
    } else if (cp <= UTF8_2BYTE_MAX) {
        putchar(UTF8_2BYTE_PREFIX | (cp >> 6));
        putchar(UTF8_CONTINUATION | (cp & UTF8_CONTINUATION_MASK));
    } else if (cp <= UTF8_3BYTE_MAX) {
        putchar(UTF8_3BYTE_PREFIX | (cp >> 12));
        putchar(UTF8_CONTINUATION | ((cp >> 6) & UTF8_CONTINUATION_MASK));
        putchar(UTF8_CONTINUATION | (cp & UTF8_CONTINUATION_MASK));
    } else {
        putchar(UTF8_4BYTE_PREFIX | (cp >> 18));
        putchar(UTF8_CONTINUATION | ((cp >> 12) & UTF8_CONTINUATION_MASK));
        putchar(UTF8_CONTINUATION | ((cp >> 6) & UTF8_CONTINUATION_MASK));
        putchar(UTF8_CONTINUATION | (cp & UTF8_CONTINUATION_MASK));
    }
}

void
rt_println_int32(int32_t n)
{
    printf("%" PRId32 "\n", n);
}

void
rt_println_int64(int64_t n)
{
    printf("%" PRId64 "\n", n);
}

void
rt_println_string(const char *s)
{
    if (!s) {
        rt_panic("NULL string in rt_println_string");
    }

    printf("%s\n", s);
}

void
rt_print_bool(bool b)
{
    printf("%s", b ? "true" : "false");
}

void
rt_println_bool(bool b)
{
    printf("%s\n", b ? "true" : "false");
}

void
rt_println_char(uint32_t cp)
{
    rt_print_char(cp);
    putchar('\n');
}

void
rt_print_float(float f)
{
    printf("%f", f);
}

void
rt_println_float(float f)
{
    printf("%f\n", f);
}

void
rt_print_double(double d)
{
    printf("%.15f", d);
}

void
rt_println_double(double d)
{
    printf("%.15f\n", d);
}

void
rt_print_bignum(mpz_t n)
{
    gmp_printf("%Zd", n);
}

void
rt_println_bignum(mpz_t n)
{
    gmp_printf("%Zd\n", n);
}

char *
rt_readln(void)
{
    size_t capacity = 128;
    size_t length = 0;

    char *buffer = rt_alloc_atomic(capacity);

    int c;
    while ((c = fgetc(stdin)) != EOF) {
        if (c == '\n') {
            break;
        }

        /* Grow buffer if needed */
        if (length + 1 >= capacity) {
            size_t new_capacity = capacity * 2;
            char *new_buffer = rt_alloc_atomic(new_capacity);
            memcpy(new_buffer, buffer, length);
            buffer = new_buffer;
            capacity = new_capacity;
        }

        buffer[length++] = (char) c;
    }

    /* Null-terminate */
    buffer[length] = '\0';

    return buffer;
}

rt_result_t *
rt_read_file(const char *filename)
{
    rt_result_t *res = rt_alloc(sizeof(rt_result_t));
    if (!res) {
        /* Failure: runtime out of memory */
        return NULL;
    }

    FILE *file = fopen(filename, "rb");
    if (!file) {
        res->value = NULL;
        if (errno == ENOENT) {
            res->status = RT_READ_STATUS_FILE_NOT_FOUND;
        } else {
            res->status = RT_READ_STATUS_IO_ERROR;
        }
        return res;
    }

    /* Move to end to determine file size */
    if (fseek(file, 0, SEEK_END) != 0) {
        fclose(file);
        res->status = RT_READ_STATUS_IO_ERROR;
        res->value = NULL;
        return res;
    }

    long length = ftell(file);
    if (length < 0) {
        fclose(file);
        res->status = RT_READ_STATUS_IO_ERROR;
        res->value = NULL;
        return res;
    }

    rewind(file);

    /* Allocate buffer (+1 for null terminator) */
    char *buffer = rt_alloc_atomic((size_t) length + 1);
    if (!buffer) {
        fclose(file);
        res->status = RT_READ_STATUS_OUT_OF_MEMORY;
        res->value = NULL;
        return res;
    }

    /* Read file contents */
    size_t read_size = fread(buffer, 1, (size_t) length, file);
    fclose(file);

    if (read_size != (size_t) length) {
        res->status = RT_READ_STATUS_IO_ERROR;
        res->value = NULL;
        return res;
    }

    buffer[length] = '\0';

    /* Success */
    res->status = RT_READ_STATUS_OK;
    res->value = buffer;
    return res;
}

rt_result_t *
rt_write_file(const char *filename, const char *data)
{
    rt_result_t *res = rt_alloc(sizeof(rt_result_t));
    if (!res) {
        /* Failure: runtime out of memory */
        return NULL;
    }

    if (!filename || !data) {
        res->status = RT_WRITE_STATUS_INVALID_INPUT;
        res->value = NULL;
        return res;
    }

    FILE *file = fopen(filename, "wb");
    if (!file) {
        res->status = RT_WRITE_STATUS_IO_ERROR;
        res->value = NULL;
        return res;
    }

    size_t length = strlen(data);
    size_t written = fwrite(data, 1, length, file);

    if (written != length) {
        fclose(file);
        res->status = RT_WRITE_STATUS_IO_ERROR;
        res->value = NULL;
        return res;
    }

    fclose(file);

    /* Success */
    res->status = RT_WRITE_STATUS_OK;
    res->value = NULL; /* write operations don't return data */
    return res;
}
