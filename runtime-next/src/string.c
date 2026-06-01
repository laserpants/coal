#include "coal/string.h"
#include "coal/bignum.h"
#include "coal/char.h"
#include "coal/gc.h"
#include "coal/panic.h"
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>

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

typedef struct rt_string {
    int64_t length;
    char data[];
} rt_string_t;

/* Verify flexible array member doesn't add to struct size */
_Static_assert(sizeof(rt_string_t) == sizeof(int64_t),
               "Flexible array should not add to struct size");

/* Get the length of a UTF-8 character from its first byte */
static int
utf8_char_len(unsigned char first_byte)
{
    if ((first_byte & 0x80) == 0) {
        return 1; /* 0xxxxxxx */
    }
    if ((first_byte & 0xE0) == 0xC0) {
        return 2; /* 110xxxxx */
    }
    if ((first_byte & 0xF0) == 0xE0) {
        return 3; /* 1110xxxx */
    }
    if ((first_byte & 0xF8) == 0xF0) {
        return 4; /* 11110xxx */
    }
    return 1; /* Invalid, treat as single byte */
}

/* Decode a UTF-8 character from a string */
static uint32_t
utf8_decode(const char *s)
{
    unsigned char first = (unsigned char) s[0];

    if ((first & 0x80) == 0) {
        return first;
    } else if ((first & 0xE0) == 0xC0) {
        return ((first & 0x1F) << 6) | ((unsigned char) s[1] & 0x3F);
    } else if ((first & 0xF0) == 0xE0) {
        return ((first & 0x0F) << 12) | (((unsigned char) s[1] & 0x3F) << 6) |
               ((unsigned char) s[2] & 0x3F);
    } else if ((first & 0xF8) == 0xF0) {
        return ((first & 0x07) << 18) | (((unsigned char) s[1] & 0x3F) << 12) |
               (((unsigned char) s[2] & 0x3F) << 6) |
               ((unsigned char) s[3] & 0x3F);
    }
    return first; /* Invalid, return as-is */
}

/* Encode a codepoint to UTF-8 */
static int
utf8_encode(uint32_t cp, char *buf)
{
    /* Validate Unicode codepoint range */
    if (cp > UTF8_4BYTE_MAX) {
        rt_panic("Invalid Unicode codepoint");
    }

    if (cp <= UTF8_1BYTE_MAX) {
        buf[0] = (char) cp;
        return 1;
    } else if (cp <= UTF8_2BYTE_MAX) {
        buf[0] = (char) (UTF8_2BYTE_PREFIX | (cp >> 6));
        buf[1] = (char) (UTF8_CONTINUATION | (cp & UTF8_CONTINUATION_MASK));
        return 2;
    } else if (cp <= UTF8_3BYTE_MAX) {
        buf[0] = (char) (UTF8_3BYTE_PREFIX | (cp >> 12));
        buf[1] =
            (char) (UTF8_CONTINUATION | ((cp >> 6) & UTF8_CONTINUATION_MASK));
        buf[2] = (char) (UTF8_CONTINUATION | (cp & UTF8_CONTINUATION_MASK));
        return 3;
    } else {
        buf[0] = (char) (UTF8_4BYTE_PREFIX | (cp >> 18));
        buf[1] =
            (char) (UTF8_CONTINUATION | ((cp >> 12) & UTF8_CONTINUATION_MASK));
        buf[2] =
            (char) (UTF8_CONTINUATION | ((cp >> 6) & UTF8_CONTINUATION_MASK));
        buf[3] = (char) (UTF8_CONTINUATION | (cp & UTF8_CONTINUATION_MASK));
        return 4;
    }
}

rt_string_t *
rt_string_new(const char *s)
{
    if (!s) {
        rt_panic("NULL string pointer in rt_string_new");
    }

    size_t len = strlen(s);

    /* Check for overflow: len + 1 and sizeof + len + 1 */
    if (len > SIZE_MAX - sizeof(rt_string_t) - 1) {
        rt_panic("String too large");
    }

    size_t size = sizeof(rt_string_t) + len + 1;
    rt_string_t *str = rt_alloc_atomic(size);
    if (!str) {
        rt_panic("Out of memory in rt_string_new");
    }

    str->length = (int64_t) len;
    memcpy(str->data, s, len + 1);

    return str;
}

rt_string_t *
rt_string_concat(const rt_string_t *a, const rt_string_t *b)
{
    if (!a || !b) {
        rt_panic("NULL string pointer in rt_string_concat");
    }

    /* Check for overflow in length addition */
    if (a->length > INT64_MAX - b->length) {
        rt_panic("String concat overflow");
    }

    int64_t new_len = a->length + b->length;

    /* Check for overflow in size calculation */
    if ((size_t) new_len > SIZE_MAX - sizeof(rt_string_t) - 1) {
        rt_panic("String too large");
    }

    size_t size = sizeof(rt_string_t) + (size_t) new_len + 1;
    rt_string_t *result = rt_alloc_atomic(size);
    if (!result) {
        rt_panic("Out of memory in rt_string_concat");
    }

    result->length = new_len;

    memcpy(result->data, a->data, (size_t) a->length);
    memcpy(result->data + a->length, b->data, (size_t) b->length);
    result->data[new_len] = '\0';

    return result;
}

int64_t
rt_string_length(const rt_string_t *s)
{
    return s->length;
}

char *
rt_string_data(const rt_string_t *s)
{
    return (char *) s->data;
}

bool
rt_string_compare(const rt_string_t *a, const rt_string_t *b)
{
    if (!a || !b) {
        rt_panic("NULL string pointer in rt_string_compare");
    }

    /* Quick length check - if lengths differ, strings are not equal */
    if (a->length != b->length) {
        return false;
    }

    const char *pa = a->data;
    const char *pb = b->data;

    /* Character-by-character comparison using UTF-8 decoding */
    while (*pa && *pb) {
        uint32_t ca = utf8_decode(pa);
        uint32_t cb = utf8_decode(pb);

        if (ca != cb) {
            return false;
        }

        /* Advance each pointer */
        pa += utf8_char_len((unsigned char) pa[0]);
        pb += utf8_char_len((unsigned char) pb[0]);
    }

    /* Both must end simultaneously */
    return *pa == '\0' && *pb == '\0';
}

rt_string_t *
rt_string_reverse(const rt_string_t *s)
{
    if (!s) {
        rt_panic("NULL string pointer in rt_string_reverse");
    }

    if (s->length == 0) {
        return rt_string_new("");
    }

    /* Count the number of characters (not bytes) */
    const char *p = s->data;
    int64_t char_count = 0;
    while (*p) {
        p += utf8_char_len((unsigned char) *p);
        char_count++;
    }

    /* Check for overflow before allocation */
    if ((size_t) char_count > SIZE_MAX / sizeof(uint32_t)) {
        rt_panic("String too large to reverse");
    }

    /* Allocate array to store codepoints */
    uint32_t *codepoints = rt_alloc((size_t) char_count * sizeof(uint32_t));
    if (!codepoints) {
        rt_panic("Out of memory in rt_string_reverse");
    }

    /* Decode all characters */
    p = s->data;
    for (int64_t i = 0; i < char_count; i++) {
        codepoints[i] = utf8_decode(p);
        p += utf8_char_len((unsigned char) *p);
    }

    /* Check for overflow in size calculation */
    if ((size_t) s->length > SIZE_MAX - sizeof(rt_string_t) - 1) {
        rt_panic("String too large");
    }

    /* Allocate result string (same byte length as original) */
    size_t size = sizeof(rt_string_t) + (size_t) s->length + 1;
    rt_string_t *result = rt_alloc_atomic(size);
    if (!result) {
        rt_panic("Out of memory in rt_string_reverse");
    }

    result->length = s->length;

    /* Encode characters in reverse order */
    char *dest = result->data;
    for (int64_t i = char_count - 1; i >= 0; i--) {
        int len = utf8_encode(codepoints[i], dest);
        dest += len;
    }
    *dest = '\0';

    return result;
}

rt_string_t *
rt_bool_to_string(bool b)
{
    return rt_string_new(b ? "true" : "false");
}

rt_string_t *
rt_int32_to_string(int32_t n)
{
    char buffer[32];
    snprintf(buffer, sizeof(buffer), "%d", n);
    return rt_string_new(buffer);
}

rt_string_t *
rt_int64_to_string(int64_t n)
{
    char buffer[32];
    snprintf(buffer, sizeof(buffer), "%lld", (long long) n);
    return rt_string_new(buffer);
}

rt_string_t *
rt_float_to_string(float f)
{
    char buffer[64];
    snprintf(buffer, sizeof(buffer), "%g", f);
    return rt_string_new(buffer);
}

rt_string_t *
rt_double_to_string(double d)
{
    char buffer[64];
    snprintf(buffer, sizeof(buffer), "%.15g", d);
    return rt_string_new(buffer);
}

rt_string_t *
rt_bignum_to_string(mpz_ptr n)
{
    return rt_string_new(mpz_get_str(NULL, 10, n));
}

rt_string_t *
rt_char_to_string(uint32_t cp)
{
    char buf[5];
    int len = utf8_encode(cp, buf);
    buf[len] = '\0';
    return rt_string_new(buf);
}

int32_t
rt_string_head(const rt_string_t *s)
{
    if (!s) {
        rt_panic("NULL string pointer in rt_string_head");
    }

    if (s->length == 0) {
        return 0; /* Empty string returns null character */
    }
    return (int32_t) utf8_decode(s->data);
}

rt_string_t *
rt_string_tail(const rt_string_t *s)
{
    if (!s) {
        rt_panic("NULL string pointer in rt_string_tail");
    }

    if (s->length == 0) {
        return rt_string_new(""); /* Empty string returns empty string */
    }

    /* Skip the first character */
    int skip = utf8_char_len((unsigned char) s->data[0]);

    /* Return the remaining string */
    return rt_string_new(s->data + skip);
}

/* Check if a UTF-8 character is whitespace */
static bool
is_utf8_whitespace(const unsigned char *s, int *char_len)
{
    unsigned char c = s[0];

    if ((c & 0x80) == 0x00) {
        /* ASCII range */
        *char_len = 1;
        return c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\v' ||
               c == '\f';
    } else if ((c & 0xE0) == 0xC0) {
        /* 2-byte UTF-8 */
        *char_len = 2;
        if (c == 0xC2 && s[1] == 0xA0) {
            return true; /* Non-breaking space (U+00A0) */
        }
    } else if ((c & 0xF0) == 0xE0) {
        /* 3-byte UTF-8 */
        *char_len = 3;
        if (c == 0xE1 && s[1] == 0x9A && s[2] == 0x80) {
            return true; /* Ogham space mark (U+1680) */
        }
        if (c == 0xE2) {
            /* Various Unicode spaces */
            if (s[1] == 0x80 && s[2] >= 0x80 && s[2] <= 0x8A) {
                return true; /* En quad (U+2000) to Hair space (U+200A) */
            }
            if (s[1] == 0x80 && s[2] == 0xAF) {
                return true; /* Narrow no-break space (U+202F) */
            }
            if (s[1] == 0x81 && s[2] == 0x9F) {
                return true; /* Medium mathematical space (U+205F) */
            }
            if (s[1] == 0x81 && s[2] == 0xA0) {
                return true; /* Ideographic space (U+3000) */
            }
        }
    } else if ((c & 0xF8) == 0xF0) {
        /* 4-byte UTF-8 - no common whitespace characters */
        *char_len = 4;
    }

    return false;
}

rt_string_t *
rt_string_remove_whitespace(const rt_string_t *s)
{
    if (!s) {
        rt_panic("NULL string pointer in rt_string_remove_whitespace");
    }

    const unsigned char *us = (const unsigned char *) s->data;
    size_t len = (size_t) s->length;

    /* Allocate output buffer same size as input (worst case) */
    char *result = rt_alloc_atomic(len + 1);
    if (!result) {
        rt_panic("Out of memory in rt_string_remove_whitespace");
    }

    size_t i = 0;
    size_t out_i = 0;

    while (i < len) {
        int char_len = utf8_char_len(us[i]);
        if (is_utf8_whitespace(us + i, &char_len)) {
            i += (size_t) char_len; /* Skip whitespace */
        } else {
            /* Copy non-whitespace character */
            for (size_t j = 0; j < (size_t) char_len && i + j < len; j++) {
                result[out_i++] = (char) us[i + j];
            }
            i += (size_t) char_len;
        }
    }

    result[out_i] = '\0';
    rt_string_t *ret = rt_string_new(result);
    return ret;
}
