#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <stdint.h>
#include <time.h>

/**
 * Seconds since the Unix epoch (1970-01-01T00:00:00Z).
 *
 * FFI: #{unix_time : unit -> int64}
 */
int64_t unix_time(void *_)
{
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    return (int64_t)ts.tv_sec;
}

/**
 * Nanoseconds since the Unix epoch (1970-01-01T00:00:00Z).
 *
 * Valid until approximately 2262-04-11, beyond which the 64-bit
 * nanosecond count overflows.
 *
 * FFI: #{coal_clock_realtime : unit -> int64}
 */
int64_t coal_clock_realtime(void *_)
{
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    return (int64_t)ts.tv_sec * 1000000000LL + (int64_t)ts.tv_nsec;
}

/**
 * Nanoseconds on the monotonic clock. Suitable for measuring elapsed
 * time; the absolute value has no defined relationship to wall-clock
 * time and is not affected by clock adjustments.
 *
 * FFI: #{coal_clock_monotonic : unit -> int64}
 */
int64_t coal_clock_monotonic(void *_)
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (int64_t)ts.tv_sec * 1000000000LL + (int64_t)ts.tv_nsec;
}

/**
 * Truncating integer division (C semantics). Exposed to Coal because
 * the compiler does not provide a `Divisible<int64>` instance.
 *
 * FFI: #{coal_int64_div : int64 -> int64 -> int64}
 */
int64_t coal_int64_div(int64_t a, int64_t b)
{
    return a / b;
}

/**
 * Widen a 32-bit signed integer to 64 bits.
 *
 * FFI: #{coal_int32_to_int64 : int32 -> int64}
 */
int64_t coal_int32_to_int64(int32_t n)
{
    return (int64_t)n;
}

/**
 * Narrow a 64-bit signed integer to 32 bits (truncating).
 *
 * FFI: #{coal_int64_to_int32 : int64 -> int32}
 */
int32_t coal_int64_to_int32(int64_t n)
{
    return (int32_t)n;
}
