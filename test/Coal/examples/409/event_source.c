#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/select.h>
#include <time.h>
#include <errno.h>

// ---------------------------------------------------------------------------
// Event source library primitives
//
// These implement the blocking select() and timer registry used by the
// EventSource module. They are application-independent.
//
// Blocking contract: coal_event_source_select must never return on a bare
// timeout. It blocks until a file descriptor is readable or a timer deadline
// has passed, then returns the index of the ready source. This keeps the
// continuation recursion in the Coal-layer select() bounded to constant depth.
// ---------------------------------------------------------------------------

#define MAX_TIMERS 64
#define MAX_SOURCES 64

typedef struct {
    int32_t id;
    int32_t interval_ms;
    struct timespec last_fire;
    int32_t active;
} coal_timer_t;

static coal_timer_t g_timers[MAX_TIMERS];
static int32_t g_next_timer_id = 1;

// Struct layout matching LLVM's Cons cell representation.
// LLVM struct: { i32, TPtr, TPtr } with natural alignment.
typedef struct {
    int32_t tag; /* 0 = Cons, 1 = Nil */
    void   *head;
    void   *tail;
} coal_cons_t;

static struct timespec now_monotonic(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts;
}

static long elapsed_ms(struct timespec from, struct timespec to)
{
    return (to.tv_sec - from.tv_sec) * 1000L +
           (to.tv_nsec - from.tv_nsec) / 1000000L;
}

// Register a timer that fires every `ms` milliseconds. Returns a timer id.
int32_t coal_event_source_register_timer(void *ms)
{
    int32_t interval = (int32_t)(uintptr_t)ms;
    for (int32_t i = 0; i < MAX_TIMERS; i++) {
        if (!g_timers[i].active) {
            g_timers[i].id = g_next_timer_id++;
            g_timers[i].interval_ms = interval;
            g_timers[i].last_fire = now_monotonic();
            g_timers[i].active = 1;
            return g_timers[i].id;
        }
    }
    return -1; /* no free timer slot */
}

// Poll a timer: return 1 if the interval has elapsed (and reset), else 0.
int32_t coal_event_source_poll_timer(void *id)
{
    int32_t timer_id = (int32_t)(uintptr_t)id;
    for (int32_t i = 0; i < MAX_TIMERS; i++) {
        if (g_timers[i].active && g_timers[i].id == timer_id) {
            struct timespec now = now_monotonic();
            if (elapsed_ms(g_timers[i].last_fire, now) >= g_timers[i].interval_ms) {
                g_timers[i].last_fire = now;
                return 1;
            }
            return 0;
        }
    }
    return 0;
}

// Compute the earliest timer deadline among the given timer ids.
// Returns remaining ms to the earliest deadline, or -1 if no active timers.
static long earliest_timer_remaining(int32_t *timer_ids, int32_t count)
{
    long earliest = -1;
    struct timespec now = now_monotonic();
    for (int32_t i = 0; i < count; i++) {
        for (int32_t t = 0; t < MAX_TIMERS; t++) {
            if (g_timers[t].active && g_timers[t].id == timer_ids[i]) {
                long remaining = g_timers[t].interval_ms -
                                 elapsed_ms(g_timers[t].last_fire, now);
                if (remaining < 0)
                    remaining = 0;
                if (earliest < 0 || remaining < earliest)
                    earliest = remaining;
                break;
            }
        }
    }
    return earliest;
}

// Check if any of the given timer ids has fired (interval elapsed).
// Returns the index of the first fired timer, or -1.
static int32_t first_fired_timer(int32_t *timer_ids, int32_t count)
{
    struct timespec now = now_monotonic();
    for (int32_t i = 0; i < count; i++) {
        for (int32_t t = 0; t < MAX_TIMERS; t++) {
            if (g_timers[t].active && g_timers[t].id == timer_ids[i]) {
                if (elapsed_ms(g_timers[t].last_fire, now) >= g_timers[t].interval_ms)
                    return i;
                break;
            }
        }
    }
    return -1;
}

// Block until an fd is readable or a timer fires. Returns the index of the
// ready source. Never returns on a bare timeout.
int32_t coal_event_source_select(void *fds_list, void *timer_ids_list)
{
    int32_t fds[MAX_SOURCES];
    int32_t timer_ids[MAX_SOURCES];
    int32_t count = 0;

    coal_cons_t *node = (coal_cons_t *)fds_list;
    while (node && count < MAX_SOURCES) {
        if (node->tag != 0)
            break; /* Nil */
        fds[count] = (int32_t)(uintptr_t)node->head;
        count++;
        node = (coal_cons_t *)node->tail;
    }

    int32_t timer_count = 0;
    node = (coal_cons_t *)timer_ids_list;
    while (node && timer_count < MAX_SOURCES) {
        if (node->tag != 0)
            break; /* Nil */
        timer_ids[timer_count] = (int32_t)(uintptr_t)node->head;
        timer_count++;
        node = (coal_cons_t *)node->tail;
    }

    while (true) {
        // If a timer has already fired, return its index.
        int32_t fired = first_fired_timer(timer_ids, timer_count);
        if (fired >= 0)
            return fired;

        // Compute the earliest timer deadline for the select timeout.
        long remaining = earliest_timer_remaining(timer_ids, timer_count);

        // Build the fd set.
        fd_set read_fds;
        FD_ZERO(&read_fds);
        int max_fd = -1;
        int has_fd = 0;
        for (int32_t i = 0; i < count; i++) {
            if (fds[i] >= 0) {
                has_fd = 1;
                if (fds[i] > max_fd)
                    max_fd = (int)fds[i];
                FD_SET((int)fds[i], &read_fds);
            }
        }

        struct timeval tv;
        struct timeval *tvp = NULL;
        if (remaining >= 0) {
            tv.tv_sec = remaining / 1000;
            tv.tv_usec = (remaining % 1000) * 1000;
            tvp = &tv;
        }

        int sel_ret;
        if (has_fd) {
            do {
                sel_ret = select(max_fd + 1, &read_fds, NULL, NULL, tvp);
            } while (sel_ret < 0 && errno == EINTR);
        } else {
            // No fds: sleep until the earliest timer deadline.
            if (remaining < 0) {
                // No fds and no timers: nothing to wait on. Return 0 to
                // avoid an infinite loop; the Coal layer will re-poll.
                return 0;
            }
            struct timespec ts;
            ts.tv_sec = remaining / 1000;
            ts.tv_nsec = (remaining % 1000) * 1000000L;
            nanosleep(&ts, NULL);
            sel_ret = 0;
        }

        if (sel_ret < 0)
            return 0; /* genuine error; let the Coal layer re-poll */

        // Check which fd is ready.
        for (int32_t i = 0; i < count; i++) {
            if (fds[i] >= 0 && FD_ISSET((int)fds[i], &read_fds))
                return i;
        }

        // No fd ready; loop and re-check timers.
    }
}