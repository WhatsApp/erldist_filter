/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef STEADY_CLOCK_H
#define STEADY_CLOCK_H

#ifdef __cplusplus
extern "C" {
#endif

#include <inttypes.h>
#include <stdint.h>
#include <time.h>

/* Type Definitions */

typedef struct timespec steady_clock_t;

/* Macros */

#define STEADY_CLOCK_INIT (steady_clock_t){.tv_sec = 0, .tv_nsec = 0}

/* Function Declarations */

static void steady_clock_diff(steady_clock_t *diff, const steady_clock_t *start, const steady_clock_t *end);
static uint64_t steady_clock_diff_nsec(const steady_clock_t *start, const steady_clock_t *end);
static uint64_t steady_clock_diff_usec(const steady_clock_t *start, const steady_clock_t *end);
static void steady_clock_now(steady_clock_t *now);
static uint64_t steady_clock_to_nsec(const steady_clock_t *clock);
static uint64_t steady_clock_to_usec(const steady_clock_t *clock);

/* Inline Function Definitions */

inline void
steady_clock_diff(steady_clock_t *diff, const steady_clock_t *start, const steady_clock_t *end)
{
    if (end->tv_nsec < start->tv_nsec) {
        diff->tv_sec = end->tv_sec - start->tv_sec - 1;
        diff->tv_nsec = 1000000000LL + end->tv_nsec - start->tv_nsec;
    } else {
        diff->tv_sec = end->tv_sec - start->tv_sec;
        diff->tv_nsec = end->tv_nsec - start->tv_nsec;
    }
    return;
}

inline uint64_t
steady_clock_diff_nsec(const steady_clock_t *start, const steady_clock_t *end)
{
    steady_clock_t diff;
    (void)steady_clock_diff(&diff, start, end);
    return steady_clock_to_nsec(&diff);
}

inline uint64_t
steady_clock_diff_usec(const steady_clock_t *start, const steady_clock_t *end)
{
    steady_clock_t diff;
    (void)steady_clock_diff(&diff, start, end);
    return steady_clock_to_usec(&diff);
}

inline void
steady_clock_now(steady_clock_t *now)
{
// Use CLOCK_MONOTONIC_RAW if available to avoid NTP adjustments
#ifdef CLOCK_MONOTONIC_RAW
#define STEADY_CLOCK CLOCK_MONOTONIC_RAW
#else
#define STEADY_CLOCK CLOCK_MONOTONIC
#endif

    (void)clock_gettime(STEADY_CLOCK, now);
    return;

#undef STEADY_CLOCK
}

inline uint64_t
steady_clock_to_nsec(const steady_clock_t *clock)
{
    return (uint64_t)clock->tv_sec * 1000000000ULL + (uint64_t)clock->tv_nsec;
}

inline uint64_t
steady_clock_to_usec(const steady_clock_t *clock)
{
    return (uint64_t)clock->tv_sec * 1000000ULL + (uint64_t)clock->tv_nsec / 1000ULL;
}

#ifdef __cplusplus
}
#endif

#endif
