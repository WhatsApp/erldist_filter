/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef SPINWAIT_H
#define SPINWAIT_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdatomic.h>
#include <stdbool.h>

#include "c11threads.h"
#include "portable_hint.h"

/* Type Definitions */

typedef struct spinwait_s spinwait_t;

struct spinwait_s {
    uint32_t counter;
};

/* Function Declarations */

static void spinwait_init(volatile spinwait_t *w);
static bool spinwait_try_spin(volatile spinwait_t *w);
static void spinwait_spin_no_yield(volatile spinwait_t *w);

/* Inline Function Definitions */

inline void
spinwait_init(volatile spinwait_t *w)
{
    w->counter = 0;
}

inline bool
spinwait_try_spin(volatile spinwait_t *w)
{
    if (w->counter >= 10) {
        return false;
    }
    w->counter += 1;
    if (w->counter <= 3) {
        int i = (1 << ((int)(w->counter)));
        while (i--) {
            HINT_SPIN_LOOP();
        }
    } else {
        (void)thrd_yield();
    }
    return true;
}

inline void
spinwait_spin_no_yield(volatile spinwait_t *w)
{
    int i;
    w->counter += 1;
    if (w->counter > 10) {
        w->counter = 10;
    }
    i = (1 << ((int)(w->counter)));
    while (i--) {
        HINT_SPIN_LOOP();
    }
    return;
}

#ifdef __cplusplus
}
#endif

#endif
