/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef SPINRWLOCK_H
#define SPINRWLOCK_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdatomic.h>

#include "portable_hint.h"

/* Type Definitions */

typedef struct spinrwlock_s spinrwlock_t;

struct spinrwlock_s {
    atomic_uint writer;
    atomic_uint n_readers;
};

/* Function Declarations */

static void spinrwlock_init(volatile spinrwlock_t *rw);
static void spinrwlock_read_lock(volatile spinrwlock_t *rw);
static void spinrwlock_read_unlock(volatile spinrwlock_t *rw);
static void spinrwlock_write_lock(volatile spinrwlock_t *rw);
static void spinrwlock_write_unlock(volatile spinrwlock_t *rw);

/* Inline Function Definitions */

inline void
spinrwlock_init(volatile spinrwlock_t *rw)
{
    (void)atomic_init(&rw->writer, 0);
    (void)atomic_init(&rw->n_readers, 0);
    (void)atomic_signal_fence(memory_order_seq_cst);
    return;
}

inline void
spinrwlock_read_lock(volatile spinrwlock_t *rw)
{
    for (;;) {
        while (atomic_load_explicit(&rw->writer, memory_order_relaxed) != 0) {
            HINT_SPIN_LOOP();
        }

        (void)atomic_fetch_add_explicit(&rw->n_readers, 1, memory_order_seq_cst);
        if (atomic_load_explicit(&rw->writer, memory_order_relaxed) == 0) {
            break;
        }
        (void)atomic_fetch_sub_explicit(&rw->n_readers, 1, memory_order_relaxed);
    }
    (void)atomic_thread_fence(memory_order_acquire);
    return;
}

inline void
spinrwlock_read_unlock(volatile spinrwlock_t *rw)
{
    (void)atomic_fetch_sub_explicit(&rw->n_readers, 1, memory_order_release);
    return;
}

inline void
spinrwlock_write_lock(volatile spinrwlock_t *rw)
{
    while (atomic_exchange_explicit(&rw->writer, 1, memory_order_relaxed) != 0) {
        HINT_SPIN_LOOP();
    }

    (void)atomic_thread_fence(memory_order_seq_cst);

    while (atomic_load_explicit(&rw->n_readers, memory_order_relaxed) != 0) {
        HINT_SPIN_LOOP();
    }

    (void)atomic_thread_fence(memory_order_acquire);
    return;
}

inline void
spinrwlock_write_unlock(volatile spinrwlock_t *rw)
{
    (void)atomic_store_explicit(&rw->writer, 0, memory_order_release);
    return;
}

#ifdef __cplusplus
}
#endif

#endif
