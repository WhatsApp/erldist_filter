/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef CORE_MUTEX_H
#define CORE_MUTEX_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdatomic.h>

#include "portable_hint.h"
#include "spinlock.h"

#include "xnif_trace.h"

// #define CORE_MUTEX_DEBUG 1
// #define CORE_MUTEX_SLOW 1

#ifdef CORE_MUTEX_DEBUG
#define CORE_MUTEX_TRACE_F(...) XNIF_TRACE_F(__VA_ARGS__)
#else
#define CORE_MUTEX_TRACE_F(...) ((void)(0))
#endif

#ifdef CORE_MUTEX_SLOW
#define CORE_MUTEX_VOLATILE
#else
#define CORE_MUTEX_VOLATILE volatile
#endif

/* Type Definitions */

typedef struct core_mutex_s core_mutex_t;

struct core_mutex_s {
#ifdef CORE_MUTEX_SLOW
    ErlNifMutex *inner;
#else
    spinlock_t inner;
    char *name;
#endif
};

/* Function Declarations */

static int core_mutex_create(CORE_MUTEX_VOLATILE core_mutex_t *mtx, char *name);
static void core_mutex_destroy(CORE_MUTEX_VOLATILE core_mutex_t *mtx);
static char *core_mutex_name(CORE_MUTEX_VOLATILE core_mutex_t *mtx);
static void core_mutex_lock(CORE_MUTEX_VOLATILE core_mutex_t *mtx);
static void core_mutex_unlock(CORE_MUTEX_VOLATILE core_mutex_t *mtx);

/* Inline Function Definitions */

inline int
core_mutex_create(CORE_MUTEX_VOLATILE core_mutex_t *mtx, char *name)
{
#ifdef CORE_MUTEX_SLOW
    mtx->inner = enif_mutex_create(name);
    if (mtx->inner == NULL) {
        return 0;
    }
    return 1;
#else
    (void)spinlock_init(&mtx->inner);
    mtx->name = name;
    return 1;
#endif
}

inline void
core_mutex_destroy(CORE_MUTEX_VOLATILE core_mutex_t *mtx)
{
#ifdef CORE_MUTEX_SLOW
    (void)enif_mutex_destroy(mtx->inner);
#else
    (void)spinlock_init(&mtx->inner);
    mtx->name = NULL;
#endif
    return;
}

inline char *
core_mutex_name(CORE_MUTEX_VOLATILE core_mutex_t *mtx)
{
#ifdef CORE_MUTEX_SLOW
    return enif_mutex_name(mtx->inner);
#else
    return mtx->name;
#endif
}

inline void
core_mutex_lock(CORE_MUTEX_VOLATILE core_mutex_t *mtx)
{
    CORE_MUTEX_TRACE_F("[%s:%p] core_mutex_lock ENTER\n", core_mutex_name(mtx), (void *)mtx);
#ifdef CORE_MUTEX_SLOW
    (void)enif_mutex_lock(mtx->inner);
#else
    (void)spinlock_lock(&mtx->inner);
#endif
    CORE_MUTEX_TRACE_F("[%s:%p] core_mutex_read_lock RETURN\n", core_mutex_name(mtx), (void *)mtx);
    return;
}

inline void
core_mutex_unlock(CORE_MUTEX_VOLATILE core_mutex_t *mtx)
{
    CORE_MUTEX_TRACE_F("[%s:%p] core_mutex_unlock ENTER\n", core_mutex_name(mtx), (void *)mtx);
#ifdef CORE_MUTEX_SLOW
    (void)enif_mutex_unlock(mtx->inner);
#else
    (void)spinlock_unlock(&mtx->inner);
#endif
    CORE_MUTEX_TRACE_F("[%s:%p] core_mutex_unlock RETURN\n", core_mutex_name(mtx), (void *)mtx);
    return;
}

#undef CORE_MUTEX_VOLATILE
#undef CORE_MUTEX_TRACE_F

#ifdef __cplusplus
}
#endif

#endif
