/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef XNIF_MUTEX_H
#define XNIF_MUTEX_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdatomic.h>

#include "../../primitive/portable_hint.h"
#include "../../primitive/spinlock.h"

#include "xnif_trace.h"

// #define XNIF_MUTEX_DEBUG 1
// #define XNIF_MUTEX_SLOW 1

#ifdef XNIF_MUTEX_DEBUG
#define XNIF_MUTEX_TRACE_F(...) XNIF_TRACE_F(__VA_ARGS__)
#else
#define XNIF_MUTEX_TRACE_F(...) ((void)(0))
#endif

#ifdef XNIF_MUTEX_SLOW
#define XNIF_MUTEX_VOLATILE
#else
#define XNIF_MUTEX_VOLATILE volatile
#endif

#ifdef XNIF_MUTEX_SLOW
#define XNIF_MUTEX_INIT {.inner = NULL}
#else
#define XNIF_MUTEX_INIT {.inner = {.value = ATOMIC_FLAG_INIT}, .name = NULL}
#endif

/* Type Definitions */

typedef struct xnif_mutex_s xnif_mutex_t;

struct xnif_mutex_s {
#ifdef XNIF_MUTEX_SLOW
    ErlNifMutex *inner;
#else
    spinlock_t inner;
    char *name;
#endif
};

/* Function Declarations */

static int xnif_mutex_create(XNIF_MUTEX_VOLATILE xnif_mutex_t *mtx, char *name);
static void xnif_mutex_destroy(XNIF_MUTEX_VOLATILE xnif_mutex_t *mtx);
static char *xnif_mutex_name(XNIF_MUTEX_VOLATILE xnif_mutex_t *mtx);
static void xnif_mutex_lock(XNIF_MUTEX_VOLATILE xnif_mutex_t *mtx);
static void xnif_mutex_unlock(XNIF_MUTEX_VOLATILE xnif_mutex_t *mtx);

/* Inline Function Definitions */

inline int
xnif_mutex_create(XNIF_MUTEX_VOLATILE xnif_mutex_t *mtx, char *name)
{
#ifdef XNIF_MUTEX_SLOW
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
xnif_mutex_destroy(XNIF_MUTEX_VOLATILE xnif_mutex_t *mtx)
{
#ifdef XNIF_MUTEX_SLOW
    (void)enif_mutex_destroy(mtx->inner);
#else
    (void)spinlock_init(&mtx->inner);
    mtx->name = NULL;
#endif
    return;
}

inline char *
xnif_mutex_name(XNIF_MUTEX_VOLATILE xnif_mutex_t *mtx)
{
#ifdef XNIF_MUTEX_SLOW
    return enif_mutex_name(mtx->inner);
#else
    return mtx->name;
#endif
}

inline void
xnif_mutex_lock(XNIF_MUTEX_VOLATILE xnif_mutex_t *mtx)
{
    XNIF_MUTEX_TRACE_F("[%s:%p] xnif_mutex_lock ENTER\n", xnif_mutex_name(mtx), (void *)mtx);
#ifdef XNIF_MUTEX_SLOW
    (void)enif_mutex_lock(mtx->inner);
#else
    (void)spinlock_lock(&mtx->inner);
#endif
    XNIF_MUTEX_TRACE_F("[%s:%p] xnif_mutex_read_lock RETURN\n", xnif_mutex_name(mtx), (void *)mtx);
    return;
}

inline void
xnif_mutex_unlock(XNIF_MUTEX_VOLATILE xnif_mutex_t *mtx)
{
    XNIF_MUTEX_TRACE_F("[%s:%p] xnif_mutex_unlock ENTER\n", xnif_mutex_name(mtx), (void *)mtx);
#ifdef XNIF_MUTEX_SLOW
    (void)enif_mutex_unlock(mtx->inner);
#else
    (void)spinlock_unlock(&mtx->inner);
#endif
    XNIF_MUTEX_TRACE_F("[%s:%p] xnif_mutex_unlock RETURN\n", xnif_mutex_name(mtx), (void *)mtx);
    return;
}

#undef XNIF_MUTEX_VOLATILE
#undef XNIF_MUTEX_TRACE_F

#ifdef __cplusplus
}
#endif

#endif
