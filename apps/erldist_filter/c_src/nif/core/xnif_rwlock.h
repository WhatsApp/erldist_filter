/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef XNIF_RWLOCK_H
#define XNIF_RWLOCK_H

#ifdef __cplusplus
extern "C" {
#endif

#include "../erl_nif_trampoline.h"

#include "../../primitive/spinrwlock.h"

#include "xnif_trace.h"

// #define XNIF_RWLOCK_DEBUG 1
// #define XNIF_RWLOCK_SLOW 1

#ifdef XNIF_RWLOCK_DEBUG
#define XNIF_RWLOCK_TRACE_F(...) XNIF_TRACE_F(__VA_ARGS__)
#else
#define XNIF_RWLOCK_TRACE_F(...) ((void)(0))
#endif

#ifdef XNIF_RWLOCK_SLOW
#define XNIF_RWLOCK_VOLATILE
#else
#define XNIF_RWLOCK_VOLATILE volatile
#endif

/* Type Definitions */

typedef struct xnif_rwlock_s xnif_rwlock_t;

struct xnif_rwlock_s {
#ifdef XNIF_RWLOCK_SLOW
    ErlNifRWLock *inner;
#else
    spinrwlock_t inner;
    char *name;
#endif
};

/* Function Declarations */

static int xnif_rwlock_create(XNIF_RWLOCK_VOLATILE xnif_rwlock_t *rw, char *name);
static void xnif_rwlock_destroy(XNIF_RWLOCK_VOLATILE xnif_rwlock_t *rw);
static char *xnif_rwlock_name(XNIF_RWLOCK_VOLATILE xnif_rwlock_t *rw);
static void xnif_rwlock_read_lock(XNIF_RWLOCK_VOLATILE xnif_rwlock_t *rw);
static void xnif_rwlock_read_unlock(XNIF_RWLOCK_VOLATILE xnif_rwlock_t *rw);
static void xnif_rwlock_write_lock(XNIF_RWLOCK_VOLATILE xnif_rwlock_t *rw);
static void xnif_rwlock_write_unlock(XNIF_RWLOCK_VOLATILE xnif_rwlock_t *rw);

/* Inline Function Definitions */

inline int
xnif_rwlock_create(XNIF_RWLOCK_VOLATILE xnif_rwlock_t *rw, char *name)
{
#ifdef XNIF_RWLOCK_SLOW
    rw->inner = enif_rwlock_create(name);
    if (rw->inner == NULL) {
        return 0;
    }
    return 1;
#else
    (void)spinrwlock_init(&rw->inner);
    rw->name = name;
    return 1;
#endif
}

inline void
xnif_rwlock_destroy(XNIF_RWLOCK_VOLATILE xnif_rwlock_t *rw)
{
#ifdef XNIF_RWLOCK_SLOW
    (void)enif_rwlock_destroy(rw->inner);
#else
    (void)spinrwlock_init(&rw->inner);
    rw->name = NULL;
#endif
    return;
}

inline char *
xnif_rwlock_name(XNIF_RWLOCK_VOLATILE xnif_rwlock_t *rw)
{
#ifdef XNIF_RWLOCK_SLOW
    return enif_rwlock_name(rw->inner);
#else
    return rw->name;
#endif
}

inline void
xnif_rwlock_read_lock(XNIF_RWLOCK_VOLATILE xnif_rwlock_t *rw)
{
    XNIF_RWLOCK_TRACE_F("[%s:%p] xnif_rwlock_read_lock ENTER\n", xnif_rwlock_name(rw), (void *)rw);
#ifdef XNIF_RWLOCK_SLOW
    (void)enif_rwlock_rlock(rw->inner);
#else
    (void)spinrwlock_read_lock(&rw->inner);
#endif
    XNIF_RWLOCK_TRACE_F("[%s:%p] xnif_rwlock_read_lock RETURN\n", xnif_rwlock_name(rw), (void *)rw);
    return;
}

inline void
xnif_rwlock_read_unlock(XNIF_RWLOCK_VOLATILE xnif_rwlock_t *rw)
{
    XNIF_RWLOCK_TRACE_F("[%s:%p] xnif_rwlock_read_unlock ENTER\n", xnif_rwlock_name(rw), (void *)rw);
#ifdef XNIF_RWLOCK_SLOW
    (void)enif_rwlock_runlock(rw->inner);
#else
    (void)spinrwlock_read_unlock(&rw->inner);
#endif
    XNIF_RWLOCK_TRACE_F("[%s:%p] xnif_rwlock_read_unlock RETURN\n", xnif_rwlock_name(rw), (void *)rw);
    return;
}

inline void
xnif_rwlock_write_lock(XNIF_RWLOCK_VOLATILE xnif_rwlock_t *rw)
{
    XNIF_RWLOCK_TRACE_F("[%s:%p] xnif_rwlock_write_lock ENTER\n", xnif_rwlock_name(rw), (void *)rw);
#ifdef XNIF_RWLOCK_SLOW
    (void)enif_rwlock_rwlock(rw->inner);
#else
    (void)spinrwlock_write_lock(&rw->inner);
#endif
    XNIF_RWLOCK_TRACE_F("[%s:%p] xnif_rwlock_write_lock RETURN\n", xnif_rwlock_name(rw), (void *)rw);
    return;
}

inline void
xnif_rwlock_write_unlock(XNIF_RWLOCK_VOLATILE xnif_rwlock_t *rw)
{
    XNIF_RWLOCK_TRACE_F("[%s:%p] xnif_rwlock_write_unlock ENTER\n", xnif_rwlock_name(rw), (void *)rw);
#ifdef XNIF_RWLOCK_SLOW
    (void)enif_rwlock_rwunlock(rw->inner);
#else
    (void)spinrwlock_write_unlock(&rw->inner);
#endif
    XNIF_RWLOCK_TRACE_F("[%s:%p] xnif_rwlock_write_unlock RETURN\n", xnif_rwlock_name(rw), (void *)rw);
    return;
}

#undef XNIF_RWLOCK_VOLATILE
#undef XNIF_RWLOCK_TRACE_F

#ifdef __cplusplus
}
#endif

#endif
