/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef CORE_RWLOCK_H
#define CORE_RWLOCK_H

#ifdef __cplusplus
extern "C" {
#endif

#include <erl_nif.h>

#include "spinrwlock.h"

#include "xnif_trace.h"

// #define CORE_RWLOCK_DEBUG 1
// #define CORE_RWLOCK_SLOW 1

#ifdef CORE_RWLOCK_DEBUG
#define CORE_RWLOCK_TRACE_F(...) XNIF_TRACE_F(__VA_ARGS__)
#else
#define CORE_RWLOCK_TRACE_F(...) ((void)(0))
#endif

#ifdef CORE_RWLOCK_SLOW
#define CORE_RWLOCK_VOLATILE
#else
#define CORE_RWLOCK_VOLATILE volatile
#endif

/* Type Definitions */

typedef struct core_rwlock_s core_rwlock_t;

struct core_rwlock_s {
#ifdef CORE_RWLOCK_SLOW
    ErlNifRWLock *inner;
#else
    spinrwlock_t inner;
    char *name;
#endif
};

/* Function Declarations */

static int core_rwlock_create(CORE_RWLOCK_VOLATILE core_rwlock_t *rw, char *name);
static void core_rwlock_destroy(CORE_RWLOCK_VOLATILE core_rwlock_t *rw);
static char *core_rwlock_name(CORE_RWLOCK_VOLATILE core_rwlock_t *rw);
static void core_rwlock_read_lock(CORE_RWLOCK_VOLATILE core_rwlock_t *rw);
static void core_rwlock_read_unlock(CORE_RWLOCK_VOLATILE core_rwlock_t *rw);
static void core_rwlock_write_lock(CORE_RWLOCK_VOLATILE core_rwlock_t *rw);
static void core_rwlock_write_unlock(CORE_RWLOCK_VOLATILE core_rwlock_t *rw);

/* Inline Function Definitions */

inline int
core_rwlock_create(CORE_RWLOCK_VOLATILE core_rwlock_t *rw, char *name)
{
#ifdef CORE_RWLOCK_SLOW
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
core_rwlock_destroy(CORE_RWLOCK_VOLATILE core_rwlock_t *rw)
{
#ifdef CORE_RWLOCK_SLOW
    (void)enif_rwlock_destroy(rw->inner);
#else
    (void)spinrwlock_init(&rw->inner);
    rw->name = NULL;
#endif
    return;
}

inline char *
core_rwlock_name(CORE_RWLOCK_VOLATILE core_rwlock_t *rw)
{
#ifdef CORE_RWLOCK_SLOW
    return enif_rwlock_name(rw->inner);
#else
    return rw->name;
#endif
}

inline void
core_rwlock_read_lock(CORE_RWLOCK_VOLATILE core_rwlock_t *rw)
{
    CORE_RWLOCK_TRACE_F("[%s:%p] core_rwlock_read_lock ENTER\n", core_rwlock_name(rw), (void *)rw);
#ifdef CORE_RWLOCK_SLOW
    (void)enif_rwlock_rlock(rw->inner);
#else
    (void)spinrwlock_read_lock(&rw->inner);
#endif
    CORE_RWLOCK_TRACE_F("[%s:%p] core_rwlock_read_lock RETURN\n", core_rwlock_name(rw), (void *)rw);
    return;
}

inline void
core_rwlock_read_unlock(CORE_RWLOCK_VOLATILE core_rwlock_t *rw)
{
    CORE_RWLOCK_TRACE_F("[%s:%p] core_rwlock_read_unlock ENTER\n", core_rwlock_name(rw), (void *)rw);
#ifdef CORE_RWLOCK_SLOW
    (void)enif_rwlock_runlock(rw->inner);
#else
    (void)spinrwlock_read_unlock(&rw->inner);
#endif
    CORE_RWLOCK_TRACE_F("[%s:%p] core_rwlock_read_unlock RETURN\n", core_rwlock_name(rw), (void *)rw);
    return;
}

inline void
core_rwlock_write_lock(CORE_RWLOCK_VOLATILE core_rwlock_t *rw)
{
    CORE_RWLOCK_TRACE_F("[%s:%p] core_rwlock_write_lock ENTER\n", core_rwlock_name(rw), (void *)rw);
#ifdef CORE_RWLOCK_SLOW
    (void)enif_rwlock_rwlock(rw->inner);
#else
    (void)spinrwlock_write_lock(&rw->inner);
#endif
    CORE_RWLOCK_TRACE_F("[%s:%p] core_rwlock_write_lock RETURN\n", core_rwlock_name(rw), (void *)rw);
    return;
}

inline void
core_rwlock_write_unlock(CORE_RWLOCK_VOLATILE core_rwlock_t *rw)
{
    CORE_RWLOCK_TRACE_F("[%s:%p] core_rwlock_write_unlock ENTER\n", core_rwlock_name(rw), (void *)rw);
#ifdef CORE_RWLOCK_SLOW
    (void)enif_rwlock_rwunlock(rw->inner);
#else
    (void)spinrwlock_write_unlock(&rw->inner);
#endif
    CORE_RWLOCK_TRACE_F("[%s:%p] core_rwlock_write_unlock RETURN\n", core_rwlock_name(rw), (void *)rw);
    return;
}

#undef CORE_RWLOCK_VOLATILE
#undef CORE_RWLOCK_TRACE_F

#ifdef __cplusplus
}
#endif

#endif
