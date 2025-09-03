/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef EDF_LOGGER_H
#define EDF_LOGGER_H

#ifdef __cplusplus
extern "C" {
#endif

#include "../erldist_filter_nif.h"
#include "../../primitive/linklist.h"
#include "../core/ipc.h"
#include "../core/xnif_monitor.h"
#include "../core/xnif_mutex.h"
#include "../core/xnif_rwlock.h"
#include "../vec.h"

/* Macro Definitions */

#define EDF_LOGGER_DEFAULT_CAPACITY (2)

/* Type Definitions */

typedef struct edf_logger_event_s edf_logger_event_t;
typedef struct edf_logger_queue_s edf_logger_queue_t;
typedef struct edf_logger_s edf_logger_t;
typedef struct edf_logger_resource_s edf_logger_resource_t;
typedef struct edf_logger_resource_table_s edf_logger_resource_table_t;

struct edf_logger_event_s {
    ipc_message_t super;
    ErlNifTime time;
    ERL_NIF_TERM sysname;
    size_t number_of_atoms;
    ERL_NIF_TERM *atoms;
    struct {
        vec_t vec;
        vec_writer_t writer;
    } control;
    struct {
        vec_t vec;
        vec_writer_t writer;
    } payload;
};

struct edf_logger_queue_s {
    ipc_queue_t super;
};

struct edf_logger_s {
    edf_logger_resource_t *resource;
    xnif_monitor_t owner;
    struct {
        bool active;
        int fd;
        ERL_NIF_TERM handle;
    } select;
    struct {
        size_t received;
        size_t dropped;
    } stats;
};

struct edf_logger_resource_s {
    linklist_t _link;
    xnif_rwlock_t rwlock;
    edf_logger_t *inner;
    int closefd;
};

struct edf_logger_resource_table_s {
    linklist_t _link;
    xnif_mutex_t mutex;
};

/* Global Declarations */

extern ErlNifResourceType *edf_logger_resource_type;
extern edf_logger_resource_table_t *edf_logger_resource_table;
extern edf_logger_queue_t *edf_logger_queue_global;

/* Function Declarations */

extern int edf_logger_load(ErlNifEnv *env);
extern void edf_logger_unload(ErlNifEnv *env);
extern ERL_NIF_TERM edf_logger_resource_open(ErlNifEnv *env, edf_logger_resource_t **resourcep, edf_logger_t **loggerp);
extern void edf_logger_destroy(ErlNifEnv *env, edf_logger_resource_t *resource, edf_logger_t *logger);
extern int edf_logger_event_create(ERL_NIF_TERM sysname, size_t number_of_atoms, bool is_external, size_t control_size,
                                   size_t payload_size, edf_logger_event_t **eventp);
extern void edf_logger_event_destroy(edf_logger_event_t *event);
extern int edf_logger_event_into_term(ErlNifEnv *env, edf_logger_event_t *event, ERL_NIF_TERM *termp);

#ifdef ERLDIST_FILTER_NIF_INTERNAL_API
#define EDF_LOGGER_RESOURCE_FLAG_OWNER_REQUIRED (1 << 0)
#define EDF_LOGGER_RESOURCE_FLAG_WRITE_LOCK (1 << 1)
static int edf_logger_resource_acquire(ErlNifEnv *env, ERL_NIF_TERM resource_term, edf_logger_resource_t **resourcep,
                                       edf_logger_t **loggerp, ERL_NIF_TERM *error_term, int flags);
static int edf_logger_resource_acquire_direct(ErlNifEnv *env, edf_logger_resource_t *resource, edf_logger_t **loggerp,
                                              ERL_NIF_TERM *error_term, int flags);
static void edf_logger_resource_release(edf_logger_resource_t **resourcep, edf_logger_t **loggerp, int flags);
#endif

/* Inline Function Definitions */

#ifdef ERLDIST_FILTER_NIF_INTERNAL_API
int
edf_logger_resource_acquire(ErlNifEnv *env, ERL_NIF_TERM resource_term, edf_logger_resource_t **resourcep, edf_logger_t **loggerp,
                            ERL_NIF_TERM *error_term, int flags)
{
    edf_logger_resource_t *resource = NULL;
    edf_logger_t *logger = NULL;

    if (!enif_get_resource(env, resource_term, edf_logger_resource_type, (void **)&resource)) {
        *error_term = EXCP_BADARG(env, "Logger Resource reference is invalid");
        return 0;
    }

    if (!edf_logger_resource_acquire_direct(env, resource, &logger, error_term, flags)) {
        return 0;
    }

    *resourcep = resource;
    *loggerp = logger;

    return 1;
}

int
edf_logger_resource_acquire_direct(ErlNifEnv *env, edf_logger_resource_t *resource, edf_logger_t **loggerp,
                                   ERL_NIF_TERM *error_term, int flags)
{
    edf_logger_t *logger = NULL;
    ErlNifPid caller_pid;

    if ((flags & EDF_LOGGER_RESOURCE_FLAG_OWNER_REQUIRED) != 0 && enif_self(env, &caller_pid) == NULL) {
        if (error_term != NULL) {
            *error_term = EXCP_ERROR(env, "Call to enif_self() failed: not a process bound environment");
        }
        return 0;
    }

    if ((flags & EDF_LOGGER_RESOURCE_FLAG_WRITE_LOCK) != 0) {
        (void)xnif_rwlock_write_lock(&resource->rwlock);
    } else {
        (void)xnif_rwlock_read_lock(&resource->rwlock);
    }

    logger = resource->inner;

    if (logger == NULL) {
        (void)edf_logger_resource_release(&resource, &logger, flags);
        if (error_term != NULL) {
            *error_term = enif_make_tuple2(env, ATOM(error), ATOM(closed));
        }
        return 0;
    }

    if ((flags & EDF_LOGGER_RESOURCE_FLAG_OWNER_REQUIRED) != 0 && enif_compare_pids(&logger->owner.pid, &caller_pid) != 0) {
        (void)edf_logger_resource_release(&resource, &logger, flags);
        if (error_term != NULL) {
            *error_term = enif_make_tuple2(env, ATOM(error), ATOM(not_owner));
        }
        return 0;
    }

    *loggerp = logger;

    return 1;
}

inline void
edf_logger_resource_release(edf_logger_resource_t **resourcep, edf_logger_t **loggerp, int flags)
{
    edf_logger_resource_t *resource = (*resourcep);
    *resourcep = NULL;
    *loggerp = NULL;
    if ((flags & EDF_LOGGER_RESOURCE_FLAG_WRITE_LOCK) != 0) {
        (void)xnif_rwlock_write_unlock(&resource->rwlock);
    } else {
        (void)xnif_rwlock_read_unlock(&resource->rwlock);
    }
}
#endif

#ifdef __cplusplus
}
#endif

#endif
