/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef EDF_CHANNEL_H
#define EDF_CHANNEL_H

#ifdef __cplusplus
extern "C" {
#endif

#include "../edf_common.h"
#include "../core/align.h"
#include "../core/linklist.h"
#include "../core/mutex.h"
#include "../core/rwlock.h"
#include "../edf_mpid.h"
#include "../ioq.h"
#include "../vec.h"
#include "edf_atom_cache.h"
#include "edf_external.h"
#include "edf_external_sequence.h"

/* Macro Definitions */

#define CHANNEL_RX_STATS_COUNT(channel, field, inc)                                                                                \
    do {                                                                                                                           \
        (channel)->rx.stats.field += inc;                                                                                          \
        (edf_channel_index_get())->rx_stats.field += inc;                                                                          \
    } while (0)

/* Type Definitions */

typedef struct edf_channel_s edf_channel_t;
typedef struct edf_channel_stats_s edf_channel_stats_t;
typedef struct edf_channel_stats_dop_s edf_channel_stats_dop_t;
typedef struct edf_channel_index_table_s edf_channel_index_table_t;
typedef struct edf_channel_index_slot_s edf_channel_index_slot_t;
typedef enum edf_channel_rx_state_t edf_channel_rx_state_t;
typedef struct edf_channel_resource_s edf_channel_resource_t;
typedef struct edf_channel_resource_table_s edf_channel_resource_table_t;

struct edf_channel_stats_dop_s {
    uint64_t seen;
    uint64_t emit;
    uint64_t drop;
};

struct edf_channel_stats_s {
    uint64_t packet_count;
    uint64_t emit_count;
    uint64_t drop_count;
    uint64_t dist_header_count;
    uint64_t dist_frag_header_count;
    uint64_t dist_frag_cont_count;
    uint64_t dist_pass_through_count;
    uint64_t atom_cache_read_count;
    uint64_t atom_cache_write_count;
    uint64_t atom_cache_overwrite_count;
    uint64_t rewrite_fragment_header_count;
    uint64_t rollback_atom_cache_count;
    uint64_t compact_external_count;
    uint64_t compact_fragment_count;
    uint64_t control_has_export_ext;
    uint64_t control_has_new_fun_ext;
    uint64_t payload_has_export_ext;
    uint64_t payload_has_new_fun_ext;
    edf_channel_stats_dop_t dop_link;
    edf_channel_stats_dop_t dop_send;
    edf_channel_stats_dop_t dop_exit;
    edf_channel_stats_dop_t dop_unlink;
    edf_channel_stats_dop_t dop_reg_send;
    edf_channel_stats_dop_t dop_group_leader;
    edf_channel_stats_dop_t dop_exit2;
    edf_channel_stats_dop_t dop_send_tt;
    edf_channel_stats_dop_t dop_exit_tt;
    edf_channel_stats_dop_t dop_reg_send_tt;
    edf_channel_stats_dop_t dop_exit2_tt;
    edf_channel_stats_dop_t dop_monitor_p;
    edf_channel_stats_dop_t dop_demonitor_p;
    edf_channel_stats_dop_t dop_monitor_p_exit;
    edf_channel_stats_dop_t dop_send_sender;
    edf_channel_stats_dop_t dop_send_sender_tt;
    edf_channel_stats_dop_t dop_payload_exit;
    edf_channel_stats_dop_t dop_payload_exit_tt;
    edf_channel_stats_dop_t dop_payload_exit2;
    edf_channel_stats_dop_t dop_payload_exit2_tt;
    edf_channel_stats_dop_t dop_payload_monitor_p_exit;
    edf_channel_stats_dop_t dop_spawn_request;
    edf_channel_stats_dop_t dop_spawn_request_tt;
    edf_channel_stats_dop_t dop_spawn_reply;
    edf_channel_stats_dop_t dop_spawn_reply_tt;
    edf_channel_stats_dop_t dop_alias_send;
    edf_channel_stats_dop_t dop_alias_send_tt;
    edf_channel_stats_dop_t dop_unlink_id;
    edf_channel_stats_dop_t dop_unlink_id_ack;
};

struct edf_channel_index_slot_s {
    linklist_t _link;
    edf_channel_stats_t rx_stats;
};

struct edf_channel_index_table_s {
    linklist_t _link;
    ErlNifTSDKey key;
    core_rwlock_t rwlock;
};

enum edf_channel_rx_state_t {
    EDF_CHANNEL_RX_STATE_PACKET_HEADER = 0,
    EDF_CHANNEL_RX_STATE_PACKET_DATA,
    EDF_CHANNEL_RX_STATE_EMIT_ATOM_CACHE_COMMIT,
    EDF_CHANNEL_RX_STATE_REWRITE_FRAGMENT_HEADER,
    EDF_CHANNEL_RX_STATE_EXTERNAL_RECV,
    EDF_CHANNEL_RX_STATE_EXTERNAL_EMIT,
    EDF_CHANNEL_RX_STATE_EXTERNAL_DROP,
    EDF_CHANNEL_RX_STATE_ROLLBACK_ATOM_CACHE,
    EDF_CHANNEL_RX_STATE_DIST_FRAME,
    EDF_CHANNEL_RX_STATE_DIST_HEADER,
    EDF_CHANNEL_RX_STATE_FRAGMENT_CONTINUATION,
    EDF_CHANNEL_RX_STATE_DECODE_CONTROL_LENGTH,
};

struct edf_channel_s {
    edf_channel_resource_t *resource;
    edf_mpid_t owner;
    edf_mpid_t trace;
    ERL_NIF_TERM sysname;
    uint32_t creation;
    uint32_t connection_id;
    uint64_t dflags;
    struct {
        size_t packet_size;
        edf_channel_rx_state_t state;
        ioq_t ioq;
        vec_t vec;
        edf_atom_cache_t *cache;
        edf_external_t *sequences;
        edf_channel_stats_t stats;
    } rx;
};

struct edf_channel_resource_s {
    linklist_t _link;
    core_rwlock_t rwlock;
    edf_channel_t *inner;
};

struct edf_channel_resource_table_s {
    linklist_t _link;
    core_mutex_t mutex;
};

/* Global Declarations */

extern ErlNifResourceType *edf_channel_resource_type;
extern edf_channel_resource_table_t *edf_channel_resource_table;
extern edf_channel_index_table_t *edf_channel_index_table;

/* Function Declarations */

extern int edf_channel_load(ErlNifEnv *env);
extern void edf_channel_unload(ErlNifEnv *env);
extern ERL_NIF_TERM edf_channel_resource_open(ErlNifEnv *env, size_t packet_size, ERL_NIF_TERM sysname, uint32_t creation,
                                              uint32_t connection_id, uint64_t dflags, edf_channel_resource_t **resourcep,
                                              edf_channel_t **channelp);
extern void edf_channel_destroy(ErlNifEnv *env, edf_channel_resource_t *resource, edf_channel_t *channel);
static edf_channel_index_slot_t *edf_channel_index_get(void);
extern edf_channel_index_slot_t *edf_channel_index_get_slow(void);
static int edf_channel_is_tracing_enabled(const edf_channel_t *channel);
static int edf_channel_tracing_send(edf_channel_t *channel, ErlNifEnv *caller_env, ErlNifEnv *msg_env, ERL_NIF_TERM msg);

#ifdef ERLDIST_FILTER_NIF_INTERNAL_API
#define EDF_CHANNEL_RESOURCE_FLAG_OWNER_REQUIRED (1 << 0)
#define EDF_CHANNEL_RESOURCE_FLAG_WRITE_LOCK (1 << 1)
static int edf_channel_resource_acquire(ErlNifEnv *env, ERL_NIF_TERM resource_term, edf_channel_resource_t **resourcep,
                                        edf_channel_t **channelp, ERL_NIF_TERM *error_term, int flags);
static int edf_channel_resource_acquire_direct(ErlNifEnv *env, edf_channel_resource_t *resource, edf_channel_t **channelp,
                                               ERL_NIF_TERM *error_term, int flags);
static void edf_channel_resource_release(edf_channel_resource_t **resourcep, edf_channel_t **channelp, int flags);
#endif

/* Inline Function Definitions */

inline edf_channel_index_slot_t *
edf_channel_index_get(void)
{
    edf_channel_index_slot_t *slot = (void *)enif_tsd_get(edf_channel_index_table->key);
    if (slot == NULL) {
        return edf_channel_index_get_slow();
    }
    return slot;
}

inline int
edf_channel_is_tracing_enabled(const edf_channel_t *channel)
{
    return (!enif_is_pid_undefined(&channel->trace.pid));
}

inline int
edf_channel_tracing_send(edf_channel_t *channel, ErlNifEnv *caller_env, ErlNifEnv *msg_env, ERL_NIF_TERM msg)
{
    return enif_send(caller_env, &channel->trace.pid, msg_env, msg);
}

#ifdef ERLDIST_FILTER_NIF_INTERNAL_API
int
edf_channel_resource_acquire(ErlNifEnv *env, ERL_NIF_TERM resource_term, edf_channel_resource_t **resourcep,
                             edf_channel_t **channelp, ERL_NIF_TERM *error_term, int flags)
{
    edf_channel_resource_t *resource = NULL;
    edf_channel_t *channel = NULL;

    if (!enif_get_resource(env, resource_term, edf_channel_resource_type, (void **)&resource)) {
        *error_term = EXCP_BADARG(env, "Channel Resource reference is invalid");
        return 0;
    }

    if (!edf_channel_resource_acquire_direct(env, resource, &channel, error_term, flags)) {
        return 0;
    }

    *resourcep = resource;
    *channelp = channel;

    return 1;
}

int
edf_channel_resource_acquire_direct(ErlNifEnv *env, edf_channel_resource_t *resource, edf_channel_t **channelp,
                                    ERL_NIF_TERM *error_term, int flags)
{
    edf_channel_t *channel = NULL;
    ErlNifPid caller_pid;

    if ((flags & EDF_CHANNEL_RESOURCE_FLAG_OWNER_REQUIRED) != 0 && enif_self(env, &caller_pid) == NULL) {
        if (error_term != NULL) {
            *error_term = EXCP_ERROR(env, "Call to enif_self() failed: not a process bound environment");
        }
        return 0;
    }

    if ((flags & EDF_CHANNEL_RESOURCE_FLAG_WRITE_LOCK) != 0) {
        (void)core_rwlock_write_lock(&resource->rwlock);
    } else {
        (void)core_rwlock_read_lock(&resource->rwlock);
    }

    channel = resource->inner;

    if (channel == NULL) {
        (void)edf_channel_resource_release(&resource, &channel, flags);
        if (error_term != NULL) {
            *error_term = enif_make_tuple2(env, ATOM(error), ATOM(closed));
        }
        return 0;
    }

    if ((flags & EDF_CHANNEL_RESOURCE_FLAG_OWNER_REQUIRED) != 0 && enif_compare_pids(&channel->owner.pid, &caller_pid) != 0) {
        (void)edf_channel_resource_release(&resource, &channel, flags);
        if (error_term != NULL) {
            *error_term = enif_make_tuple2(env, ATOM(error), ATOM(not_owner));
        }
        return 0;
    }

    *channelp = channel;

    return 1;
}

inline void
edf_channel_resource_release(edf_channel_resource_t **resourcep, edf_channel_t **channelp, int flags)
{
    edf_channel_resource_t *resource = (*resourcep);
    *resourcep = NULL;
    *channelp = NULL;
    if ((flags & EDF_CHANNEL_RESOURCE_FLAG_WRITE_LOCK) != 0) {
        (void)core_rwlock_write_unlock(&resource->rwlock);
    } else {
        (void)core_rwlock_read_unlock(&resource->rwlock);
    }
}
#endif

#ifdef __cplusplus
}
#endif

#endif
