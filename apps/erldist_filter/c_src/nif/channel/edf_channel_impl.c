/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#define ERLDIST_FILTER_NIF_INTERNAL_API 1
#include "edf_channel_impl.h"
#include "edf_channel_inspect.h"
#include "edf_channel_recv.h"

#include "../core/simd.h"
#include "../erts/dist.h"

ERL_NIF_TERM
erldist_filter_nif_channel_open_5(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM out_term;
    unsigned int packet_size;
    ERL_NIF_TERM sysname;
    unsigned long creation;
    unsigned long connection_id;
    ErlNifUInt64 distribution_flags;
    uint64_t dflags = 0;

    if (argc != 5) {
        return EXCP_BADARG(env, "argc must be 5");
    }

    if (!enif_get_uint(env, argv[0], &packet_size) ||
        !(packet_size == 0 || packet_size == 1 || packet_size == 2 || packet_size == 4 || packet_size == 8)) {
        return EXCP_BADARG(env, "PacketSize must be an unsigned integer and one of {0, 1, 2, 4, 8}");
    }

    sysname = argv[1];
    if (!enif_is_atom(env, sysname)) {
        return EXCP_BADARG(env, "Sysname must be an atom");
    }

    if (!enif_get_ulong(env, argv[2], &creation)) {
        return EXCP_BADARG(env, "Creation must be a 32-bit unsigned integer");
    }

    if (!enif_get_ulong(env, argv[3], &connection_id)) {
        return EXCP_BADARG(env, "ConnectionId must be a 32-bit unsigned integer");
    }

    if (!enif_get_uint64(env, argv[4], &distribution_flags)) {
        return EXCP_BADARG(env, "DistributionFlags must be a 64-bit unsigned integer");
    }

    dflags = (uint64_t)distribution_flags;
    if ((dflags & DFLAG_DIST_MANDATORY) == 0) {
        return EXCP_BADARG(env, "DistributionFlags must have DFLAG_DIST_MANDATORY flags set");
    }

    out_term = edf_channel_resource_open(env, (size_t)packet_size, sysname, (uint32_t)creation, (uint32_t)connection_id, dflags,
                                         NULL, NULL);

    return out_term;
}

ERL_NIF_TERM
erldist_filter_nif_channel_close_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM out_term;
    edf_channel_resource_t *resource = NULL;
    edf_channel_t *channel = NULL;
    edf_channel_resource_t *closed_resource = NULL;
    int flags = (EDF_CHANNEL_RESOURCE_FLAG_OWNER_REQUIRED | EDF_CHANNEL_RESOURCE_FLAG_WRITE_LOCK);

    if (argc != 1) {
        return EXCP_BADARG(env, "argc must be 1");
    }

    if (!edf_channel_resource_acquire(env, argv[0], &resource, &channel, &out_term, flags)) {
        return out_term;
    }

    resource->inner = NULL;

    closed_resource = resource;
    (void)edf_channel_destroy(env, resource, channel);
    (void)edf_channel_resource_release(&resource, &channel, flags);

    (void)enif_release_resource((void *)closed_resource);

    out_term = ATOM(ok);

    return out_term;
}

ERL_NIF_TERM
erldist_filter_nif_channel_index_get_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
#define RET_MAP_SIZE (2)

    ERL_NIF_TERM out_term;
    ERL_NIF_TERM keys[RET_MAP_SIZE];
    ERL_NIF_TERM vals[RET_MAP_SIZE];
    size_t k = 0;
    size_t v = 0;
    ErlNifUInt64 slots;
    edf_channel_stats_t acc[1];
    edf_channel_index_slot_t *root = NULL;
    edf_channel_index_slot_t *slot = NULL;

    if (argc != 0) {
        return EXCP_BADARG(env, "argc must be 0");
    }

    (void)memset((void *)acc, 0, sizeof(edf_channel_stats_t));

    slots = 0;
    (void)core_rwlock_read_lock(&edf_channel_index_table->rwlock);
    root = (void *)&edf_channel_index_table->_link;
    slot = (void *)root->_link.next;
    while (slot != root) {
        (void)core_simd_add_vec_u64((uint64_t *)acc, (const uint64_t *)&slot->rx_stats,
                                    (sizeof(edf_channel_stats_t) / sizeof(uint64_t)));
        slots += 1;
        slot = (void *)slot->_link.next;
    }
    (void)core_rwlock_read_unlock(&edf_channel_index_table->rwlock);
    keys[k++] = ATOM(rx_stats);
    if (!edf_channel_inspect_stats(env, acc, &out_term)) {
        return out_term;
    }
    vals[v++] = out_term;

    keys[k++] = ATOM(slots);
    vals[v++] = enif_make_uint64(env, slots);

    if (!enif_make_map_from_arrays(env, keys, vals, RET_MAP_SIZE, &out_term)) {
        return EXCP_BADARG(env, "Call to enif_make_map_from_arrays() failed: duplicate keys detected");
    }

    return out_term;

#undef RET_MAP_SIZE
}

ERL_NIF_TERM
erldist_filter_nif_channel_inspect_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
#define RET_MAP_SIZE (4)

    ERL_NIF_TERM out_term;
    edf_channel_resource_t *resource = NULL;
    edf_channel_t *channel = NULL;
    int flags = 0;
    ERL_NIF_TERM keys[RET_MAP_SIZE];
    ERL_NIF_TERM vals[RET_MAP_SIZE];
    size_t k = 0;
    size_t v = 0;

    if (argc != 1) {
        return EXCP_BADARG(env, "argc must be 1");
    }

    if (!edf_channel_resource_acquire(env, argv[0], &resource, &channel, &out_term, flags)) {
        return out_term;
    }

    keys[k++] = ATOM(controlling_process);
    vals[v++] = enif_make_pid(env, &channel->owner.pid);

    keys[k++] = ATOM(entry);
    if (!edf_channel_inspect_entry(env, channel, &out_term)) {
        (void)edf_channel_resource_release(&resource, &channel, flags);
        return out_term;
    }
    vals[v++] = out_term;

    keys[k++] = ATOM(rx);
    if (!edf_channel_inspect_rx(env, channel, &out_term)) {
        (void)edf_channel_resource_release(&resource, &channel, flags);
        return out_term;
    }
    vals[v++] = out_term;

    keys[k++] = ATOM(tracing_process);
    vals[v++] = enif_make_pid(env, &channel->trace.pid);

    if (!enif_make_map_from_arrays(env, keys, vals, RET_MAP_SIZE, &out_term)) {
        (void)edf_channel_resource_release(&resource, &channel, flags);
        return EXCP_BADARG(env, "Call to enif_make_map_from_arrays() failed: duplicate keys detected");
    }

    (void)edf_channel_resource_release(&resource, &channel, flags);
    return out_term;

#undef RET_MAP_SIZE
}

ERL_NIF_TERM
erldist_filter_nif_channel_list_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    edf_channel_resource_t *root = NULL;
    edf_channel_resource_t *node = NULL;
    edf_channel_resource_t *temp = NULL;
    ERL_NIF_TERM list;

    if (argc != 0) {
        return EXCP_BADARG(env, "argc must be 0");
    }

    list = enif_make_list(env, 0);
    root = (void *)edf_channel_resource_table;
    (void)core_mutex_lock(&edf_channel_resource_table->mutex);
    node = (void *)(root->_link.prev);
    while (root != node) {
        temp = (void *)(node->_link.prev);
        (void)core_rwlock_read_lock(&node->rwlock);
        if (node->inner != NULL) {
            list = enif_make_list_cell(env, enif_make_resource(env, (void *)node), list);
        }
        (void)core_rwlock_read_unlock(&node->rwlock);
        node = temp;
    }
    (void)core_mutex_unlock(&edf_channel_resource_table->mutex);

    return list;
}

ERL_NIF_TERM
erldist_filter_nif_channel_list_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM sysname;
    edf_channel_resource_t *root = NULL;
    edf_channel_resource_t *node = NULL;
    edf_channel_resource_t *temp = NULL;
    ERL_NIF_TERM list;

    if (argc != 1) {
        return EXCP_BADARG(env, "argc must be 1");
    }

    if (!enif_is_atom(env, argv[0])) {
        return EXCP_BADARG(env, "Sysname must be an atom");
    }

    sysname = argv[0];

    list = enif_make_list(env, 0);
    root = (void *)edf_channel_resource_table;
    (void)core_mutex_lock(&edf_channel_resource_table->mutex);
    node = (void *)(root->_link.prev);
    while (root != node) {
        temp = (void *)(node->_link.prev);
        (void)core_rwlock_read_lock(&node->rwlock);
        if (node->inner != NULL && node->inner->sysname == sysname) {
            list = enif_make_list_cell(env, enif_make_resource(env, (void *)node), list);
        }
        (void)core_rwlock_read_unlock(&node->rwlock);
        node = temp;
    }
    (void)core_mutex_unlock(&edf_channel_resource_table->mutex);

    return list;
}

ERL_NIF_TERM
erldist_filter_nif_channel_recv_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM out_term;
    ERL_NIF_TERM trap_term;
    edf_channel_resource_t *resource = NULL;
    edf_channel_t *channel = NULL;
    int flags = (EDF_CHANNEL_RESOURCE_FLAG_OWNER_REQUIRED | EDF_CHANNEL_RESOURCE_FLAG_WRITE_LOCK);

    if (argc != 2) {
        return EXCP_BADARG(env, "argc must be 2");
    }

    if (!edf_channel_resource_acquire(env, argv[0], &resource, &channel, &out_term, flags)) {
        return out_term;
    }

    if (!ioq_inspect_iovec_and_enqv(env, ~(0ULL), argv[1], &out_term, &channel->rx.ioq)) {
        (void)edf_channel_resource_release(&resource, &channel, flags);
        return EXCP_BADARG(env, "Iodata is invalid");
    }

    if (!enif_is_empty_list(env, out_term)) {
        (void)edf_channel_resource_release(&resource, &channel, flags);
        return EXCP_BADARG(env, "Call to enif_ioq_enqv() failed: expected tail to be an empty list");
    }

    trap_term = edf_channel_recv_trap_open(env, resource, channel, NULL);
    if (enif_is_exception(env, trap_term)) {
        (void)edf_channel_resource_release(&resource, &channel, flags);
        return trap_term;
    }
    (void)edf_channel_resource_release(&resource, &channel, flags);
    return edf_trap_schedule_from_term(env, trap_term);
}

ERL_NIF_TERM
erldist_filter_nif_channel_set_controlling_process_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM out_term;
    edf_channel_resource_t *resource = NULL;
    edf_channel_t *channel = NULL;
    int flags = (EDF_CHANNEL_RESOURCE_FLAG_OWNER_REQUIRED | EDF_CHANNEL_RESOURCE_FLAG_WRITE_LOCK);
    ErlNifPid new_owner_pid;
    ErlNifPid old_owner_pid;
    int retval;

    if (argc != 2) {
        return EXCP_BADARG(env, "argc must be 2");
    }

    if (!edf_channel_resource_acquire(env, argv[0], &resource, &channel, &out_term, flags)) {
        return out_term;
    }

    if (!enif_get_local_pid(env, argv[1], &new_owner_pid)) {
        (void)edf_channel_resource_release(&resource, &channel, flags);
        return EXCP_BADARG(env, "NewOwnerPid must be a local process");
    }

    old_owner_pid = channel->owner.pid;

    if (enif_compare_pids(&old_owner_pid, &new_owner_pid) == 0) {
        // OldOwnerPid matches NewOwnerPid, do nothing.
        (void)edf_channel_resource_release(&resource, &channel, flags);
        return ATOM(ok);
    }

    if (!enif_is_process_alive(env, &new_owner_pid)) {
        (void)edf_channel_resource_release(&resource, &channel, flags);
        return EXCP_BADARG(env, "NewOwnerPid is no longer alive");
    }

    if (edf_mpid_demonitor_process(env, (void *)resource, &channel->owner) != 0) {
        // OwnerPid is about to exit, consider the channel closed.
        (void)edf_channel_resource_release(&resource, &channel, flags);
        return enif_make_tuple2(env, ATOM(error), ATOM(closed));
    }

    retval = edf_mpid_monitor_process(env, (void *)resource, &new_owner_pid, &channel->owner);
    if (retval < 0) {
        (void)edf_channel_resource_release(&resource, &channel, flags);
        return EXCP_ERROR(env, "Call to enif_monitor_process() failed: no `down' callback provided");
    } else if (retval > 0) {
        (void)edf_channel_resource_release(&resource, &channel, flags);
        return EXCP_ERROR(env, "Call to enif_monitor_process() failed: target process is no longer alive");
    }

    (void)edf_channel_resource_release(&resource, &channel, flags);
    return ATOM(ok);
}

ERL_NIF_TERM
erldist_filter_nif_channel_set_tracing_process_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM out_term;
    edf_channel_resource_t *resource = NULL;
    edf_channel_t *channel = NULL;
    int flags = (EDF_CHANNEL_RESOURCE_FLAG_OWNER_REQUIRED | EDF_CHANNEL_RESOURCE_FLAG_WRITE_LOCK);
    ErlNifPid new_trace_pid;
    ErlNifPid old_trace_pid;
    int retval;

    if (argc != 2) {
        return EXCP_BADARG(env, "argc must be 2");
    }

    if (!edf_channel_resource_acquire(env, argv[0], &resource, &channel, &out_term, flags)) {
        return out_term;
    }

    if (!enif_get_local_pid(env, argv[1], &new_trace_pid)) {
        (void)edf_channel_resource_release(&resource, &channel, flags);
        return EXCP_BADARG(env, "NewTracePid must be a local process");
    }

    old_trace_pid = channel->trace.pid;

    if (enif_compare_pids(&old_trace_pid, &new_trace_pid) == 0) {
        // OldTracePid matches NewTracePid, do nothing.
        (void)edf_channel_resource_release(&resource, &channel, flags);
        return ATOM(ok);
    }

    if (!enif_is_process_alive(env, &new_trace_pid)) {
        (void)edf_channel_resource_release(&resource, &channel, flags);
        return EXCP_BADARG(env, "NewTracePid is no longer alive");
    }

    (void)edf_mpid_demonitor_process(env, (void *)resource, &channel->trace);

    retval = edf_mpid_monitor_process(env, (void *)resource, &new_trace_pid, &channel->owner);
    if (retval < 0) {
        (void)edf_channel_resource_release(&resource, &channel, flags);
        return EXCP_ERROR(env, "Call to enif_monitor_process() failed: no `down' callback provided");
    } else if (retval > 0) {
        (void)edf_channel_resource_release(&resource, &channel, flags);
        return EXCP_ERROR(env, "Call to enif_monitor_process() failed: target process is no longer alive");
    }

    (void)edf_channel_resource_release(&resource, &channel, flags);
    return ATOM(ok);
}
