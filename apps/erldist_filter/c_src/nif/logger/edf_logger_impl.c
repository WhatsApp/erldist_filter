/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#define ERLDIST_FILTER_NIF_INTERNAL_API 1
#include "edf_logger_impl.h"
#include "edf_logger_queue_recv.h"

ERL_NIF_TERM
erldist_filter_nif_logger_open_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM out_term;

    if (argc != 0) {
        return EXCP_BADARG(env, "argc must be 0");
    }

    out_term = edf_logger_resource_open(env, NULL, NULL);

    return out_term;
}

ERL_NIF_TERM
erldist_filter_nif_logger_close_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM out_term;
    edf_logger_resource_t *resource = NULL;
    edf_logger_t *logger = NULL;
    edf_logger_resource_t *closed_resource = NULL;
    int flags = (EDF_LOGGER_RESOURCE_FLAG_OWNER_REQUIRED | EDF_LOGGER_RESOURCE_FLAG_WRITE_LOCK);

    if (argc != 1) {
        return EXCP_BADARG(env, "argc must be 1");
    }

    if (!edf_logger_resource_acquire(env, argv[0], &resource, &logger, &out_term, flags)) {
        return out_term;
    }

    resource->inner = NULL;

    closed_resource = resource;
    (void)edf_logger_destroy(env, resource, logger);
    (void)edf_logger_resource_release(&resource, &logger, flags);

    (void)enif_release_resource((void *)closed_resource);

    out_term = ATOM(ok);

    return out_term;
}

ERL_NIF_TERM
erldist_filter_nif_logger_inspect_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
#define RET_MAP_SIZE (3)

    ERL_NIF_TERM out_term;
    edf_logger_resource_t *resource = NULL;
    edf_logger_t *logger = NULL;
    int flags = 0;
    ERL_NIF_TERM keys[RET_MAP_SIZE];
    ERL_NIF_TERM vals[RET_MAP_SIZE];
    size_t k = 0;
    size_t v = 0;

    if (argc != 1) {
        return EXCP_BADARG(env, "argc must be 1");
    }

    if (!edf_logger_resource_acquire(env, argv[0], &resource, &logger, &out_term, flags)) {
        return out_term;
    }

    keys[k++] = enif_make_atom(env, "controlling_process");
    vals[v++] = enif_make_pid(env, &logger->owner.pid);

    keys[k++] = enif_make_atom(env, "dropped");
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)logger->stats.dropped);

    keys[k++] = enif_make_atom(env, "received");
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)logger->stats.received);

    if (!enif_make_map_from_arrays(env, keys, vals, RET_MAP_SIZE, &out_term)) {
        (void)edf_logger_resource_release(&resource, &logger, flags);
        return EXCP_BADARG(env, "Call to enif_make_map_from_arrays() failed: duplicate keys detected");
    }

    (void)edf_logger_resource_release(&resource, &logger, flags);
    return out_term;

#undef RET_MAP_SIZE
}

ERL_NIF_TERM
erldist_filter_nif_logger_list_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    edf_logger_resource_t *root = NULL;
    edf_logger_resource_t *node = NULL;
    edf_logger_resource_t *temp = NULL;
    ERL_NIF_TERM list;

    if (argc != 0) {
        return EXCP_BADARG(env, "argc must be 0");
    }

    list = enif_make_list(env, 0);
    root = (void *)edf_logger_resource_table;
    (void)core_mutex_lock(&edf_logger_resource_table->mutex);
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
    (void)core_mutex_unlock(&edf_logger_resource_table->mutex);

    return list;
}

ERL_NIF_TERM
erldist_filter_nif_logger_recv_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM out_term;
    ERL_NIF_TERM trap_term;
    edf_logger_resource_t *resource = NULL;
    edf_logger_t *logger = NULL;
    int flags = (EDF_LOGGER_RESOURCE_FLAG_OWNER_REQUIRED | EDF_LOGGER_RESOURCE_FLAG_WRITE_LOCK);
    ipc_batch_t batch_buf;
    ipc_batch_t *batch = &batch_buf;

    if (argc != 1) {
        return EXCP_BADARG(env, "argc must be 1");
    }

    if (!edf_logger_resource_acquire(env, argv[0], &resource, &logger, &out_term, flags)) {
        return out_term;
    }

    (void)ipc_batch_init(batch);
    if (!edf_logger_queue_recv(env, resource, logger, batch, &out_term)) {
        (void)edf_logger_resource_release(&resource, &logger, flags);
        return out_term;
    }

    trap_term = edf_logger_queue_recv_trap_open(env, resource, logger, batch, NULL);
    if (enif_is_exception(env, trap_term)) {
        (void)ipc_batch_dtor(batch);
        (void)edf_logger_resource_release(&resource, &logger, flags);
        return trap_term;
    }
    (void)edf_logger_resource_release(&resource, &logger, flags);
    return edf_trap_schedule_from_term(env, trap_term);
}

ERL_NIF_TERM
erldist_filter_nif_logger_set_capacity_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    size_t new_capacity;
    size_t old_capacity;

    if (argc != 1) {
        return EXCP_BADARG(env, "argc must be 1");
    }

    if (!enif_get_uint64(env, argv[0], (ErlNifUInt64 *)&new_capacity)) {
        return EXCP_BADARG(env, "NewCapacity is invalid");
    }

    if (!ipc_queue_set_capacity(&edf_logger_queue_global->super, new_capacity, &old_capacity)) {
        return EXCP_BADARG(env, "Call to ipc_queue_set_capacity() failed");
    }

    return enif_make_uint64(env, (ErlNifUInt64)old_capacity);
}

ERL_NIF_TERM
erldist_filter_nif_logger_set_controlling_process_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM out_term;
    edf_logger_resource_t *resource = NULL;
    edf_logger_t *logger = NULL;
    int flags = (EDF_LOGGER_RESOURCE_FLAG_OWNER_REQUIRED | EDF_LOGGER_RESOURCE_FLAG_WRITE_LOCK);
    ErlNifPid new_owner_pid;
    ErlNifPid old_owner_pid;
    int retval;

    if (argc != 2) {
        return EXCP_BADARG(env, "argc must be 2");
    }

    if (!edf_logger_resource_acquire(env, argv[0], &resource, &logger, &out_term, flags)) {
        return out_term;
    }

    if (!enif_get_local_pid(env, argv[1], &new_owner_pid)) {
        (void)edf_logger_resource_release(&resource, &logger, flags);
        return EXCP_BADARG(env, "NewOwnerPid must be a local process");
    }

    old_owner_pid = logger->owner.pid;

    if (enif_compare_pids(&old_owner_pid, &new_owner_pid) == 0) {
        // OldOwnerPid matches NewOwnerPid, do nothing.
        (void)edf_logger_resource_release(&resource, &logger, flags);
        return ATOM(ok);
    }

    if (!enif_is_process_alive(env, &new_owner_pid)) {
        (void)edf_logger_resource_release(&resource, &logger, flags);
        return EXCP_BADARG(env, "NewOwnerPid is no longer alive");
    }

    if (edf_mpid_demonitor_process(env, (void *)resource, &logger->owner) != 0) {
        // OwnerPid is about to exit, consider the logger closed.
        (void)edf_logger_resource_release(&resource, &logger, flags);
        return enif_make_tuple2(env, ATOM(error), ATOM(closed));
    }

    retval = edf_mpid_monitor_process(env, (void *)resource, &new_owner_pid, &logger->owner);
    if (retval < 0) {
        (void)edf_logger_resource_release(&resource, &logger, flags);
        return EXCP_ERROR(env, "Call to enif_monitor_process() failed: no `down' callback provided");
    } else if (retval > 0) {
        (void)edf_logger_resource_release(&resource, &logger, flags);
        return EXCP_ERROR(env, "Call to enif_monitor_process() failed: target process is no longer alive");
    }

    (void)edf_logger_resource_release(&resource, &logger, flags);
    return ATOM(ok);
}
