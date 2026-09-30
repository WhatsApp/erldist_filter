/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "edf_channel_test_hook.h"

#ifdef EDF_TEST_HOOKS

#include <inttypes.h>
#include <stdatomic.h>
#include <stdio.h>

typedef enum edf_channel_test_hook_barrier_t {
    EDF_CHANNEL_TEST_HOOK_BARRIER_IDLE = 0,
    EDF_CHANNEL_TEST_HOOK_BARRIER_WAITING,
    EDF_CHANNEL_TEST_HOOK_BARRIER_OPEN,
} edf_channel_test_hook_barrier_t;

static ErlNifMutex *test_hook_mutex = NULL;
static atomic_bool test_hook_armed = false;
static ErlNifPid test_hook_notify_pid;
static const edf_channel_resource_t *test_hook_resource = NULL;
static uint64_t test_hook_barrier_fragment_id = 0;
static edf_channel_test_hook_barrier_t test_hook_barrier = EDF_CHANNEL_TEST_HOOK_BARRIER_IDLE;
static uint64_t test_hook_event_count = 0;

int
edf_channel_test_hook_load(void)
{
    if (test_hook_mutex == NULL) {
        test_hook_mutex = enif_mutex_create("erldist_filter.channel_test_hook_mutex");
        if (test_hook_mutex == NULL) {
            return -1;
        }
    }
    return 0;
}

void
edf_channel_test_hook_unload(void)
{
    // Called only when the last NIF module instance unloads.
    if (test_hook_mutex != NULL) {
        (void)atomic_store_explicit(&test_hook_armed, false, memory_order_release);
        test_hook_resource = NULL;
        (void)enif_mutex_destroy(test_hook_mutex);
        test_hook_mutex = NULL;
    }
    return;
}

static ERL_NIF_TERM
test_hook_pointer_term(ErlNifEnv *env, const void *pointer)
{
    return enif_make_uint64(env, (ErlNifUInt64)((uintptr_t)pointer));
}

// Must be called with test_hook_mutex held.
static void
test_hook_notify(ErlNifEnv *msg_env, uint64_t count, const char *name, ERL_NIF_TERM details)
{
    ERL_NIF_TERM msg;

    msg = enif_make_tuple4(msg_env, enif_make_atom(msg_env, "erldist_filter_test_hook"), enif_make_uint64(msg_env, count),
                           enif_make_atom(msg_env, name), details);
    // caller_env is NULL: events fire from NIF calls, monitor down callbacks, and resource destructors.
    (void)enif_send(NULL, &test_hook_notify_pid, msg_env, msg);
}

void
edf_channel_test_hook_event(const char *name, const edf_channel_resource_t *resource, const void *trap, const void *external)
{
    ErlNifEnv *msg_env = NULL;
    ERL_NIF_TERM keys[2];
    ERL_NIF_TERM values[2];
    ERL_NIF_TERM details;
    uint64_t count;

    if (!atomic_load_explicit(&test_hook_armed, memory_order_acquire)) {
        return;
    }
    (void)enif_mutex_lock(test_hook_mutex);
    if (!atomic_load_explicit(&test_hook_armed, memory_order_relaxed) || resource == NULL || resource != test_hook_resource) {
        (void)enif_mutex_unlock(test_hook_mutex);
        return;
    }
    count = ++test_hook_event_count;
    // Pointers are only printed; `external' may already be freed and must never be dereferenced here.
    (void)enif_fprintf(stderr, "[edf_test_hook] event=%" PRIu64 " name=%s resource=%p trap=%p external=%p\n", count, name,
                       (const void *)resource, trap, external);
    msg_env = enif_alloc_env();
    if (msg_env != NULL) {
        keys[0] = enif_make_atom(msg_env, "trap");
        values[0] = test_hook_pointer_term(msg_env, trap);
        keys[1] = enif_make_atom(msg_env, "external");
        values[1] = test_hook_pointer_term(msg_env, external);
        if (!enif_make_map_from_arrays(msg_env, keys, values, 2, &details)) {
            details = enif_make_new_map(msg_env);
        }
        (void)test_hook_notify(msg_env, count, name, details);
        (void)enif_free_env(msg_env);
    }
    (void)enif_mutex_unlock(test_hook_mutex);
    return;
}

int
edf_channel_test_hook_recv_barrier(const edf_channel_resource_t *resource, const void *trap, edf_external_t *external)
{
    ErlNifEnv *msg_env = NULL;
    ERL_NIF_TERM keys[6];
    ERL_NIF_TERM values[6];
    ERL_NIF_TERM details;
    uint64_t count;
    uint64_t fragment_id;
    bool linked;
    int should_yield = 0;

    if (external == NULL || !atomic_load_explicit(&test_hook_armed, memory_order_acquire)) {
        return 0;
    }

    (void)enif_mutex_lock(test_hook_mutex);
    if (!atomic_load_explicit(&test_hook_armed, memory_order_relaxed) || resource == NULL || resource != test_hook_resource ||
        test_hook_barrier_fragment_id == 0 || test_hook_barrier == EDF_CHANNEL_TEST_HOOK_BARRIER_OPEN) {
        (void)enif_mutex_unlock(test_hook_mutex);
        return 0;
    }
    // FragmentId of the most recently received fragment of the in-flight external.
    fragment_id = external->fragment_id_next + 1;
    if (fragment_id != test_hook_barrier_fragment_id) {
        (void)enif_mutex_unlock(test_hook_mutex);
        return 0;
    }
    should_yield = 1;
    if (test_hook_barrier == EDF_CHANNEL_TEST_HOOK_BARRIER_IDLE) {
        test_hook_barrier = EDF_CHANNEL_TEST_HOOK_BARRIER_WAITING;
        count = ++test_hook_event_count;
        linked = edf_external_sequence_is_linked(external);
        (void)enif_fprintf(stderr,
                           "[edf_test_hook] event=%" PRIu64
                           " name=recv_barrier resource=%p trap=%p external=%p sequence_id=%" PRIu64 " fragment_id=%" PRIu64
                           " fragment_count=%" PRIu64 " linked_in_rx_sequences=%s\n",
                           count, (const void *)resource, trap, (void *)external, external->sequence_id, fragment_id,
                           external->fragment_count, (linked) ? "true" : "false");
        msg_env = enif_alloc_env();
        if (msg_env != NULL) {
            keys[0] = enif_make_atom(msg_env, "trap");
            values[0] = test_hook_pointer_term(msg_env, trap);
            keys[1] = enif_make_atom(msg_env, "external");
            values[1] = test_hook_pointer_term(msg_env, external);
            keys[2] = enif_make_atom(msg_env, "sequence_id");
            values[2] = enif_make_uint64(msg_env, external->sequence_id);
            keys[3] = enif_make_atom(msg_env, "fragment_id");
            values[3] = enif_make_uint64(msg_env, fragment_id);
            keys[4] = enif_make_atom(msg_env, "fragment_count");
            values[4] = enif_make_uint64(msg_env, external->fragment_count);
            keys[5] = enif_make_atom(msg_env, "linked_in_rx_sequences");
            values[5] = enif_make_atom(msg_env, (linked) ? "true" : "false");
            if (!enif_make_map_from_arrays(msg_env, keys, values, 6, &details)) {
                details = enif_make_new_map(msg_env);
            }
            (void)test_hook_notify(msg_env, count, "recv_barrier", details);
            (void)enif_free_env(msg_env);
        }
    }
    (void)enif_mutex_unlock(test_hook_mutex);
    return should_yield;
}

ERL_NIF_TERM
erldist_filter_nif_test_hook_arm_3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifPid notify_pid;
    edf_channel_resource_t *resource = NULL;
    ErlNifUInt64 barrier_fragment_id = 0;

    if (argc != 3) {
        return EXCP_BADARG(env, "argc must be 3");
    }
    if (!enif_get_local_pid(env, argv[0], &notify_pid)) {
        return EXCP_BADARG(env, "NotifyPid must be a local pid");
    }
    if (!enif_get_resource(env, argv[1], edf_channel_resource_type, (void **)&resource)) {
        return EXCP_BADARG(env, "Channel Resource reference is invalid");
    }
    if (!enif_get_uint64(env, argv[2], &barrier_fragment_id)) {
        return EXCP_BADARG(env, "BarrierFragmentId must be a non-negative integer (0 disables the barrier)");
    }

    (void)enif_mutex_lock(test_hook_mutex);
    test_hook_notify_pid = notify_pid;
    test_hook_resource = resource;
    test_hook_barrier_fragment_id = (uint64_t)barrier_fragment_id;
    test_hook_barrier = EDF_CHANNEL_TEST_HOOK_BARRIER_IDLE;
    test_hook_event_count = 0;
    (void)atomic_store_explicit(&test_hook_armed, true, memory_order_release);
    (void)enif_fprintf(stderr, "[edf_test_hook] armed resource=%p barrier_fragment_id=%" PRIu64 "\n", (void *)resource,
                       (uint64_t)barrier_fragment_id);
    (void)enif_mutex_unlock(test_hook_mutex);

    return ATOM(ok);
}

ERL_NIF_TERM
erldist_filter_nif_test_hook_open_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argv;

    if (argc != 0) {
        return EXCP_BADARG(env, "argc must be 0");
    }

    (void)enif_mutex_lock(test_hook_mutex);
    test_hook_barrier = EDF_CHANNEL_TEST_HOOK_BARRIER_OPEN;
    (void)enif_fprintf(stderr, "[edf_test_hook] barrier opened resource=%p\n", (const void *)test_hook_resource);
    (void)enif_mutex_unlock(test_hook_mutex);

    return ATOM(ok);
}

ERL_NIF_TERM
erldist_filter_nif_test_hook_disarm_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argv;

    if (argc != 0) {
        return EXCP_BADARG(env, "argc must be 0");
    }

    (void)enif_mutex_lock(test_hook_mutex);
    (void)atomic_store_explicit(&test_hook_armed, false, memory_order_release);
    test_hook_resource = NULL;
    test_hook_barrier_fragment_id = 0;
    test_hook_barrier = EDF_CHANNEL_TEST_HOOK_BARRIER_IDLE;
    (void)enif_mutex_unlock(test_hook_mutex);

    return ATOM(ok);
}

#endif
