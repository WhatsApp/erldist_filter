/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "edf_world_impl.h"
#include "../channel/edf_channel_inspect.h"
#include "../core/simd.h"

static int edf_world_make_stat_group_channel(ErlNifEnv *env, edf_world_stat_group_channel_t *stats, ERL_NIF_TERM *termp);
static int edf_world_make_stat_group_memory(ErlNifEnv *env, edf_world_stat_group_memory_t *stats, ERL_NIF_TERM *termp);

ERL_NIF_TERM
erldist_filter_nif_world_stats_get_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
#define RET_MAP_SIZE (3)

    ERL_NIF_TERM out_term;
    ERL_NIF_TERM keys[RET_MAP_SIZE];
    ERL_NIF_TERM vals[RET_MAP_SIZE];
    size_t k = 0;
    size_t v = 0;
    ErlNifUInt64 slots;
    edf_world_stats_t acc[1];
    edf_world_slot_t *root = NULL;
    edf_world_slot_t *slot = NULL;

    if (argc != 0) {
        return EXCP_BADARG(env, "argc must be 0");
    }

    (void)memset((void *)acc, 0, sizeof(edf_world_stats_t));

    slots = 0;
    (void)core_rwlock_read_lock(&edf_world_table->rwlock);
    root = (void *)&edf_world_table->_link;
    slot = (void *)root->_link.next;
    while (slot != root) {
        (void)core_simd_add_vec_u64((uint64_t *)acc, (const uint64_t *)&slot->stats,
                                    (sizeof(edf_world_stats_t) / sizeof(uint64_t)));
        slots += 1;
        slot = (void *)slot->_link.next;
    }
    (void)core_rwlock_read_unlock(&edf_world_table->rwlock);
    keys[k++] = ATOM(channel);
    if (!edf_world_make_stat_group_channel(env, &acc->channel, &vals[v++])) {
        return vals[v - 1];
    }
    keys[k++] = ATOM(memory);
    if (!edf_world_make_stat_group_memory(env, &acc->memory, &vals[v++])) {
        return vals[v - 1];
    }

    keys[k++] = ATOM(slots);
    vals[v++] = enif_make_uint64(env, slots);

    if (!enif_make_map_from_arrays(env, keys, vals, RET_MAP_SIZE, &out_term)) {
        return EXCP_BADARG(env, "Call to enif_make_map_from_arrays() failed: duplicate keys detected");
    }

    return out_term;

#undef RET_MAP_SIZE
}

int
edf_world_make_stat_group_channel(ErlNifEnv *env, edf_world_stat_group_channel_t *stats, ERL_NIF_TERM *termp)
{
#define RET_MAP_SIZE (3)

    ERL_NIF_TERM keys[RET_MAP_SIZE];
    ERL_NIF_TERM vals[RET_MAP_SIZE];
    size_t k = 0;
    size_t v = 0;

    keys[k++] = ATOM(create);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)stats->create);
    keys[k++] = ATOM(destroy);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)stats->destroy);
    keys[k++] = ATOM(rx_stats);
    if (!edf_channel_inspect_stats(env, &stats->rx_stats, &vals[v++])) {
        return vals[v - 1];
    }

    if (!enif_make_map_from_arrays(env, keys, vals, RET_MAP_SIZE, termp)) {
        *termp = EXCP_BADARG(env, "Call to enif_make_map_from_arrays() failed: duplicate keys detected");
        return 0;
    }

    return 1;

#undef RET_MAP_SIZE
}

int
edf_world_make_stat_group_memory(ErlNifEnv *env, edf_world_stat_group_memory_t *stats, ERL_NIF_TERM *termp)
{
#define RET_MAP_SIZE (14)

    ERL_NIF_TERM keys[RET_MAP_SIZE];
    ERL_NIF_TERM vals[RET_MAP_SIZE];
    size_t k = 0;
    size_t v = 0;

    keys[k++] = ATOM(vec_own_bin_create);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)stats->vec_own_bin_create);
    keys[k++] = ATOM(vec_own_bin_create_capacity);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)stats->vec_own_bin_create_capacity);
    keys[k++] = ATOM(vec_own_bin_realloc);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)stats->vec_own_bin_realloc);
    keys[k++] = ATOM(vec_own_bin_realloc_capacity);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)stats->vec_own_bin_realloc_capacity);
    keys[k++] = ATOM(vec_own_bin_destroy);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)stats->vec_own_bin_destroy);
    keys[k++] = ATOM(vec_own_mem_create);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)stats->vec_own_mem_create);
    keys[k++] = ATOM(vec_own_mem_create_capacity);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)stats->vec_own_mem_create_capacity);
    keys[k++] = ATOM(vec_own_mem_realloc);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)stats->vec_own_mem_realloc);
    keys[k++] = ATOM(vec_own_mem_realloc_capacity);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)stats->vec_own_mem_realloc_capacity);
    keys[k++] = ATOM(vec_own_mem_destroy);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)stats->vec_own_mem_destroy);
    keys[k++] = ATOM(vec_ref_bin_create);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)stats->vec_ref_bin_create);
    keys[k++] = ATOM(vec_ref_bin_destroy);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)stats->vec_ref_bin_destroy);
    keys[k++] = ATOM(vec_ref_ioq_create);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)stats->vec_ref_ioq_create);
    keys[k++] = ATOM(vec_ref_ioq_destroy);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)stats->vec_ref_ioq_destroy);

    if (!enif_make_map_from_arrays(env, keys, vals, RET_MAP_SIZE, termp)) {
        *termp = EXCP_BADARG(env, "Call to enif_make_map_from_arrays() failed: duplicate keys detected");
        return 0;
    }

    return 1;

#undef RET_MAP_SIZE
}
