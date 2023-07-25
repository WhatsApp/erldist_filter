/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "edf_world_impl.h"

ERL_NIF_TERM
erldist_filter_nif_world_stats_get_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
#define RET_MAP_SIZE (2)

    ERL_NIF_TERM out_term;
    ERL_NIF_TERM keys[RET_MAP_SIZE];
    ERL_NIF_TERM vals[RET_MAP_SIZE];
    size_t k = 0;
    size_t v = 0;

    if (argc != 0) {
        return EXCP_BADARG(env, "argc must be 0");
    }

    keys[k++] = ATOM(channels_created);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)(atomic_load(&edf_world->channels_created)));
    keys[k++] = ATOM(channels_destroyed);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)(atomic_load(&edf_world->channels_destroyed)));

    if (!enif_make_map_from_arrays(env, keys, vals, RET_MAP_SIZE, &out_term)) {
        return EXCP_BADARG(env, "Call to enif_make_map_from_arrays() failed: duplicate keys detected");
    }

    return out_term;

#undef RET_MAP_SIZE
}
