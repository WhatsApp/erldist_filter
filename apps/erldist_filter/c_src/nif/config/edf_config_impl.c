/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#define ERLDIST_FILTER_NIF_INTERNAL_API 1
#include "edf_config_impl.h"

static ERL_NIF_TERM edf_config_get_bool(ErlNifEnv *env, bool *val);
static ERL_NIF_TERM edf_config_set_bool(ErlNifEnv *env, bool *val, ERL_NIF_TERM val_term);

ERL_NIF_TERM
erldist_filter_nif_config_get_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
#define RET_MAP_SIZE (4)

    ERL_NIF_TERM out_term;
    ERL_NIF_TERM keys[RET_MAP_SIZE];
    ERL_NIF_TERM vals[RET_MAP_SIZE];
    size_t k = 0;
    size_t v = 0;

    if (argc != 0) {
        return EXCP_BADARG(env, "argc must be 0");
    }

    keys[k++] = ATOM(compact_fragments);
    vals[v++] = edf_config_get_bool(env, &edf_config_global->compact_fragments);
    keys[k++] = ATOM(deep_packet_inspection);
    vals[v++] = edf_config_get_bool(env, &edf_config_global->deep_packet_inspection);
    keys[k++] = ATOM(logging);
    vals[v++] = edf_config_get_bool(env, &edf_config_global->logging);
    keys[k++] = ATOM(redirect_dist_operations);
    vals[v++] = edf_config_get_bool(env, &edf_config_global->redirect_dist_operations);

    if (!enif_make_map_from_arrays(env, keys, vals, RET_MAP_SIZE, &out_term)) {
        return EXCP_BADARG(env, "Call to enif_make_map_from_arrays() failed: duplicate keys detected");
    }

    return out_term;

#undef RET_MAP_SIZE
}

ERL_NIF_TERM
erldist_filter_nif_config_get_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM key_term;
    ERL_NIF_TERM val_term = THE_NON_VALUE;

    if (argc != 1) {
        return EXCP_BADARG(env, "argc must be 1");
    }

    key_term = argv[0];

    if (key_term == ATOM(compact_fragments)) {
        val_term = edf_config_get_bool(env, &edf_config_global->compact_fragments);
    } else if (key_term == ATOM(deep_packet_inspection)) {
        val_term = edf_config_get_bool(env, &edf_config_global->deep_packet_inspection);
    } else if (key_term == ATOM(logging)) {
        val_term = edf_config_get_bool(env, &edf_config_global->logging);
    } else if (key_term == ATOM(redirect_dist_operations)) {
        val_term = edf_config_get_bool(env, &edf_config_global->redirect_dist_operations);
    }

    if (val_term == THE_NON_VALUE) {
        return EXCP_BADARG(env, "Key is invalid");
    }

    return val_term;
}

ERL_NIF_TERM
erldist_filter_nif_config_set_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM key_term;
    ERL_NIF_TERM val_term;

    if (argc != 2) {
        return EXCP_BADARG(env, "argc must be 2");
    }

    key_term = argv[0];
    val_term = argv[1];

    if (key_term == ATOM(compact_fragments)) {
        return edf_config_set_bool(env, &edf_config_global->compact_fragments, val_term);
    } else if (key_term == ATOM(deep_packet_inspection)) {
        return edf_config_set_bool(env, &edf_config_global->deep_packet_inspection, val_term);
    } else if (key_term == ATOM(logging)) {
        return edf_config_set_bool(env, &edf_config_global->logging, val_term);
    } else if (key_term == ATOM(redirect_dist_operations)) {
        return edf_config_set_bool(env, &edf_config_global->redirect_dist_operations, val_term);
    }

    return EXCP_BADARG(env, "Key is invalid");
}

ERL_NIF_TERM
edf_config_get_bool(ErlNifEnv *env, bool *val)
{
    (void)env;
    return (*val) ? ATOM(true) : ATOM(false);
}

ERL_NIF_TERM
edf_config_set_bool(ErlNifEnv *env, bool *val, ERL_NIF_TERM val_term)
{
    if (!(val_term == ATOM(true) || val_term == ATOM(false))) {
        return EXCP_BADARG(env, "Val must be a boolean");
    }
    *val = (val_term == ATOM(true)) ? true : false;
    return ATOM(ok);
}
