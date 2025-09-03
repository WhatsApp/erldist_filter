/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "erldist_filter_nif.h"

#include <stdbool.h>
#include <unistd.h>

#include "blocklist/edf_otp_name_blocklist.h"
#include "channel/edf_atom_text.h"
#include "channel/edf_channel_impl.h"
#include "config/edf_config_impl.h"
#include "erts/edf_erts_dist_impl.h"
#include "logger/edf_logger_impl.h"
#include "trap/edf_trap_impl.h"
#include "vterm/vterm_impl.h"
#include "world/edf_world_impl.h"

#include "erts/dist.h"
#include "erts/external.h"

#include "core/xnif_trace.h"

/* Global Variables */

uint64_t erldist_filter_router_count = 0;
ERL_NIF_TERM erldist_filter_router_names[ERLDIST_FILTER_ROUTER_LIMIT];

/* Resource Type Functions (Declarations) */

/* NIF Function Declarations */

static ERL_NIF_TERM erldist_filter_nif_atom2cix_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erldist_filter_nif_atom_text_get_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erldist_filter_nif_atom_text_put_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erldist_filter_nif_internal_hash_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erldist_filter_nif_dist_ext_to_term_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erldist_filter_nif_router_info_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erldist_filter_nif_router_name_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erldist_filter_nif_version_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

/* NIF Function Definitions */

ERL_NIF_TERM
erldist_filter_nif_atom2cix_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int cix;

    if (argc != 1) {
        return EXCP_BADARG(env, "argc must be 1");
    }
    if (!enif_is_atom(env, argv[0])) {
        return EXCP_BADARG(env, "Atom must be an atom");
    }

    cix = edf_atom_cache_index(argv[0]);
    return enif_make_int(env, cix);
}

ERL_NIF_TERM
erldist_filter_nif_atom_text_get_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary atom_text_bin;
    ErlNifCharEncoding atom_encoding;

    if (argc != 2) {
        return EXCP_BADARG(env, "argc must be 2");
    }
    if (!enif_is_atom(env, argv[0])) {
        return EXCP_BADARG(env, "Atom is invalid (must be an atom)");
    }
    if (argv[1] == ATOM(latin1)) {
        atom_encoding = ERL_NIF_LATIN1;
    } else if (argv[1] == ATOM(utf8)) {
        atom_encoding = ERL_NIF_UTF8;
    } else {
        return EXCP_BADARG(env, "AtomEncoding is invalid (must be one of 'latin1' or 'utf8')");
    }
    if (!edf_atom_text_inspect_as_binary(argv[0], atom_encoding, &atom_text_bin)) {
        return ATOM(error);
    }

    return enif_make_binary(env, &atom_text_bin);
}

ERL_NIF_TERM
erldist_filter_nif_atom_text_put_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary atom_text_bin;
    ErlNifCharEncoding atom_encoding;
    ERL_NIF_TERM atom;

    if (argc != 2) {
        return EXCP_BADARG(env, "argc must be 2");
    }
    if (!enif_inspect_binary(env, argv[0], &atom_text_bin)) {
        return EXCP_BADARG(env, "AtomText is invalid (must be a binary)");
    }
    if (argv[1] == ATOM(latin1)) {
        atom_encoding = ERL_NIF_LATIN1;
    } else if (argv[1] == ATOM(utf8)) {
        atom_encoding = ERL_NIF_UTF8;
    } else {
        return EXCP_BADARG(env, "AtomEncoding is invalid (must be one of 'latin1' or 'utf8')");
    }

    if (!edf_atom_text_put((void *)atom_text_bin.data, atom_text_bin.size, atom_encoding, &atom)) {
        return EXCP_ERROR(env, "Call to edf_atom_text_put() failed");
    }

    return atom;
}

ERL_NIF_TERM
erldist_filter_nif_internal_hash_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifUInt64 hvalue;

    if (argc != 1) {
        return EXCP_BADARG(env, "argc must be 1");
    }

    hvalue = enif_hash(ERL_NIF_INTERNAL_HASH, argv[0], 0);
    return enif_make_uint64(env, hvalue);
}

ERL_NIF_TERM
erldist_filter_nif_dist_ext_to_term_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 2) {
        return EXCP_BADARG(env, "argc must be 2");
    }

    return vterm_env_direct_dist_ext_to_term(env, argv[0], argv[1]);
}

ERL_NIF_TERM
erldist_filter_nif_router_info_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
#define RET_MAP_SIZE (3)

    ERL_NIF_TERM keys[RET_MAP_SIZE];
    ERL_NIF_TERM vals[RET_MAP_SIZE];
    size_t k = 0;
    size_t v = 0;
    ERL_NIF_TERM out_term;

    keys[k++] = ATOM(count);
    vals[v++] = enif_make_uint64(env, erldist_filter_router_count);
    keys[k++] = ATOM(limit);
    vals[v++] = enif_make_uint64(env, ERLDIST_FILTER_ROUTER_LIMIT);
    keys[k++] = ATOM(names);
    vals[v++] = enif_make_list_from_array(env, erldist_filter_router_names, (unsigned int)erldist_filter_router_count);

    if (!enif_make_map_from_arrays(env, keys, vals, RET_MAP_SIZE, &out_term)) {
        return EXCP_BADARG(env, "Call to enif_make_map_from_arrays() failed: duplicate keys detected");
    }

    return out_term;

#undef RET_MAP_SIZE
}

ERL_NIF_TERM
erldist_filter_nif_router_name_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM sysname;

    if (argc != 1) {
        return EXCP_BADARG(env, "argc must be 1");
    }

    sysname = argv[0];

    if (!enif_is_atom(env, sysname)) {
        return EXCP_BADARG(env, "Sysname must be an atom");
    }

    return edf_channel_router_name(env, sysname);
}

ERL_NIF_TERM
erldist_filter_nif_version_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
#define RET_MAP_SIZE (2)

    if (argc != 0) {
        return EXCP_BADARG(env, "argc must be 2");
    }

    ERL_NIF_TERM keys[RET_MAP_SIZE];
    ERL_NIF_TERM vals[RET_MAP_SIZE];
    size_t k = 0;
    size_t v = 0;
    ERL_NIF_TERM out_term;

    static const char erldist_filter_nif_version_date[12] = __DATE__;
    static const char erldist_filter_nif_version_time[9] = __TIME__;

    keys[k++] = ATOM(date);
    vals[v++] =
        enif_make_string_len(env, erldist_filter_nif_version_date, sizeof(erldist_filter_nif_version_date) - 1, ERL_NIF_LATIN1);
    keys[k++] = ATOM(time);
    vals[v++] =
        enif_make_string_len(env, erldist_filter_nif_version_time, sizeof(erldist_filter_nif_version_time) - 1, ERL_NIF_LATIN1);

    if (!enif_make_map_from_arrays(env, keys, vals, RET_MAP_SIZE, &out_term)) {
        return EXCP_BADARG(env, "Call to enif_make_map_from_arrays() failed: duplicate keys detected");
    }

    return out_term;

#undef RET_MAP_SIZE
}

/* NIF Callbacks */

static ErlNifFunc erldist_filter_nif_funcs[] = {
    {"altact_sig_flags", 0, erldist_filter_nif_altact_sig_flags_0, ERL_NIF_NORMAL_JOB_BOUND},
    {"atom2cix", 1, erldist_filter_nif_atom2cix_1, ERL_NIF_NORMAL_JOB_BOUND},
    {"atom_text_get", 2, erldist_filter_nif_atom_text_get_2, ERL_NIF_NORMAL_JOB_BOUND},
    {"atom_text_put", 2, erldist_filter_nif_atom_text_put_2, ERL_NIF_NORMAL_JOB_BOUND},
    {"channel_open", 5, erldist_filter_nif_channel_open_5, ERL_NIF_NORMAL_JOB_BOUND},
    {"channel_close", 1, erldist_filter_nif_channel_close_1, ERL_NIF_NORMAL_JOB_BOUND},
    {"channel_inspect", 1, erldist_filter_nif_channel_inspect_1, ERL_NIF_NORMAL_JOB_BOUND},
    {"channel_list", 0, erldist_filter_nif_channel_list_0, ERL_NIF_NORMAL_JOB_BOUND},
    {"channel_list", 1, erldist_filter_nif_channel_list_1, ERL_NIF_NORMAL_JOB_BOUND},
    {"channel_recv", 2, erldist_filter_nif_channel_recv_2, ERL_NIF_NORMAL_JOB_BOUND},
    {"channel_set_controlling_process", 2, erldist_filter_nif_channel_set_controlling_process_2, ERL_NIF_NORMAL_JOB_BOUND},
    {"channel_set_tracing_process", 2, erldist_filter_nif_channel_set_tracing_process_2, ERL_NIF_NORMAL_JOB_BOUND},
    {"config_get", 0, erldist_filter_nif_config_get_0, ERL_NIF_NORMAL_JOB_BOUND},
    {"config_get", 1, erldist_filter_nif_config_get_1, ERL_NIF_NORMAL_JOB_BOUND},
    {"config_set", 2, erldist_filter_nif_config_set_2, ERL_NIF_NORMAL_JOB_BOUND},
    {"dist_ext_to_term", 2, erldist_filter_nif_dist_ext_to_term_2, ERL_NIF_NORMAL_JOB_BOUND},
    {"dist_ext_to_vdist", 2, erldist_filter_nif_dist_ext_to_vdist_2, ERL_NIF_NORMAL_JOB_BOUND},
    {"dist_ext_to_vterm", 2, erldist_filter_nif_dist_ext_to_vterm_2, ERL_NIF_NORMAL_JOB_BOUND},
    {"dist_ext_to_vterm", 3, erldist_filter_nif_dist_ext_to_vterm_3, ERL_NIF_NORMAL_JOB_BOUND},
    {"dist_int_to_vdist", 2, erldist_filter_nif_dist_int_to_vdist_2, ERL_NIF_NORMAL_JOB_BOUND},
    {"dist_int_to_vterm", 2, erldist_filter_nif_dist_int_to_vterm_2, ERL_NIF_NORMAL_JOB_BOUND},
    {"dist_int_to_vterm", 3, erldist_filter_nif_dist_int_to_vterm_3, ERL_NIF_NORMAL_JOB_BOUND},
    {"distribution_flags", 0, erldist_filter_nif_distribution_flags_0, ERL_NIF_NORMAL_JOB_BOUND},
    {"internal_hash", 1, erldist_filter_nif_internal_hash_1, ERL_NIF_NORMAL_JOB_BOUND},
    {"logger_open", 0, erldist_filter_nif_logger_open_0, ERL_NIF_NORMAL_JOB_BOUND},
    {"logger_close", 1, erldist_filter_nif_logger_close_1, ERL_NIF_NORMAL_JOB_BOUND},
    {"logger_inspect", 1, erldist_filter_nif_logger_inspect_1, ERL_NIF_NORMAL_JOB_BOUND},
    {"logger_list", 0, erldist_filter_nif_logger_list_0, ERL_NIF_NORMAL_JOB_BOUND},
    {"logger_recv", 1, erldist_filter_nif_logger_recv_1, ERL_NIF_NORMAL_JOB_BOUND},
    {"logger_set_capacity", 1, erldist_filter_nif_logger_set_capacity_1, ERL_NIF_NORMAL_JOB_BOUND},
    {"logger_set_controlling_process", 2, erldist_filter_nif_logger_set_controlling_process_2, ERL_NIF_NORMAL_JOB_BOUND},
    {"otp_name_blocklist", 0, erldist_filter_nif_otp_name_blocklist_0, ERL_NIF_NORMAL_JOB_BOUND},
    {"otp_name_is_blocked", 1, erldist_filter_nif_otp_name_is_blocked_1, ERL_NIF_NORMAL_JOB_BOUND},
    {"router_info", 0, erldist_filter_nif_router_info_0, ERL_NIF_NORMAL_JOB_BOUND},
    {"router_name", 1, erldist_filter_nif_router_name_1, ERL_NIF_NORMAL_JOB_BOUND},
    {"spawn_flags", 0, erldist_filter_nif_spawn_flags_0, ERL_NIF_NORMAL_JOB_BOUND},
    {"version", 0, erldist_filter_nif_version_0, ERL_NIF_NORMAL_JOB_BOUND},
    {"world_stats_get", 0, erldist_filter_nif_world_stats_get_0, ERL_NIF_NORMAL_JOB_BOUND},
};

static int erldist_filter_nif_instances = 0;

static int erldist_filter_nif_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info);
static int erldist_filter_nif_upgrade(ErlNifEnv *env, void **new_priv_data, void **old_priv_data, ERL_NIF_TERM load_info);
static void erldist_filter_nif_unload(ErlNifEnv *env, void *priv_data);

static int
erldist_filter_nif_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    int retval = 0;

    /* Initialize common atoms. */
    (void)erldist_filter_nif_make_atoms(env);

    /* Initialize resource types. */
    retval = edf_world_load(env);
    if (retval != 0) {
        return retval;
    }

    retval = edf_otp_name_blocklist_load(env);
    if (retval != 0) {
        return retval;
    }

    retval = edf_channel_load(env);
    if (retval != 0) {
        return retval;
    }

    retval = edf_logger_load(env);
    if (retval != 0) {
        return retval;
    }

    retval = edf_trap_load(env);
    if (retval != 0) {
        return retval;
    }

    retval = vterm_env_load(env);
    if (retval != 0) {
        return retval;
    }

    /* Initialize scheduler specific data. */
    {
        ErlNifSysInfo sys_info[1];
        uint64_t i;
        char name[] = "erldist_filter_router_XXXXXXXXXXXXXXXXXXXX";
        size_t name_len = 0;
        (void)enif_system_info(sys_info, sizeof(ErlNifSysInfo));
        erldist_filter_router_count = (uint64_t)(sys_info->scheduler_threads);
        if (erldist_filter_router_count > ERLDIST_FILTER_ROUTER_LIMIT) {
            erldist_filter_router_count = ERLDIST_FILTER_ROUTER_LIMIT;
        }
        if (erldist_filter_router_count == 0) {
            erldist_filter_router_count = 1;
        }
        for (i = 0; i < erldist_filter_router_count; i++) {
            retval = enif_snprintf(name, sizeof(name), "erldist_filter_router_%llu", i + 1);
            if (retval < 0) {
                return retval;
            }
            name_len = (size_t)retval;
            if (!edf_atom_text_put((const uint8_t *)name, (signed int)name_len, ERL_NIF_UTF8, &erldist_filter_router_names[i])) {
                return -1;
            }
        }
        retval = 0;
    };

    /* Initialize private data. */
    (void)priv_data;
    (void)load_info;

    erldist_filter_nif_instances++;

    return retval;
}

static int
erldist_filter_nif_upgrade(ErlNifEnv *env, void **new_priv_data, void **old_priv_data, ERL_NIF_TERM load_info)
{
    int retval = 0;

    /* Initialize common atoms. */
    (void)erldist_filter_nif_make_atoms(env);

    /* Upgrade resource types. */
    retval = edf_world_load(env);
    if (retval != 0) {
        return retval;
    }

    retval = edf_otp_name_blocklist_load(env);
    if (retval != 0) {
        return retval;
    }

    retval = edf_channel_load(env);
    if (retval != 0) {
        return retval;
    }

    retval = edf_logger_load(env);
    if (retval != 0) {
        return retval;
    }

    retval = edf_trap_load(env);
    if (retval != 0) {
        return retval;
    }

    retval = vterm_env_load(env);
    if (retval != 0) {
        return retval;
    }

    /* Upgrade private data. */
    (void)env;
    (void)new_priv_data;
    (void)old_priv_data;
    (void)load_info;

    erldist_filter_nif_instances++;

    return retval;
}

static void
erldist_filter_nif_unload(ErlNifEnv *env, void *priv_data)
{
    (void)env;

    (void)vterm_env_unload(env);
    (void)edf_trap_unload(env);
    (void)edf_logger_unload(env);
    (void)edf_channel_unload(env);
    (void)edf_otp_name_blocklist_unload(env);
    (void)edf_world_unload(env);

    if (erldist_filter_nif_instances == 1) {
        /* Destroy private data. */
        (void)priv_data;
    }

    erldist_filter_nif_instances--;

    return;
}

ERL_NIF_INIT(erldist_filter_nif, erldist_filter_nif_funcs, erldist_filter_nif_load, NULL, erldist_filter_nif_upgrade,
             erldist_filter_nif_unload);
