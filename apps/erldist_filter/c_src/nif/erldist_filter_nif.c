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

#include "channel/edf_atom_text.h"
#include "channel/edf_channel_impl.h"
#include "config/edf_config_impl.h"
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
static ERL_NIF_TERM erldist_filter_nif_atom_text_table_list_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erldist_filter_nif_atom_text_table_size_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erldist_filter_nif_atom_text_inspect_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erldist_filter_nif_atom_text_put_and_keep_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erldist_filter_nif_atom_text_release_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erldist_filter_nif_distribution_flags_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erldist_filter_nif_internal_hash_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erldist_filter_nif_dist_ext_to_term_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erldist_filter_nif_router_info_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erldist_filter_nif_router_name_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

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
erldist_filter_nif_atom_text_table_list_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 0) {
        return EXCP_BADARG(env, "argc must be 0");
    }
    return edf_atom_text_table_list(env);
}

ERL_NIF_TERM
erldist_filter_nif_atom_text_table_size_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 0) {
        return EXCP_BADARG(env, "argc must be 0");
    }
    return enif_make_int(env, edf_atom_text_table_size());
}

ERL_NIF_TERM
erldist_filter_nif_atom_text_inspect_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary atom_text_bin;

    if (argc != 1) {
        return EXCP_BADARG(env, "argc must be 1");
    }
    if (!enif_is_atom(env, argv[0])) {
        return EXCP_BADARG(env, "Atom is invalid (must be an atom)");
    }
    if (!edf_atom_text_inspect_as_binary(argv[0], &atom_text_bin)) {
        return ATOM(error);
    }

    return enif_make_binary(env, &atom_text_bin);
}

ERL_NIF_TERM
erldist_filter_nif_atom_text_put_and_keep_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary atom_text_bin;
    ErtsAtomEncoding atom_encoding;
    ERL_NIF_TERM atom;

    if (argc != 2) {
        return EXCP_BADARG(env, "argc must be 2");
    }
    if (!enif_inspect_binary(env, argv[0], &atom_text_bin)) {
        return EXCP_BADARG(env, "AtomText is invalid (must be a binary)");
    }
    if (argv[1] == ATOM(latin1)) {
        atom_encoding = ERTS_ATOM_ENC_LATIN1;
    } else if (argv[1] == ATOM(utf8)) {
        atom_encoding = ERTS_ATOM_ENC_UTF8;
    } else {
        return EXCP_BADARG(env, "AtomEncoding is invalid (must be one of 'latin1' or 'utf8')");
    }

    if (!edf_atom_text_put_and_keep((void *)atom_text_bin.data, (signed int)atom_text_bin.size, atom_encoding, &atom)) {
        return EXCP_ERROR(env, "Call to edf_atom_text_put_and_keep() failed");
    }

    return atom;
}

ERL_NIF_TERM
erldist_filter_nif_atom_text_release_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 1) {
        return EXCP_BADARG(env, "argc must be 1");
    }
    if (!enif_is_atom(env, argv[0])) {
        return EXCP_BADARG(env, "Atom is invalid (must be an atom)");
    }
    (void)edf_atom_text_release(argv[0]);

    return ATOM(ok);
}

ERL_NIF_TERM
erldist_filter_nif_distribution_flags_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
#define NUMBER_OF_DISTRIBUTION_FLAGS (42)
    ERL_NIF_TERM keys[NUMBER_OF_DISTRIBUTION_FLAGS];
    ERL_NIF_TERM vals[NUMBER_OF_DISTRIBUTION_FLAGS];
    size_t k = 0;
    size_t v = 0;
    ERL_NIF_TERM out_term;

    if (argc != 0) {
        return EXCP_BADARG(env, "argc must be 0");
    }

    keys[k++] = enif_make_atom(env, "DFLAG_PUBLISHED");
    vals[v++] = enif_make_uint64(env, DFLAG_PUBLISHED);
    keys[k++] = enif_make_atom(env, "DFLAG_ATOM_CACHE");
    vals[v++] = enif_make_uint64(env, DFLAG_ATOM_CACHE);
    keys[k++] = enif_make_atom(env, "DFLAG_EXTENDED_REFERENCES");
    vals[v++] = enif_make_uint64(env, DFLAG_EXTENDED_REFERENCES);
    keys[k++] = enif_make_atom(env, "DFLAG_DIST_MONITOR");
    vals[v++] = enif_make_uint64(env, DFLAG_DIST_MONITOR);
    keys[k++] = enif_make_atom(env, "DFLAG_FUN_TAGS");
    vals[v++] = enif_make_uint64(env, DFLAG_FUN_TAGS);
    keys[k++] = enif_make_atom(env, "DFLAG_DIST_MONITOR_NAME");
    vals[v++] = enif_make_uint64(env, DFLAG_DIST_MONITOR_NAME);
    keys[k++] = enif_make_atom(env, "DFLAG_HIDDEN_ATOM_CACHE");
    vals[v++] = enif_make_uint64(env, DFLAG_HIDDEN_ATOM_CACHE);
    keys[k++] = enif_make_atom(env, "DFLAG_NEW_FUN_TAGS");
    vals[v++] = enif_make_uint64(env, DFLAG_NEW_FUN_TAGS);
    keys[k++] = enif_make_atom(env, "DFLAG_EXTENDED_PIDS_PORTS");
    vals[v++] = enif_make_uint64(env, DFLAG_EXTENDED_PIDS_PORTS);
    keys[k++] = enif_make_atom(env, "DFLAG_EXPORT_PTR_TAG");
    vals[v++] = enif_make_uint64(env, DFLAG_EXPORT_PTR_TAG);
    keys[k++] = enif_make_atom(env, "DFLAG_BIT_BINARIES");
    vals[v++] = enif_make_uint64(env, DFLAG_BIT_BINARIES);
    keys[k++] = enif_make_atom(env, "DFLAG_NEW_FLOATS");
    vals[v++] = enif_make_uint64(env, DFLAG_NEW_FLOATS);
    keys[k++] = enif_make_atom(env, "DFLAG_UNICODE_IO");
    vals[v++] = enif_make_uint64(env, DFLAG_UNICODE_IO);
    keys[k++] = enif_make_atom(env, "DFLAG_DIST_HDR_ATOM_CACHE");
    vals[v++] = enif_make_uint64(env, DFLAG_DIST_HDR_ATOM_CACHE);
    keys[k++] = enif_make_atom(env, "DFLAG_SMALL_ATOM_TAGS");
    vals[v++] = enif_make_uint64(env, DFLAG_SMALL_ATOM_TAGS);
    keys[k++] = enif_make_atom(env, "DFLAG_ETS_COMPRESSED");
    vals[v++] = enif_make_uint64(env, DFLAG_ETS_COMPRESSED);
    keys[k++] = enif_make_atom(env, "DFLAG_UTF8_ATOMS");
    vals[v++] = enif_make_uint64(env, DFLAG_UTF8_ATOMS);
    keys[k++] = enif_make_atom(env, "DFLAG_MAP_TAG");
    vals[v++] = enif_make_uint64(env, DFLAG_MAP_TAG);
    keys[k++] = enif_make_atom(env, "DFLAG_BIG_CREATION");
    vals[v++] = enif_make_uint64(env, DFLAG_BIG_CREATION);
    keys[k++] = enif_make_atom(env, "DFLAG_SEND_SENDER");
    vals[v++] = enif_make_uint64(env, DFLAG_SEND_SENDER);
    keys[k++] = enif_make_atom(env, "DFLAG_BIG_SEQTRACE_LABELS");
    vals[v++] = enif_make_uint64(env, DFLAG_BIG_SEQTRACE_LABELS);
    keys[k++] = enif_make_atom(env, "DFLAG_PENDING_CONNECT");
    vals[v++] = enif_make_uint64(env, DFLAG_PENDING_CONNECT);
    keys[k++] = enif_make_atom(env, "DFLAG_EXIT_PAYLOAD");
    vals[v++] = enif_make_uint64(env, DFLAG_EXIT_PAYLOAD);
    keys[k++] = enif_make_atom(env, "DFLAG_FRAGMENTS");
    vals[v++] = enif_make_uint64(env, DFLAG_FRAGMENTS);
    keys[k++] = enif_make_atom(env, "DFLAG_HANDSHAKE_23");
    vals[v++] = enif_make_uint64(env, DFLAG_HANDSHAKE_23);
    keys[k++] = enif_make_atom(env, "DFLAG_UNLINK_ID");
    vals[v++] = enif_make_uint64(env, DFLAG_UNLINK_ID);
    keys[k++] = enif_make_atom(env, "DFLAG_MANDATORY_25_DIGEST");
    vals[v++] = enif_make_uint64(env, DFLAG_MANDATORY_25_DIGEST);
    keys[k++] = enif_make_atom(env, "DFLAG_RESERVED");
    vals[v++] = enif_make_uint64(env, DFLAG_RESERVED);
    keys[k++] = enif_make_atom(env, "DFLAG_SPAWN");
    vals[v++] = enif_make_uint64(env, DFLAG_SPAWN);
    keys[k++] = enif_make_atom(env, "DFLAG_NAME_ME");
    vals[v++] = enif_make_uint64(env, DFLAG_NAME_ME);
    keys[k++] = enif_make_atom(env, "DFLAG_V4_NC");
    vals[v++] = enif_make_uint64(env, DFLAG_V4_NC);
    keys[k++] = enif_make_atom(env, "DFLAG_ALIAS");
    vals[v++] = enif_make_uint64(env, DFLAG_ALIAS);
    keys[k++] = enif_make_atom(env, "DFLAG_DETERMINISTIC");
    vals[v++] = enif_make_uint64(env, DFLAG_DETERMINISTIC);
    keys[k++] = enif_make_atom(env, "DFLAG_DIST_MANDATORY_25");
    vals[v++] = enif_make_uint64(env, DFLAG_DIST_MANDATORY_25);
    keys[k++] = enif_make_atom(env, "DFLAG_DIST_MANDATORY_26");
    vals[v++] = enif_make_uint64(env, DFLAG_DIST_MANDATORY_26);
    keys[k++] = enif_make_atom(env, "DFLAG_DIST_MANDATORY");
    vals[v++] = enif_make_uint64(env, DFLAG_DIST_MANDATORY);
    keys[k++] = enif_make_atom(env, "DFLAG_DIST_HOPEFULLY");
    vals[v++] = enif_make_uint64(env, DFLAG_DIST_HOPEFULLY);
    keys[k++] = enif_make_atom(env, "DFLAG_DIST_DEFAULT");
    vals[v++] = enif_make_uint64(env, DFLAG_DIST_DEFAULT);
    keys[k++] = enif_make_atom(env, "DFLAG_DIST_ADDABLE");
    vals[v++] = enif_make_uint64(env, DFLAG_DIST_ADDABLE);
    keys[k++] = enif_make_atom(env, "DFLAG_DIST_REJECTABLE");
    vals[v++] = enif_make_uint64(env, DFLAG_DIST_REJECTABLE);
    keys[k++] = enif_make_atom(env, "DFLAG_DIST_STRICT_ORDER");
    vals[v++] = enif_make_uint64(env, DFLAG_DIST_STRICT_ORDER);
    keys[k++] = enif_make_atom(env, "TERM_TO_BINARY_DFLAGS");
    vals[v++] = enif_make_uint64(env, TERM_TO_BINARY_DFLAGS);

    if (!enif_make_map_from_arrays(env, keys, vals, NUMBER_OF_DISTRIBUTION_FLAGS, &out_term)) {
        return EXCP_ERROR(env, "Call to enif_make_map_from_arrays() failed: duplicate keys detected");
    }

#undef NUMBER_OF_DISTRIBUTION_FLAGS
    return out_term;
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

/* NIF Callbacks */

static ErlNifFunc erldist_filter_nif_funcs[] = {
    {"atom2cix", 1, erldist_filter_nif_atom2cix_1, ERL_NIF_NORMAL_JOB_BOUND},
    {"atom_text_table_list", 0, erldist_filter_nif_atom_text_table_list_0, ERL_NIF_NORMAL_JOB_BOUND},
    {"atom_text_table_size", 0, erldist_filter_nif_atom_text_table_size_0, ERL_NIF_NORMAL_JOB_BOUND},
    {"atom_text_inspect", 1, erldist_filter_nif_atom_text_inspect_1, ERL_NIF_NORMAL_JOB_BOUND},
    {"atom_text_put_and_keep", 2, erldist_filter_nif_atom_text_put_and_keep_2, ERL_NIF_NORMAL_JOB_BOUND},
    {"atom_text_release", 1, erldist_filter_nif_atom_text_release_1, ERL_NIF_NORMAL_JOB_BOUND},
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
    {"router_info", 0, erldist_filter_nif_router_info_0, ERL_NIF_NORMAL_JOB_BOUND},
    {"router_name", 1, erldist_filter_nif_router_name_1, ERL_NIF_NORMAL_JOB_BOUND},
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

    /* Initialize resource types. */
    retval = edf_world_load(env);
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

    if (!edf_atom_text_table_init()) {
        return -1;
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
            if (!edf_atom_text_put_and_keep((const uint8_t *)name, (signed int)name_len, ERTS_ATOM_ENC_UTF8,
                                            &erldist_filter_router_names[i])) {
                return -1;
            }
        }
        retval = 0;
    };

    /* Initialize private data. */
    (void)priv_data;
    (void)load_info;

    /* Initialize common atoms. */
    (void)erldist_filter_nif_make_atoms(env);

    erldist_filter_nif_instances++;

    return retval;
}

static int
erldist_filter_nif_upgrade(ErlNifEnv *env, void **new_priv_data, void **old_priv_data, ERL_NIF_TERM load_info)
{
    int retval = 0;

    /* Upgrade resource types. */
    retval = edf_world_load(env);
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

    if (!edf_atom_text_table_init()) {
        return -1;
    }

    /* Upgrade private data. */
    (void)env;
    (void)new_priv_data;
    (void)old_priv_data;
    (void)load_info;

    /* Initialize common atoms. */
    (void)erldist_filter_nif_make_atoms(env);

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
    (void)edf_world_unload(env);

    if (erldist_filter_nif_instances == 1) {
        /* Destroy private data. */
        (void)priv_data;
        (void)edf_atom_text_table_destroy();
    }

    erldist_filter_nif_instances--;

    return;
}

ERL_NIF_INIT(erldist_filter_nif, erldist_filter_nif_funcs, erldist_filter_nif_load, NULL, erldist_filter_nif_upgrade,
             erldist_filter_nif_unload);
