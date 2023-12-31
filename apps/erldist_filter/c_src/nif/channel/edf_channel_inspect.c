/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "edf_channel_inspect.h"

int
edf_channel_inspect_atom_cache(ErlNifEnv *env, edf_atom_cache_t *cache, ERL_NIF_TERM *termp)
{
    ERL_NIF_TERM ordlist;
    int i;

    if (cache == NULL) {
        *termp = ATOM(undefined);
        return 1;
    }

    ordlist = enif_make_list(env, 0);
    for (i = ERTS_ATOM_CACHE_SIZE - 1; i >= 0; i -= 1) {
        if (cache->entries[i] != THE_NON_VALUE) {
            ERL_NIF_TERM entry;
            entry = enif_make_tuple2(env, enif_make_int(env, i), cache->entries[i]);
            ordlist = enif_make_list_cell(env, entry, ordlist);
        }
    }

    *termp = ordlist;

    return 1;
}

int
edf_channel_inspect_entry(ErlNifEnv *env, edf_channel_t *channel, ERL_NIF_TERM *termp)
{
#define RET_MAP_SIZE (4)

    ERL_NIF_TERM keys[RET_MAP_SIZE];
    ERL_NIF_TERM vals[RET_MAP_SIZE];
    size_t k = 0;
    size_t v = 0;

    keys[k++] = ATOM(connection_id);
    vals[v++] = enif_make_uint(env, (unsigned int)channel->connection_id);
    keys[k++] = ATOM(creation);
    vals[v++] = enif_make_uint(env, (unsigned int)channel->creation);
    keys[k++] = ATOM(dflags);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)channel->dflags);
    keys[k++] = ATOM(sysname);
    vals[v++] = channel->sysname;

    if (!enif_make_map_from_arrays(env, keys, vals, RET_MAP_SIZE, termp)) {
        *termp = EXCP_BADARG(env, "Call to enif_make_map_from_arrays() failed: duplicate keys detected");
        return 0;
    }

    return 1;

#undef RET_MAP_SIZE
}

int
edf_channel_inspect_rx(ErlNifEnv *env, edf_channel_t *channel, ERL_NIF_TERM *termp)
{
#define RET_MAP_SIZE (6)

    ERL_NIF_TERM keys[RET_MAP_SIZE];
    ERL_NIF_TERM vals[RET_MAP_SIZE];
    size_t k = 0;
    size_t v = 0;

    keys[k++] = ATOM(atom_cache);
    if (!edf_channel_inspect_atom_cache(env, channel->rx.cache, &vals[v])) {
        *termp = vals[v];
        return 0;
    }
    v += 1;
    keys[k++] = ATOM(ioq_size);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)ioq_size(&channel->rx.ioq));
    keys[k++] = ATOM(packet_size);
    vals[v++] = enif_make_uint(env, (unsigned int)channel->rx.packet_size);
    keys[k++] = ATOM(stats);
    if (!edf_channel_inspect_stats(env, &channel->rx.stats, &vals[v])) {
        *termp = vals[v];
        return 0;
    }
    v += 1;
    keys[k++] = ATOM(vec_capacity);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)vec_capacity(&channel->rx.vec));
    keys[k++] = ATOM(vec_len);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)vec_len(&channel->rx.vec));

    if (!enif_make_map_from_arrays(env, keys, vals, RET_MAP_SIZE, termp)) {
        *termp = EXCP_BADARG(env, "Call to enif_make_map_from_arrays() failed: duplicate keys detected");
        return 0;
    }

    return 1;

#undef RET_MAP_SIZE
}

static int edf_channel_inspect_stats_dop(ErlNifEnv *env, edf_channel_stats_dop_t *dop, ERL_NIF_TERM *termp);

int
edf_channel_inspect_stats(ErlNifEnv *env, edf_channel_stats_t *stats, ERL_NIF_TERM *termp)
{
#define RET_MAP_SIZE (49)

    ERL_NIF_TERM keys[RET_MAP_SIZE];
    ERL_NIF_TERM vals[RET_MAP_SIZE];
    size_t k = 0;
    size_t v = 0;

    keys[k++] = ATOM(packet_count);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)stats->packet_count);
    keys[k++] = ATOM(emit_count);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)stats->emit_count);
    keys[k++] = ATOM(drop_count);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)stats->drop_count);
    keys[k++] = ATOM(dist_header_count);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)stats->dist_header_count);
    keys[k++] = ATOM(dist_frag_header_count);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)stats->dist_frag_header_count);
    keys[k++] = ATOM(dist_frag_cont_count);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)stats->dist_frag_cont_count);
    keys[k++] = ATOM(dist_pass_through_count);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)stats->dist_pass_through_count);
    keys[k++] = ATOM(atom_cache_read_count);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)stats->atom_cache_read_count);
    keys[k++] = ATOM(atom_cache_write_count);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)stats->atom_cache_write_count);
    keys[k++] = ATOM(atom_cache_overwrite_count);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)stats->atom_cache_overwrite_count);
    keys[k++] = ATOM(rewrite_fragment_header_count);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)stats->rewrite_fragment_header_count);
    keys[k++] = ATOM(rollback_atom_cache_count);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)stats->rollback_atom_cache_count);
    keys[k++] = ATOM(compact_external_count);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)stats->compact_external_count);
    keys[k++] = ATOM(compact_fragment_count);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)stats->compact_fragment_count);
    keys[k++] = ATOM(control_has_export_ext);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)stats->control_has_export_ext);
    keys[k++] = ATOM(control_has_new_fun_ext);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)stats->control_has_new_fun_ext);
    keys[k++] = ATOM(payload_has_export_ext);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)stats->payload_has_export_ext);
    keys[k++] = ATOM(payload_has_new_fun_ext);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)stats->payload_has_new_fun_ext);
    keys[k++] = ATOM(fastpath);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)stats->fastpath);
    keys[k++] = ATOM(slowpath);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)stats->slowpath);
    keys[k++] = ATOM(dop_link);
    if (!edf_channel_inspect_stats_dop(env, &stats->dop_link, &vals[v++])) {
        return vals[v - 1];
    }
    keys[k++] = ATOM(dop_send);
    if (!edf_channel_inspect_stats_dop(env, &stats->dop_send, &vals[v++])) {
        return vals[v - 1];
    }
    keys[k++] = ATOM(dop_exit);
    if (!edf_channel_inspect_stats_dop(env, &stats->dop_exit, &vals[v++])) {
        return vals[v - 1];
    }
    keys[k++] = ATOM(dop_unlink);
    if (!edf_channel_inspect_stats_dop(env, &stats->dop_unlink, &vals[v++])) {
        return vals[v - 1];
    }
    keys[k++] = ATOM(dop_reg_send);
    if (!edf_channel_inspect_stats_dop(env, &stats->dop_reg_send, &vals[v++])) {
        return vals[v - 1];
    }
    keys[k++] = ATOM(dop_group_leader);
    if (!edf_channel_inspect_stats_dop(env, &stats->dop_group_leader, &vals[v++])) {
        return vals[v - 1];
    }
    keys[k++] = ATOM(dop_exit2);
    if (!edf_channel_inspect_stats_dop(env, &stats->dop_exit2, &vals[v++])) {
        return vals[v - 1];
    }
    keys[k++] = ATOM(dop_send_tt);
    if (!edf_channel_inspect_stats_dop(env, &stats->dop_send_tt, &vals[v++])) {
        return vals[v - 1];
    }
    keys[k++] = ATOM(dop_exit_tt);
    if (!edf_channel_inspect_stats_dop(env, &stats->dop_exit_tt, &vals[v++])) {
        return vals[v - 1];
    }
    keys[k++] = ATOM(dop_reg_send_tt);
    if (!edf_channel_inspect_stats_dop(env, &stats->dop_reg_send_tt, &vals[v++])) {
        return vals[v - 1];
    }
    keys[k++] = ATOM(dop_exit2_tt);
    if (!edf_channel_inspect_stats_dop(env, &stats->dop_exit2_tt, &vals[v++])) {
        return vals[v - 1];
    }
    keys[k++] = ATOM(dop_monitor_p);
    if (!edf_channel_inspect_stats_dop(env, &stats->dop_monitor_p, &vals[v++])) {
        return vals[v - 1];
    }
    keys[k++] = ATOM(dop_demonitor_p);
    if (!edf_channel_inspect_stats_dop(env, &stats->dop_demonitor_p, &vals[v++])) {
        return vals[v - 1];
    }
    keys[k++] = ATOM(dop_monitor_p_exit);
    if (!edf_channel_inspect_stats_dop(env, &stats->dop_monitor_p_exit, &vals[v++])) {
        return vals[v - 1];
    }
    keys[k++] = ATOM(dop_send_sender);
    if (!edf_channel_inspect_stats_dop(env, &stats->dop_send_sender, &vals[v++])) {
        return vals[v - 1];
    }
    keys[k++] = ATOM(dop_send_sender_tt);
    if (!edf_channel_inspect_stats_dop(env, &stats->dop_send_sender_tt, &vals[v++])) {
        return vals[v - 1];
    }
    keys[k++] = ATOM(dop_payload_exit);
    if (!edf_channel_inspect_stats_dop(env, &stats->dop_payload_exit, &vals[v++])) {
        return vals[v - 1];
    }
    keys[k++] = ATOM(dop_payload_exit_tt);
    if (!edf_channel_inspect_stats_dop(env, &stats->dop_payload_exit_tt, &vals[v++])) {
        return vals[v - 1];
    }
    keys[k++] = ATOM(dop_payload_exit2);
    if (!edf_channel_inspect_stats_dop(env, &stats->dop_payload_exit2, &vals[v++])) {
        return vals[v - 1];
    }
    keys[k++] = ATOM(dop_payload_exit2_tt);
    if (!edf_channel_inspect_stats_dop(env, &stats->dop_payload_exit2_tt, &vals[v++])) {
        return vals[v - 1];
    }
    keys[k++] = ATOM(dop_payload_monitor_p_exit);
    if (!edf_channel_inspect_stats_dop(env, &stats->dop_payload_monitor_p_exit, &vals[v++])) {
        return vals[v - 1];
    }
    keys[k++] = ATOM(dop_spawn_request);
    if (!edf_channel_inspect_stats_dop(env, &stats->dop_spawn_request, &vals[v++])) {
        return vals[v - 1];
    }
    keys[k++] = ATOM(dop_spawn_request_tt);
    if (!edf_channel_inspect_stats_dop(env, &stats->dop_spawn_request_tt, &vals[v++])) {
        return vals[v - 1];
    }
    keys[k++] = ATOM(dop_spawn_reply);
    if (!edf_channel_inspect_stats_dop(env, &stats->dop_spawn_reply, &vals[v++])) {
        return vals[v - 1];
    }
    keys[k++] = ATOM(dop_spawn_reply_tt);
    if (!edf_channel_inspect_stats_dop(env, &stats->dop_spawn_reply_tt, &vals[v++])) {
        return vals[v - 1];
    }
    keys[k++] = ATOM(dop_alias_send);
    if (!edf_channel_inspect_stats_dop(env, &stats->dop_alias_send, &vals[v++])) {
        return vals[v - 1];
    }
    keys[k++] = ATOM(dop_alias_send_tt);
    if (!edf_channel_inspect_stats_dop(env, &stats->dop_alias_send_tt, &vals[v++])) {
        return vals[v - 1];
    }
    keys[k++] = ATOM(dop_unlink_id);
    if (!edf_channel_inspect_stats_dop(env, &stats->dop_unlink_id, &vals[v++])) {
        return vals[v - 1];
    }
    keys[k++] = ATOM(dop_unlink_id_ack);
    if (!edf_channel_inspect_stats_dop(env, &stats->dop_unlink_id_ack, &vals[v++])) {
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
edf_channel_inspect_stats_dop(ErlNifEnv *env, edf_channel_stats_dop_t *dop, ERL_NIF_TERM *termp)
{
#define RET_MAP_SIZE (3)

    ERL_NIF_TERM keys[RET_MAP_SIZE];
    ERL_NIF_TERM vals[RET_MAP_SIZE];
    size_t k = 0;
    size_t v = 0;

    keys[k++] = ATOM(seen);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)dop->seen);
    keys[k++] = ATOM(emit);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)dop->emit);
    keys[k++] = ATOM(drop);
    vals[v++] = enif_make_uint64(env, (ErlNifUInt64)dop->drop);

    if (!enif_make_map_from_arrays(env, keys, vals, RET_MAP_SIZE, termp)) {
        *termp = EXCP_BADARG(env, "Call to enif_make_map_from_arrays() failed: duplicate keys detected");
        return 0;
    }

    return 1;

#undef RET_MAP_SIZE
}
