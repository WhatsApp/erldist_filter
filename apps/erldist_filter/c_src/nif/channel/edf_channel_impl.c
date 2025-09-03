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

#include "../config/edf_config.h"
#include "../core/xnif_simd.h"
#include "../erts/dist.h"
#include "../uterm/uterm.h"

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
    (void)xnif_mutex_lock(&edf_channel_resource_table->mutex);
    node = (void *)(root->_link.prev);
    while (root != node) {
        temp = (void *)(node->_link.prev);
        (void)xnif_rwlock_read_lock(&node->rwlock);
        if (node->inner != NULL) {
            list = enif_make_list_cell(env, enif_make_resource(env, (void *)node), list);
        }
        (void)xnif_rwlock_read_unlock(&node->rwlock);
        node = temp;
    }
    (void)xnif_mutex_unlock(&edf_channel_resource_table->mutex);

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
    (void)xnif_mutex_lock(&edf_channel_resource_table->mutex);
    node = (void *)(root->_link.prev);
    while (root != node) {
        temp = (void *)(node->_link.prev);
        (void)xnif_rwlock_read_lock(&node->rwlock);
        if (node->inner != NULL && node->inner->sysname == sysname) {
            list = enif_make_list_cell(env, enif_make_resource(env, (void *)node), list);
        }
        (void)xnif_rwlock_read_unlock(&node->rwlock);
        node = temp;
    }
    (void)xnif_mutex_unlock(&edf_channel_resource_table->mutex);

    return list;
}

static int channel_recv_2_fast(ErlNifEnv *env, edf_channel_t *channel, ERL_NIF_TERM input_term, ErlNifBinary *input_binary,
                               ERL_NIF_TERM *out_termp, ERL_NIF_TERM *err_termp);

ERL_NIF_TERM
erldist_filter_nif_channel_recv_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM out_term = THE_NON_VALUE;
    ERL_NIF_TERM err_term = THE_NON_VALUE;
    ERL_NIF_TERM trap_term;
    edf_channel_resource_t *resource = NULL;
    edf_channel_t *channel = NULL;
    int flags = (EDF_CHANNEL_RESOURCE_FLAG_OWNER_REQUIRED | EDF_CHANNEL_RESOURCE_FLAG_WRITE_LOCK);
    bool eligible_for_fast_recv = true;
    unsigned int iovec_length;
    ERL_NIF_TERM iovec_tail = THE_NON_VALUE;
    ERL_NIF_TERM input_term = THE_NON_VALUE;
    ErlNifBinary input_binary;

    if (argc != 2) {
        return EXCP_BADARG(env, "argc must be 2");
    }

    if (!edf_channel_resource_acquire(env, argv[0], &resource, &channel, &err_term, flags)) {
        return err_term;
    }

    if (ioq_size(&channel->rx.ioq) > 0 || edf_config_is_untrusted_enabled()) {
        eligible_for_fast_recv = false;
    }

    if (eligible_for_fast_recv && enif_is_list(env, argv[1]) && enif_get_list_length(env, argv[1], &iovec_length) &&
        iovec_length == 1 && enif_get_list_cell(env, argv[1], &input_term, &iovec_tail) && enif_is_binary(env, input_term) &&
        enif_is_empty_list(env, iovec_tail)) {
        eligible_for_fast_recv = true;
    } else {
        input_term = argv[1];
        eligible_for_fast_recv = (eligible_for_fast_recv && enif_is_binary(env, argv[1])) ? true : false;
    }

    if (eligible_for_fast_recv && enif_is_binary(env, input_term) && enif_inspect_binary(env, input_term, &input_binary)) {
        int retval = channel_recv_2_fast(env, channel, input_term, &input_binary, &out_term, &err_term);
        if (retval == 0) {
            (void)edf_channel_resource_release(&resource, &channel, flags);
            return err_term;
        } else if (retval == 1) {
            (void)edf_channel_resource_release(&resource, &channel, flags);
            return out_term;
        } else {
            eligible_for_fast_recv = false;
        }
    }

    if (enif_is_binary(env, input_term)) {
        input_term = enif_make_list1(env, input_term);
    }

    if (!ioq_inspect_iovec_and_enqv(env, ~(0ULL), input_term, &out_term, &channel->rx.ioq)) {
        (void)edf_channel_resource_release(&resource, &channel, flags);
        return EXCP_BADARG(env, "Input is invalid");
    }

    if (!enif_is_empty_list(env, out_term)) {
        (void)edf_channel_resource_release(&resource, &channel, flags);
        return EXCP_BADARG(env, "Call to enif_ioq_enqv() failed: expected tail to be an empty list");
    }

    CHANNEL_RX_STATS_COUNT(channel, slowpath, 1);
    trap_term = edf_channel_recv_trap_open(env, resource, channel, NULL);
    if (enif_is_exception(env, trap_term)) {
        (void)edf_channel_resource_release(&resource, &channel, flags);
        return trap_term;
    }
    (void)edf_channel_resource_release(&resource, &channel, flags);
    return edf_trap_schedule_from_term(env, trap_term);
}

int
channel_recv_2_fast(ErlNifEnv *env, edf_channel_t *channel, ERL_NIF_TERM input_term, ErlNifBinary *input_binary,
                    ERL_NIF_TERM *out_termp, ERL_NIF_TERM *err_termp)
{
    uint8_t dist_header_type;
    uint64_t fragment_id;
    const uint8_t *p = NULL;
    const uint8_t *pend = NULL;
    size_t packet_length = 0;

    if (input_binary->size == 0) {
        *out_termp = enif_make_list(env, 0);
        return 1;
    }

    p = (void *)input_binary->data;
    pend = p + input_binary->size;

    if (channel->rx.packet_size != 0) {
        if ((pend - p) < channel->rx.packet_size) {
            goto try_decode_slow;
        }
        switch (channel->rx.packet_size) {
        case 1:
            packet_length = (size_t)(*p++);
            break;
        case 2:
            packet_length = (size_t)(be16toh(*((uint16_t *)(void *)(p))));
            p += 2;
            break;
        case 4:
            packet_length = (size_t)(be32toh(*((uint32_t *)(void *)(p))));
            p += 4;
            break;
        case 8:
            packet_length = (size_t)(be64toh(*((uint64_t *)(void *)(p))));
            p += 8;
            break;
        default:
            goto try_decode_slow;
        }
        if ((pend - p) != packet_length) {
            goto try_decode_slow;
        }
    } else {
        packet_length = input_binary->size;
    }

    // This was a "tick" message where the packet is sized 0.
    // Consume the packet header and start waiting for the next packet.
    if (packet_length == 0) {
        CHANNEL_RX_STATS_COUNT(channel, fastpath, 1);
        CHANNEL_RX_STATS_COUNT(channel, packet_count, 1);
        CHANNEL_RX_STATS_COUNT(channel, dist_tick_count, 1);
        *out_termp = enif_make_list(env, 0);
        return 1;
    }

    if ((channel->dflags & (DFLAG_DIST_HDR_ATOM_CACHE | DFLAG_FRAGMENTS)) == 0) {
        // If none of these distribution flags are set, then we fallback to "pass-through" mode.
        // We expect a PASS_THROUGH byte to be present.
        if (*(p++) != PASS_THROUGH) {
            *err_termp = EXCP_ERROR_F(env, "Dist Frame in pass-through mode must have an 8-bit type field equal to %u (was %u)\n",
                                      PASS_THROUGH, *(p - 1));
            return 0;
        }
        dist_header_type = PASS_THROUGH;
        goto try_decode_slow;
    } else {
        // Otherwise, we expect a VERSION_MAGIC byte to be present.
        if (*(p++) != VERSION_MAGIC) {
            *err_termp = EXCP_ERROR_F(env, "Dist Frame in pass-through mode must have an 8-bit type field equal to %u (was %u)\n",
                                      PASS_THROUGH, *(p - 1));
            return 0;
        }
        if (*p == DIST_HEADER) {
            p++;
            if ((channel->dflags & (DFLAG_DIST_HDR_ATOM_CACHE)) == 0) {
                *err_termp = EXCP_ERROR(env, "Dist Frame tagged with 'DIST_HEADER' not support by current "
                                             "distribution flags (missing: DFLAG_DIST_HDR_ATOM_CACHE).\n");
                return 0;
            }
            dist_header_type = DIST_HEADER;
            goto try_decode_fast;
        } else if (*p == DIST_FRAG_HEADER) {
            p++;
            if ((channel->dflags & (DFLAG_FRAGMENTS)) == 0) {
                *err_termp = EXCP_ERROR(env, "Dist Frame tagged with 'DIST_FRAG_HEADER' not support by current "
                                             "distribution flags (missing: DFLAG_FRAGMENTS).\n");
                return 0;
            }
            if ((pend - p) < 16) {
                *err_termp = EXCP_ERROR_F(
                    env, "Dist Frame tagged with 'DIST_FRAG_HEADER' must be at least 16-bytes in size (was %u-bytes).\n",
                    (pend - p));
                return 0;
            }
            // skip sequence_id
            p += 8;
            fragment_id = be64toh(*((uint64_t *)(void *)(p)));
            p += 8;
            dist_header_type = DIST_FRAG_HEADER;
            if (fragment_id != 1) {
                goto try_decode_slow;
            }
            goto try_decode_fast;
        } else if (*p == DIST_FRAG_CONT) {
            dist_header_type = DIST_FRAG_CONT;
            goto try_decode_slow;
        } else {
            *err_termp = EXCP_ERROR_F(env, "Dist Frame tagged with unrecognized tag '%u' is not supported.\n", *p);
            return 0;
        }
    }
try_decode_fast: {
    vec_t vec[1];
    vec_t control_vec[1];
    vec_reader_t vr[1];
    edf_atom_translation_table_t attab[1];
    int external_flags = 0;
    slice_t headers[1];
    slice_t control[1];
    slice_t payload[1];
    vterm_env_t vtenv[1];
    udist_t up[1];
    (void)edf_atom_translation_table_init(attab);
    (void)vec_init_free(vec);
    (void)vec_create_from_slice(vec, p, pend);
    (void)vec_reader_create(vr, vec, 0);
    if (!etf_fast_decode_dist_header(env, channel, attab, vr, &external_flags, headers, err_termp)) {
        return 0;
    }
    control->head = vec_reader_raw_bytes(vr);
    if (!etf_fast_skip_terms(env, false, vr, 1, err_termp)) {
        return 0;
    }
    control->tail = vec_reader_raw_bytes(vr);
    vtenv->nif_env = env;
    vtenv->attab = attab;
    (void)vec_init_free(control_vec);
    (void)vec_create_from_slice(control_vec, control->head, control->tail);
    (void)udist_init(up);
    if (!etf_decode_udist_control(env, vtenv, false, true, control_vec, up, err_termp)) {
        return 0;
    }
    if (udist_control_is_send_to_name(up) &&
        (up->control.data.send.to == ATOM(net_kernel) || up->control.data.send.to == ATOM(rex))) {
        goto try_decode_slow;
    }
    if (up->info.payload && udist_control_is_send(up)) {
        payload->head = vec_reader_raw_bytes(vr);
        if (!etf_fast_skip_terms(env, false, vr, 1, err_termp)) {
            return 0;
        }
        payload->tail = vec_reader_raw_bytes(vr);
        if (!udist_classify(env, vtenv, up, false, false, payload, err_termp)) {
            return 0;
        }
        if (up->flags == UDIST_CLASSIFY_FLAG_EMIT) {
            if (packet_length < input_binary->size) {
                input_term = enif_make_sub_binary(env, input_term, channel->rx.packet_size, packet_length);
            }
            *out_termp = enif_make_list1(env, enif_make_tuple2(env, ATOM(emit), input_term));
            {
                edf_channel_stats_dop_t *statsdop = NULL;
                edf_world_slot_t *slot = NULL;
                CHANNEL_RX_STATS_COUNT(channel, fastpath, 1);
                CHANNEL_RX_STATS_COUNT(channel, packet_count, 1);
                CHANNEL_RX_STATS_COUNT(channel, emit_count, 1);
                switch (dist_header_type) {
                case PASS_THROUGH:
                    CHANNEL_RX_STATS_COUNT(channel, dist_pass_through_count, 1);
                    break;
                case DIST_HEADER:
                    CHANNEL_RX_STATS_COUNT(channel, dist_header_count, 1);
                    break;
                case DIST_FRAG_HEADER:
                    CHANNEL_RX_STATS_COUNT(channel, dist_frag_header_count, 1);
                    break;
                case DIST_FRAG_CONT:
                    CHANNEL_RX_STATS_COUNT(channel, dist_frag_cont_count, 1);
                    break;
                default:
                    break;
                }
                statsdop = NULL;
                if (udist_get_channel_stats_dop(up, &channel->rx.stats, &statsdop) && statsdop != NULL) {
                    statsdop->seen += 1;
                    statsdop->emit += 1;
                }
                statsdop = NULL;
                slot = edf_world_get();
                if (udist_get_channel_stats_dop(up, &slot->stats.channel.rx_stats, &statsdop) && statsdop != NULL) {
                    statsdop->seen += 1;
                    statsdop->emit += 1;
                }
            };
            return 1;
        }
    }
    goto try_decode_slow;
}
try_decode_slow: {
    return -1;
}
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

    if (xnif_demonitor_process(env, (void *)resource, &channel->owner) != 0) {
        // OwnerPid is about to exit, consider the channel closed.
        (void)edf_channel_resource_release(&resource, &channel, flags);
        return enif_make_tuple2(env, ATOM(error), ATOM(closed));
    }

    retval = xnif_monitor_process(env, (void *)resource, &new_owner_pid, &channel->owner);
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

    (void)xnif_demonitor_process(env, (void *)resource, &channel->trace);

    retval = xnif_monitor_process(env, (void *)resource, &new_trace_pid, &channel->owner);
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
