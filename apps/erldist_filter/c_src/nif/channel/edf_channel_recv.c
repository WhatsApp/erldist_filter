/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#define ERLDIST_FILTER_NIF_INTERNAL_API 1
#include "edf_channel_recv.h"
#undef ERLDIST_FILTER_NIF_INTERNAL_API
#include "edf_external_recv.h"
#include "../config/edf_config.h"
#include "../logger/edf_logger.h"
#include "../vterm/vterm_env.h"

#include "../erts/dist.h"
#include "../erts/external.h"

static int edf_channel_recv_trap_acquire(ErlNifEnv *caller_env, edf_trap_t *super, void *arg, ERL_NIF_TERM *error_term);
static void edf_channel_recv_trap_release(ErlNifEnv *caller_env, edf_trap_t *super, void *arg);
static void edf_channel_recv_trap_dtor(ErlNifEnv *caller_env, edf_trap_t *super, void *arg);
static void edf_channel_recv_trap_edit(ErlNifEnv *caller_env, edf_trap_t *super, void *arg, edf_trap_result_t *result);
static edf_trap_result_t edf_channel_recv_trap_next(ErlNifEnv *caller_env, edf_trap_t *super, void *arg);

static int channel_rx_stats_dop_seen(edf_external_t *external);
static int channel_rx_stats_dop_emit(edf_external_t *external);
static int channel_rx_stats_dop_drop(edf_external_t *external);

ERL_NIF_TERM
edf_channel_recv_trap_open(ErlNifEnv *env, edf_channel_resource_t *resource, edf_channel_t *channel,
                           edf_channel_recv_trap_t **trapp)
{
    edf_trap_state_t trap_state = {
        .resource = (void *)resource,
        .acquire = edf_channel_recv_trap_acquire,
        .release = edf_channel_recv_trap_release,
        .dtor = edf_channel_recv_trap_dtor,
        .edit = edf_channel_recv_trap_edit,
        .next = edf_channel_recv_trap_next,
        .arg = NULL,
    };
    edf_channel_recv_trap_t *trap = NULL;
    ERL_NIF_TERM trap_term;

    trap_term = edf_trap_open(env, &trap_state, sizeof(edf_channel_recv_trap_t), (edf_trap_t **)(&trap));
    if (trap == NULL) {
        return trap_term;
    }

    trap->super.state.arg = (void *)trap;
    trap->resource = resource;
    trap->channel = channel;
    (void)avec_init_free(&trap->actions);
    (void)avec_create_fixed(&trap->actions);
    trap->external = NULL;
    trap->fragment_index = 0;
    trap->packet_count = 0;

    if (trapp != NULL) {
        *trapp = trap;
    }

    return trap_term;
}

int
edf_channel_recv_trap_acquire(ErlNifEnv *caller_env, edf_trap_t *super, void *arg, ERL_NIF_TERM *error_term)
{
    edf_channel_recv_trap_t *trap = (void *)arg;
    edf_channel_resource_t *resource = NULL;
    edf_channel_t *channel = NULL;
    int flags = (EDF_CHANNEL_RESOURCE_FLAG_OWNER_REQUIRED | EDF_CHANNEL_RESOURCE_FLAG_WRITE_LOCK);

    (void)super;

    resource = trap->resource;

    // Silence warnings about this function not being used.
    (void)edf_channel_resource_acquire;

    if (!edf_channel_resource_acquire_direct(caller_env, resource, &channel, error_term, flags)) {
        return 0;
    }

    if (trap->channel != channel) {
        *error_term = EXCP_ERROR_F(caller_env, "Fatal error: trap->channel=%p does not match channel=%p\n", trap->channel, channel);
        (void)edf_channel_resource_release(&resource, &channel, flags);
        return 0;
    }

    return 1;
}

void
edf_channel_recv_trap_release(ErlNifEnv *caller_env, edf_trap_t *super, void *arg)
{
    edf_channel_recv_trap_t *trap = (void *)arg;
    edf_channel_resource_t *resource = NULL;
    edf_channel_t *channel = NULL;
    int flags = (EDF_CHANNEL_RESOURCE_FLAG_OWNER_REQUIRED | EDF_CHANNEL_RESOURCE_FLAG_WRITE_LOCK);

    (void)caller_env;
    (void)super;

    resource = trap->resource;
    channel = trap->channel;

    (void)edf_channel_resource_release(&resource, &channel, flags);

    return;
}

void
edf_channel_recv_trap_dtor(ErlNifEnv *caller_env, edf_trap_t *super, void *arg)
{
    edf_channel_recv_trap_t *trap = (void *)arg;

    XNIF_TRACE_F("%s:%d [edf_channel_recv_trap] dtor callback\n", __FILE__, __LINE__);

    (void)caller_env;
    (void)super;

    trap->resource = NULL;
    trap->channel = NULL;
    (void)avec_destroy(&trap->actions);
    if (trap->external != NULL) {
        (void)edf_external_destroy(trap->external);
        trap->external = NULL;
    }

    return;
}

void
edf_channel_recv_trap_edit(ErlNifEnv *caller_env, edf_trap_t *super, void *arg, edf_trap_result_t *result)
{
    edf_channel_recv_trap_t *trap = (void *)arg;
    edf_channel_resource_t *resource = trap->resource;
    edf_channel_t *channel = NULL;
    int flags = (EDF_CHANNEL_RESOURCE_FLAG_WRITE_LOCK);

    if (result->tag == EDF_TRAP_RESULT_TAG_ERR) {
        // Any exception raised should close the channel, even if the exception is caught by the Erlang process.
        XNIF_TRACE_F("%s:%d [trap] error callback\n", __FILE__, __LINE__);

        if (resource != NULL && edf_channel_resource_acquire_direct(caller_env, resource, &channel, NULL, flags)) {
            if (trap->external != NULL) {
                if (!edf_external_sequence_is_linked(trap->external)) {
                    (void)edf_external_destroy(trap->external);
                }
                trap->external = NULL;
            }
            (void)edf_channel_destroy(caller_env, resource, channel);
            trap->channel = NULL;
            (void)edf_channel_resource_release(&resource, &channel, flags);
        }
        if (trap->external != NULL) {
            trap->external->channel = NULL;
            (void)edf_external_destroy(trap->external);
            trap->external = NULL;
        }
    }

    return;
}

static int get_packet_length(edf_channel_recv_trap_t *trap, ioq_reader_t *ir, size_t *szp);
static edf_trap_result_t trap_actions(ErlNifEnv *caller_env, edf_channel_recv_trap_t *trap);
static void decode_control_length_callback(ErlNifEnv *caller_env, etf_decode_term_length_trap_t *child, void *arg,
                                           edf_trap_result_t *result);

edf_trap_result_t
edf_channel_recv_trap_next(ErlNifEnv *caller_env, edf_trap_t *super, void *arg)
{
    edf_channel_recv_trap_t *trap = (void *)arg;

#define TRAP_ACTIONS() trap_actions(caller_env, trap)

    do {
        edf_channel_rx_state_t curr_state = trap->channel->rx.state;

        switch (curr_state) {
        case EDF_CHANNEL_RX_STATE_PACKET_HEADER: {
            ioq_t *rx_ioq = &(trap->channel->rx.ioq);
            vec_t *rx_vec = &(trap->channel->rx.vec);
            ioq_reader_t rx_ioq_reader;
            ioq_reader_t *ir = &rx_ioq_reader;
            size_t packet_length = 0;

            if (!vec_is_free(rx_vec)) {
                return TRAP_ERR(EXCP_ERROR(caller_env, "Corrupted channel->rx.vec state: vec must be free\n"));
            }

            if (!ioq_reader_create(ir, rx_ioq)) {
                return TRAP_ERR(EXCP_ERROR(caller_env, "Call to ioq_reader_create() failed: unable to read rx_ioq\n"));
            }

            // If we don't have enough bytes to read the packet header, then yield.
            if (!get_packet_length(trap, ir, &packet_length)) {
                (void)ioq_reader_destroy(ir);
                return TRAP_ACTIONS();
            }
            TRAP_REDUCE(trap, packet_length);

            // This was a "tick" message where the packet is sized 0.
            // Consume the packet header and start waiting for the next packet.
            if (packet_length == 0) {
                if (!ioq_reader_consume(ir)) {
                    (void)ioq_reader_destroy(ir);
                    return TRAP_ERR(
                        EXCP_ERROR(caller_env, "Call to ioq_reader_consume() failed: 0-byte packet or 'tick message'\n"));
                }
                if (trap->channel->rx.packet_size == 0 && trap->packet_count > 0) {
                    // We've already seen at least 1 packet so far and the packet size is 0, so let's yield.
                    (void)ioq_reader_destroy(ir);
                    return TRAP_ACTIONS();
                }
                trap->packet_count += 1;
                CHANNEL_RX_STATS_COUNT(trap->channel, packet_count, 1);
                CHANNEL_RX_STATS_COUNT(trap->channel, dist_tick_count, 1);
                goto transition_to_packet_header;
            }

            // Consume the packet header, if it exists, so we don't accidentally emit it later.
            //
            // BIF calls for dist_ctrl_* cannot handle packet header.
            if (packet_length != 0 && !ioq_reader_consume(ir)) {
                (void)ioq_reader_destroy(ir);
                return TRAP_ERR(EXCP_ERROR_F(caller_env, "Call to ioq_reader_consume() failed: %u-byte packet\n", packet_length));
            }

            // If the entire packet is already available, create a "referenced" readable vec that points to the ioq.
            // Note: a "referenced" vec does not consume the ioq.
            //
            // Otherwise, create an "owned" writable vec of size packet_length and copy/consume all of the ioq.
            if (!vec_create_from_ioq_mem(rx_vec, packet_length, rx_ioq)) {
                (void)ioq_reader_destroy(ir);
                return TRAP_ERR(
                    EXCP_ERROR_F(caller_env, "Call to vec_create_from_ioq_mem() failed: %u-byte packet\n", packet_length));
            }

            goto transition_to_packet_data;
        }
        case EDF_CHANNEL_RX_STATE_PACKET_DATA: {
            ioq_t *rx_ioq = &(trap->channel->rx.ioq);
            vec_t *rx_vec = &(trap->channel->rx.vec);
            size_t rx_vec_before_write_len = 0;
            size_t rx_vec_after_write_len = 0;

            if (vec_is_free(rx_vec)) {
                return TRAP_ERR(EXCP_ERROR(caller_env, "Corrupted channel->rx.vec state: vec must not be free\n"));
            }

            // Copy/consume bytes from ioq by writing to vec until vec is filled with packet_length bytes.
            rx_vec_before_write_len = vec_len(rx_vec);
            if (vec_is_writable(rx_vec) && !vec_write_from_ioq(rx_vec, rx_ioq)) {
                return TRAP_ERR(
                    EXCP_ERROR_F(caller_env, "Call to vec_write_from_ioq() failed: %u-byte packet\n", vec_capacity(rx_vec)));
            }
            rx_vec_after_write_len = vec_len(rx_vec);
            TRAP_REDUCE(trap, rx_vec_after_write_len - rx_vec_before_write_len);

            // If vec is still writable, then we need to wait for more rx data.
            if (vec_is_writable(rx_vec)) {
                return TRAP_ACTIONS();
            }

            trap->packet_count += 1;
            CHANNEL_RX_STATS_COUNT(trap->channel, packet_count, 1);

            goto transition_to_dist_frame;
        }
        case EDF_CHANNEL_RX_STATE_EMIT_ATOM_CACHE_COMMIT: {
            static const uint8_t reg_send_noop[36] = {104, 4, 97,  6,   88,  118, 0,   0,   0,   0,   0,   0,
                                                      0,   0, 0,   0,   0,   0,   0,   0,   118, 0,   0,   118,
                                                      0,   9, 117, 110, 100, 101, 102, 105, 110, 101, 100, 106};
            vec_t emit_vec;
            vec_t *ev = &emit_vec;
            vec_writer_t emit_vec_writer;
            vec_writer_t *vw = &emit_vec_writer;
            size_t dist_header_size = 0;
            ERL_NIF_TERM err_term = THE_NON_VALUE;
            action_t action;

            if (!vec_is_slice(&trap->external->slices.headers.vec)) {
                return TRAP_ERR(EXCP_ERROR(caller_env, "Fatal error: trap->external->slices.headers.vec must be a slice\n"));
            }

            dist_header_size = vec_len(&trap->external->slices.headers.vec);

            (void)vec_init_free(ev);
            if (!vec_create_owned(ev, 1 + 1 + 8 + 8 + dist_header_size + sizeof(reg_send_noop))) {
                return TRAP_ERR(EXCP_ERROR(caller_env, "Call to vec_create_owned() failed: out of memory\n"));
            }
            if (!vec_writer_create(vw, ev, 0)) {
                (void)vec_destroy(ev);
                return TRAP_ERR(EXCP_ERROR(caller_env, "Call to vec_writer_create() failed\n"));
            }

#define TRAP_PREP_ERR(gen_err)                                                                                                     \
    do {                                                                                                                           \
        err_term = (gen_err);                                                                                                      \
        TRAP_REDUCE(trap, vec_writer_offset(vw));                                                                                  \
        (void)vec_writer_destroy(vw);                                                                                              \
        (void)vec_destroy(ev);                                                                                                     \
    } while (0)

            if (!vec_writer_write_u8(vw, VERSION_MAGIC)) {
                TRAP_PREP_ERR(EXCP_ERROR(caller_env, "Call to vec_writer_write_u8() failed: unable to write VERSION_MAGIC\n"));
                return TRAP_ERR(err_term);
            }
            TRAP_REDUCE(trap, 1);
            if (!vec_writer_write_u8(vw, DIST_FRAG_HEADER)) {
                TRAP_PREP_ERR(EXCP_ERROR(caller_env, "Call to vec_writer_write_u8() failed: unable to write DIST_FRAG_HEADER\n"));
                return TRAP_ERR(err_term);
            }
            TRAP_REDUCE(trap, 1);
            if (!vec_writer_write_u64(vw, trap->external->sequence_id)) {
                TRAP_PREP_ERR(EXCP_ERROR_F(caller_env, "Call to vec_writer_write_u64() failed: unable to write SequenceId=%u\n",
                                           trap->external->sequence_id));
                return TRAP_ERR(err_term);
            }
            TRAP_REDUCE(trap, 8);
            if (!vec_writer_write_u64(vw, 1)) {
                TRAP_PREP_ERR(EXCP_ERROR(caller_env, "Call to vec_writer_write_u64() failed: unable to write FragmentId=1\n"));
                return TRAP_ERR(err_term);
            }
            TRAP_REDUCE(trap, 8);
            if (!vec_writer_write_exact(vw, vec_buf(&trap->external->slices.headers.vec), dist_header_size)) {
                TRAP_PREP_ERR(EXCP_ERROR_F(
                    caller_env, "Call to vec_writer_write_exact() failed: unable to write Dist Header of size %u-bytes\n",
                    dist_header_size));
                return TRAP_ERR(err_term);
            }
            TRAP_REDUCE(trap, dist_header_size);
            if (!vec_writer_write_exact(vw, reg_send_noop, sizeof(reg_send_noop))) {
                TRAP_PREP_ERR(EXCP_ERROR_F(
                    caller_env,
                    "Call to vec_writer_write_exact() failed: unable to write Control Message and Message of size %u-bytes\n",
                    sizeof(reg_send_noop)));
                return TRAP_ERR(err_term);
            }
            TRAP_REDUCE(trap, sizeof(reg_send_noop));
            if (vec_is_writable(ev)) {
                TRAP_PREP_ERR(EXCP_ERROR(caller_env, "Fatal error: vec must not be writable\n"));
                return TRAP_ERR(err_term);
            }
            (void)vec_writer_destroy(vw);

            (void)action_init_free(&action);
            if (!action_create_emit(&action, ev)) {
                return TRAP_ERR(EXCP_ERROR(caller_env, "Call to action_create_emit() failed\n"));
            }

            if (!avec_push(&trap->actions, &action)) {
                (void)action_destroy(&action);
                return TRAP_ERR(EXCP_ERROR(caller_env, "Call to avec_push() failed: unable to push to trap->actions\n"));
            }

            CHANNEL_RX_STATS_COUNT(trap->channel, emit_count, 1);

            trap->external = NULL;
            goto transition_to_packet_header;

#undef TRAP_PREP_ERR
        }
        case EDF_CHANNEL_RX_STATE_REWRITE_FRAGMENT_HEADER: {
            edf_trap_result_t child_result;
            if (!edf_trap_has_child(&trap->super)) {
                ERL_NIF_TERM child_trap_term;
                etf_rewrite_fragment_header_trap_t *child_trap = NULL;
                child_trap_term = etf_rewrite_fragment_header_trap_open(caller_env, trap->external, &child_trap);
                if (child_trap == NULL || enif_is_exception(caller_env, child_trap_term)) {
                    return TRAP_ERR(child_trap_term);
                }
                if (!edf_trap_attach_child(&trap->super, &child_trap->super)) {
                    return TRAP_ERR(EXCP_ERROR(caller_env, "Call to edf_trap_attach_child() failed\n"));
                }
            }
            child_result = edf_trap_child_next(caller_env, &trap->super);
            switch (child_result.tag) {
            case EDF_TRAP_RESULT_TAG_OK:
                (void)edf_trap_detach_child(&trap->super);
                goto transition_to_external_recv;
            case EDF_TRAP_RESULT_TAG_ERR:
                (void)edf_trap_detach_child(&trap->super);
                return child_result;
            case EDF_TRAP_RESULT_TAG_YIELD:
                return child_result;
            default:
                (void)edf_trap_detach_child(&trap->super);
                return TRAP_ERR(EXCP_ERROR_F(caller_env,
                                             "Fatal error: corrupted child result while in state "
                                             "EDF_CHANNEL_RX_STATE_REWRITE_FRAGMENT_HEADER (result.tag was %d)\n",
                                             (int)(child_result.tag)));
            }
        }
        case EDF_CHANNEL_RX_STATE_EXTERNAL_RECV: {
            edf_trap_result_t child_result;
            if (!edf_trap_has_child(&trap->super)) {
                ERL_NIF_TERM child_trap_term;
                edf_external_recv_trap_t *child_trap = NULL;
                child_trap_term = edf_external_recv_trap_open(caller_env, trap->external, &child_trap);
                if (child_trap == NULL || enif_is_exception(caller_env, child_trap_term)) {
                    return TRAP_ERR(child_trap_term);
                }
                if (!edf_trap_attach_child(&trap->super, &child_trap->super)) {
                    return TRAP_ERR(EXCP_ERROR(caller_env, "Call to edf_trap_attach_child() failed\n"));
                }
            }
            child_result = edf_trap_child_next(caller_env, &trap->super);
            switch (child_result.tag) {
            case EDF_TRAP_RESULT_TAG_OK: {
                (void)edf_trap_detach_child(&trap->super);
                if (!trap->external->emit) {
                    goto transition_to_external_drop;
                }
                goto transition_to_external_emit;
            }
            case EDF_TRAP_RESULT_TAG_ERR:
                (void)edf_trap_detach_child(&trap->super);
                return child_result;
            case EDF_TRAP_RESULT_TAG_YIELD:
                return child_result;
            default:
                (void)edf_trap_detach_child(&trap->super);
                return TRAP_ERR(EXCP_ERROR_F(
                    caller_env,
                    "Fatal error: corrupted child result while in state EDF_CHANNEL_RX_STATE_DECODE_CONTROL (result.tag was %d)\n",
                    (int)(child_result.tag)));
            }
        }
        case EDF_CHANNEL_RX_STATE_EXTERNAL_EMIT: {
            edf_external_t *ext = trap->external;
            edf_fragment_t *frag = NULL;
            action_t action;

            if (ext == NULL) {
                return TRAP_ERR(EXCP_ERROR(caller_env, "Corrupted trap->external state: must not be NULL\n"));
            }

            while ((ext->fragments_compacted && trap->fragment_index == 0) ||
                   (!ext->fragments_compacted && trap->fragment_index < ext->fragment_capacity)) {
                frag = (ext->fragments_compacted) ? ext->compact : &ext->fragments[trap->fragment_index];
                (void)action_init_free(&action);
                if (!action_create_emit(&action, &frag->vec)) {
                    return TRAP_ERR(EXCP_ERROR(caller_env, "Call to action_create_emit() failed\n"));
                }

                if (!avec_push(&trap->actions, &action)) {
                    (void)action_destroy(&action);
                    return TRAP_ERR(EXCP_ERROR(caller_env, "Call to avec_push() failed: unable to push to trap->actions\n"));
                }

                CHANNEL_RX_STATS_COUNT(trap->channel, emit_count, 1);

                TRAP_REDUCE(trap, 1);
                trap->fragment_index += 1;

                if (TRAP_SHOULD_YIELD(trap)) {
                    return TRAP_YIELD();
                }
            }

            (void)channel_rx_stats_dop_emit(ext);

            if ((trap->external->flags & EDF_EXTERNAL_FLAG_ATOM_CACHE_NEED_ROLLBACK) != 0) {
                goto transition_to_rollback_atom_cache;
            }

            trap->external = NULL;
            trap->fragment_index = 0;
            XNIF_TRACE_F("%s:%d EDF_CHANNEL_RX_STATE_EMIT_EXTERNAL edf_external_destroy()\n", __FILE__, __LINE__);
            (void)edf_external_destroy(ext);

            goto transition_to_packet_header;
        }
        case EDF_CHANNEL_RX_STATE_EXTERNAL_DROP: {
            edf_external_t *ext = trap->external;
            edf_fragment_t *frag = NULL;
            action_t action;
            ERL_NIF_TERM err_term = THE_NON_VALUE;

            if (ext == NULL) {
                return TRAP_ERR(EXCP_ERROR(caller_env, "Corrupted trap->external state: must not be NULL\n"));
            }

            if (edf_external_is_pass_through(ext)) {
                goto external_drop_immediate;
            }

            if (trap->fragment_index == 0 && !etf_rewrite_as_drop(caller_env, ext, &err_term)) {
                return TRAP_ERR(err_term);
            }

            while ((ext->fragments_compacted && trap->fragment_index == 0) ||
                   (!ext->fragments_compacted && trap->fragment_index < ext->fragment_capacity)) {
                frag = (ext->fragments_compacted) ? ext->compact : &ext->fragments[trap->fragment_index];
                (void)action_init_free(&action);
                if (!action_create_emit(&action, &frag->vec)) {
                    return TRAP_ERR(EXCP_ERROR(caller_env, "Call to action_create_emit() failed\n"));
                }

                if (!avec_push(&trap->actions, &action)) {
                    (void)action_destroy(&action);
                    return TRAP_ERR(EXCP_ERROR(caller_env, "Call to avec_push() failed: unable to push to trap->actions\n"));
                }

                CHANNEL_RX_STATS_COUNT(trap->channel, drop_count, 1);

                TRAP_REDUCE(trap, 1);
                trap->fragment_index += 1;

                if (TRAP_SHOULD_YIELD(trap)) {
                    return TRAP_YIELD();
                }
            }

        external_drop_immediate:

            (void)channel_rx_stats_dop_drop(ext);

            if ((trap->external->flags & EDF_EXTERNAL_FLAG_ATOM_CACHE_NEED_ROLLBACK) != 0) {
                goto transition_to_rollback_atom_cache;
            }

            trap->external = NULL;
            trap->fragment_index = 0;
            XNIF_TRACE_F("%s:%d EDF_CHANNEL_RX_STATE_EMIT_EXTERNAL edf_external_destroy()\n", __FILE__, __LINE__);
            (void)edf_external_destroy(ext);

            goto transition_to_packet_header;
        }
        case EDF_CHANNEL_RX_STATE_ROLLBACK_ATOM_CACHE: {
            edf_external_t *ext = trap->external;
            edf_trap_result_t child_result;
            if (!edf_trap_has_child(&trap->super)) {
                ERL_NIF_TERM child_trap_term;
                etf_rollback_atom_cache_trap_t *child_trap = NULL;
                child_trap_term = etf_rollback_atom_cache_trap_open(caller_env, trap->external, &child_trap);
                if (child_trap == NULL || enif_is_exception(caller_env, child_trap_term)) {
                    return TRAP_ERR(child_trap_term);
                }
                if (!edf_trap_attach_child(&trap->super, &child_trap->super)) {
                    return TRAP_ERR(EXCP_ERROR(caller_env, "Call to edf_trap_attach_child() failed\n"));
                }
            }
            child_result = edf_trap_child_next(caller_env, &trap->super);
            switch (child_result.tag) {
            case EDF_TRAP_RESULT_TAG_OK:
                (void)edf_trap_detach_child(&trap->super);
                if (!vec_is_free(&ext->rollback_vec)) {
                    action_t action;
                    (void)action_init_free(&action);
                    if (!action_create_emit(&action, &ext->rollback_vec)) {
                        return TRAP_ERR(EXCP_ERROR(caller_env, "Call to action_create_emit() failed\n"));
                    }
                    if (!avec_push(&trap->actions, &action)) {
                        (void)action_destroy(&action);
                        return TRAP_ERR(EXCP_ERROR(caller_env, "Call to avec_push() failed: unable to push to trap->actions\n"));
                    }
                    CHANNEL_RX_STATS_COUNT(trap->channel, emit_count, 1);
                }
                trap->external = NULL;
                trap->fragment_index = 0;
                XNIF_TRACE_F("%s:%d EDF_CHANNEL_RX_STATE_ROLLBACK_ATOM_CACHE edf_external_destroy()\n", __FILE__, __LINE__);
                (void)edf_external_destroy(ext);
                goto transition_to_packet_header;
            case EDF_TRAP_RESULT_TAG_ERR:
                (void)edf_trap_detach_child(&trap->super);
                return child_result;
            case EDF_TRAP_RESULT_TAG_YIELD:
                return child_result;
            default:
                (void)edf_trap_detach_child(&trap->super);
                return TRAP_ERR(EXCP_ERROR_F(caller_env,
                                             "Fatal error: corrupted child result while in state "
                                             "EDF_CHANNEL_RX_STATE_ROLLBACK_ATOM_CACHE (result.tag was %d)\n",
                                             (int)(child_result.tag)));
            }
        }
        case EDF_CHANNEL_RX_STATE_DIST_FRAME: {
            vec_t *rx_vec = &(trap->channel->rx.vec);
            vec_reader_t rx_vec_reader;
            vec_reader_t *vr = &rx_vec_reader;
            ERL_NIF_TERM err_term = THE_NON_VALUE;

            if (vec_is_free(rx_vec)) {
                return TRAP_ERR(EXCP_ERROR(caller_env, "Corrupted channel->rx.vec state: Dist Frame vec must not be free\n"));
            }

            if (vec_is_writable(rx_vec)) {
                return TRAP_ERR(EXCP_ERROR(caller_env, "Corrupted channel->rx.vec state: Dist Frame vec must not be writable\n"));
            }

            if (!vec_reader_create(vr, rx_vec, 0)) {
                return TRAP_ERR(
                    EXCP_ERROR(caller_env, "Call to vec_reader_create() failed: unable to read rx_vec for Dist Frame\n"));
            }

#define TRAP_PREP_ERR(gen_err)                                                                                                     \
    do {                                                                                                                           \
        err_term = (gen_err);                                                                                                      \
        TRAP_REDUCE(trap, vec_reader_offset(vr));                                                                                  \
        (void)vec_reader_destroy(vr);                                                                                              \
    } while (0)

            if (vec_reader_remaining_capacity(vr) < 2) {
                TRAP_PREP_ERR(EXCP_ERROR_F(caller_env, "Dist Frame must be at least 2-bytes in size (was %u-bytes)\n",
                                           vec_reader_remaining_capacity(vr)));
                return TRAP_ERR(err_term);
            }

            if ((trap->channel->dflags & (DFLAG_DIST_HDR_ATOM_CACHE | DFLAG_FRAGMENTS)) == 0) {
                // If none of these distribution flags are set, then we fallback to "pass-through" mode.
                // We expect a PASS_THROUGH byte to be present.
                uint8_t type;
                if (!vec_reader_read_u8(vr, &type)) {
                    TRAP_PREP_ERR(EXCP_ERROR(
                        caller_env,
                        "Call to vec_reader_read_u8() failed: Dist Frame in pass-through mode must have an 8-bit type field\n"));
                    return TRAP_ERR(err_term);
                }
                if (type != PASS_THROUGH) {
                    TRAP_PREP_ERR(EXCP_ERROR_F(
                        caller_env, "Dist Frame in pass-through mode must have an 8-bit type field equal to %u (was %u)\n",
                        PASS_THROUGH, type));
                    return TRAP_ERR(err_term);
                }
                if (!edf_external_create(trap->channel, EDF_EXTERNAL_MODE_PASS_THROUGH, 0, 1, rx_vec, vec_reader_offset(vr),
                                         &trap->external)) {
                    TRAP_PREP_ERR(EXCP_ERROR(caller_env, "Call to edf_external_create() failed: Dist Frame in pass-through mode "
                                                         "failed to allocate external structure\n"));
                    return TRAP_ERR(err_term);
                }
                TRAP_REDUCE(trap, vec_reader_offset(vr));
                (void)vec_reader_destroy(vr);
                CHANNEL_RX_STATS_COUNT(trap->channel, dist_pass_through_count, 1);
                goto transition_to_decode_control_length;
            }

            {
                // Otherwise, we expect a VERSION_MAGIC byte to be present.
                uint8_t version_magic;
                if (!vec_reader_read_u8(vr, &version_magic)) {
                    TRAP_PREP_ERR(EXCP_ERROR(caller_env, "Call to vec_reader_read_u8() failed: Dist Frame in normal/fragment mode "
                                                         "must have an 8-bit version magic field\n"));
                    return TRAP_ERR(err_term);
                }
                if (version_magic != VERSION_MAGIC) {
                    TRAP_PREP_ERR(EXCP_ERROR_F(
                        caller_env,
                        "Dist Frame in normal/fragment mode must have an 8-bit version magic field equal to %u (was %u)\n",
                        VERSION_MAGIC, version_magic));
                    return TRAP_ERR(err_term);
                }
            };

            {
                // Dist Frame in normal/fragment mode must have a tag field which identifies the mode of operation.
                uint8_t tag;
                uint64_t sequence_id;
                uint64_t fragment_id;
                edf_external_t *ext = NULL;

                if (!vec_reader_read_u8(vr, &tag)) {
                    TRAP_PREP_ERR(EXCP_ERROR(
                        caller_env,
                        "Call to vec_reader_read_u8() failed: Dist Frame in normal/fragment mode must have an 8-bit tag field\n"));
                    return TRAP_ERR(err_term);
                }
                if (tag == DIST_HEADER) {
                    if ((trap->channel->dflags & (DFLAG_DIST_HDR_ATOM_CACHE)) == 0) {
                        TRAP_PREP_ERR(EXCP_ERROR(caller_env, "Dist Frame tagged with 'DIST_HEADER' not support by current "
                                                             "distribution flags (missing: DFLAG_DIST_HDR_ATOM_CACHE).\n"));
                        return TRAP_ERR(err_term);
                    }
                    if (!edf_external_create(trap->channel, EDF_EXTERNAL_MODE_NORMAL, 0, 1, rx_vec, vec_reader_offset(vr),
                                             &trap->external)) {
                        TRAP_PREP_ERR(EXCP_ERROR(caller_env, "Call to edf_external_create() failed: Dist Frame tagged with "
                                                             "'DIST_HEADER' failed to allocate external structure\n"));
                        return TRAP_ERR(err_term);
                    }
                    TRAP_REDUCE(trap, vec_reader_offset(vr));
                    (void)vec_reader_destroy(vr);
                    CHANNEL_RX_STATS_COUNT(trap->channel, dist_header_count, 1);
                    goto transition_to_dist_header;
                } else if (tag == DIST_FRAG_HEADER) {
                    if ((trap->channel->dflags & (DFLAG_FRAGMENTS)) == 0) {
                        TRAP_PREP_ERR(EXCP_ERROR(caller_env, "Dist Frame tagged with 'DIST_FRAG_HEADER' not support by current "
                                                             "distribution flags (missing: DFLAG_FRAGMENTS).\n"));
                        return TRAP_ERR(err_term);
                    }
                    if (vec_reader_remaining_capacity(vr) < 16) {
                        TRAP_PREP_ERR(EXCP_ERROR_F(
                            caller_env,
                            "Dist Frame tagged with 'DIST_FRAG_HEADER' must be at least 16-bytes in size (was %u-bytes).\n",
                            vec_reader_remaining_capacity(vr)));
                        return TRAP_ERR(err_term);
                    }
                    if (!vec_reader_read_u64(vr, &sequence_id)) {
                        TRAP_PREP_ERR(EXCP_ERROR(caller_env, "Call to vec_reader_read_u64() failed: Dist Frame tagged with "
                                                             "'DIST_FRAG_HEADER' must have a 64-bit SequenceId.\n"));
                        return TRAP_ERR(err_term);
                    }
                    if (!vec_reader_read_u64(vr, &fragment_id)) {
                        TRAP_PREP_ERR(EXCP_ERROR(caller_env, "Call to vec_reader_read_u64() failed: Dist Frame tagged with "
                                                             "'DIST_FRAG_HEADER' must have a 64-bit FragmentId.\n"));
                        return TRAP_ERR(err_term);
                    }
                    if (fragment_id < 1) {
                        TRAP_PREP_ERR(EXCP_ERROR_F(caller_env,
                                                   "Call to vec_reader_read_u64() failed: Dist Frame tagged with "
                                                   "'DIST_FRAG_HEADER' must have a 64-bit FragmentId not less than 1 (was %u).\n",
                                                   fragment_id));
                        return TRAP_ERR(err_term);
                    }
                    if (!edf_external_create(trap->channel, EDF_EXTERNAL_MODE_FRAGMENT, sequence_id, fragment_id, rx_vec,
                                             vec_reader_offset(vr), &trap->external)) {
                        TRAP_PREP_ERR(EXCP_ERROR(caller_env, "Call to edf_external_create() failed: Dist Frame tagged with "
                                                             "'DIST_FRAG_HEADER' failed to allocate external structure\n"));
                        return TRAP_ERR(err_term);
                    }
                    ext = edf_external_sequence_lookup_insert(&(trap->channel->rx.sequences), trap->external);
                    if (ext == NULL) {
                        // returns NULL when it has successfully inserted the external sequence
                        ext = trap->external;
                    }
                    if (ext != trap->external) {
                        TRAP_PREP_ERR(EXCP_ERROR_F(
                            caller_env,
                            "Call to edf_external_sequence_lookup_insert() failed: Dist Frame tagged with 'DIST_FRAG_HEADER' "
                            "found invalid reference to existing sequence for SequenceId=%u, FragmentId=%u\n",
                            sequence_id, fragment_id));
                        return TRAP_ERR(err_term);
                    }
                    TRAP_REDUCE(trap, vec_reader_offset(vr));
                    (void)vec_reader_destroy(vr);
                    CHANNEL_RX_STATS_COUNT(trap->channel, dist_frag_header_count, 1);
                    goto transition_to_dist_header;
                } else if (tag == DIST_FRAG_CONT) {
                    if ((trap->channel->dflags & (DFLAG_FRAGMENTS)) == 0) {
                        TRAP_PREP_ERR(EXCP_ERROR(caller_env, "Dist Frame tagged with 'DIST_FRAG_CONT' not support by current "
                                                             "distribution flags (missing: DFLAG_FRAGMENTS).\n"));
                        return TRAP_ERR(err_term);
                    }
                    if (vec_reader_remaining_capacity(vr) < 16) {
                        TRAP_PREP_ERR(EXCP_ERROR_F(
                            caller_env,
                            "Dist Frame tagged with 'DIST_FRAG_CONT' must be at least 16-bytes in size (was %u-bytes).\n",
                            vec_reader_remaining_capacity(vr)));
                        return TRAP_ERR(err_term);
                    }
                    if (!vec_reader_read_u64(vr, &sequence_id)) {
                        TRAP_PREP_ERR(EXCP_ERROR(caller_env, "Call to vec_reader_read_u64() failed: Dist Frame tagged with "
                                                             "'DIST_FRAG_CONT' must have a 64-bit SequenceId.\n"));
                        return TRAP_ERR(err_term);
                    }
                    if (!vec_reader_read_u64(vr, &fragment_id)) {
                        TRAP_PREP_ERR(EXCP_ERROR(caller_env, "Call to vec_reader_read_u64() failed: Dist Frame tagged with "
                                                             "'DIST_FRAG_CONT' must have a 64-bit FragmentId.\n"));
                        return TRAP_ERR(err_term);
                    }
                    if (fragment_id < 1) {
                        TRAP_PREP_ERR(EXCP_ERROR_F(caller_env,
                                                   "Call to vec_reader_read_u64() failed: Dist Frame tagged with 'DIST_FRAG_CONT' "
                                                   "must have a 64-bit FragmentId not less than 1 (was %u).\n",
                                                   fragment_id));
                        return TRAP_ERR(err_term);
                    }
                    ext = edf_external_sequence_lookup(trap->channel->rx.sequences, sequence_id);
                    if (ext == NULL) {
                        TRAP_PREP_ERR(
                            EXCP_ERROR_F(caller_env,
                                         "Call to edf_external_sequence_lookup() failed: Dist Frame tagged with 'DIST_FRAG_CONT' "
                                         "could not find existing sequence for SequenceId=%u, FragmentId=%u\n",
                                         sequence_id, fragment_id));
                        return TRAP_ERR(err_term);
                    }
                    if (ext->sequence_id != sequence_id) {
                        TRAP_PREP_ERR(
                            EXCP_ERROR_F(caller_env,
                                         "Call to edf_external_sequence_lookup() failed: Dist Frame tagged with 'DIST_FRAG_CONT' "
                                         "found corrupted state in sequence for SequenceId=%u, FragmentId=%u\n",
                                         sequence_id, fragment_id));
                        return TRAP_ERR(err_term);
                    }
                    trap->external = ext;
                    if (trap->external->mode != EDF_EXTERNAL_MODE_FRAGMENT) {
                        TRAP_PREP_ERR(EXCP_ERROR_F(caller_env,
                                                   "Dist Frame tagged with 'DIST_FRAG_CONT' found invalid mode sequence for "
                                                   "SequenceId=%u, FragmentId=%u, expected Mode=%d (was Mode=%d)\n",
                                                   sequence_id, fragment_id, EDF_EXTERNAL_MODE_FRAGMENT, trap->external->mode));
                        return TRAP_ERR(err_term);
                    }
                    if (trap->external->fragment_id_next != fragment_id) {
                        TRAP_PREP_ERR(EXCP_ERROR_F(caller_env,
                                                   "Dist Frame tagged with 'DIST_FRAG_CONT' received out-of-order fragment for "
                                                   "SequenceId=%u, expected FragmentId=%u (was FragmentId=%u)\n",
                                                   sequence_id, trap->external->fragment_id_next, fragment_id));
                        return TRAP_ERR(err_term);
                    }
                    if (!edf_external_add_fragment(trap->external, fragment_id, rx_vec, vec_reader_offset(vr))) {
                        TRAP_PREP_ERR(EXCP_ERROR(caller_env, "Call to edf_external_add_fragment() failed: Dist Frame tagged with "
                                                             "'DIST_FRAG_CONT' failed to allocate vec structure\n"));
                        return TRAP_ERR(err_term);
                    }
                    TRAP_REDUCE(trap, vec_reader_offset(vr));
                    (void)vec_reader_destroy(vr);
                    CHANNEL_RX_STATS_COUNT(trap->channel, dist_frag_cont_count, 1);
                    goto transition_to_fragment_continuation;
                } else {
                    TRAP_PREP_ERR(EXCP_ERROR_F(caller_env, "Dist Frame tagged with unknown Tag=%u\n", tag));
                    return TRAP_ERR(err_term);
                }
            };

#undef TRAP_PREP_ERR
        }
        case EDF_CHANNEL_RX_STATE_DIST_HEADER: {
            edf_trap_result_t child_result;
            if (!edf_trap_has_child(&trap->super)) {
                ERL_NIF_TERM child_trap_term;
                etf_decode_dist_header_trap_t *child_trap = NULL;
                child_trap_term = etf_decode_dist_header_trap_open(caller_env, trap->external, &child_trap);
                if (child_trap == NULL || enif_is_exception(caller_env, child_trap_term)) {
                    return TRAP_ERR(child_trap_term);
                }
                if (!edf_trap_attach_child(&trap->super, &child_trap->super)) {
                    return TRAP_ERR(EXCP_ERROR(caller_env, "Call to edf_trap_attach_child() failed\n"));
                }
            }
            child_result = edf_trap_child_next(caller_env, &trap->super);
            switch (child_result.tag) {
            case EDF_TRAP_RESULT_TAG_OK:
                (void)edf_trap_detach_child(&trap->super);
                if (trap->external->mode == EDF_EXTERNAL_MODE_NORMAL ||
                    (trap->external->mode == EDF_EXTERNAL_MODE_FRAGMENT &&
                     trap->external->fragment_id_next + 1 == trap->external->fragment_count)) {
                    goto transition_to_decode_control_length;
                }
                goto transition_to_fragment_continuation;
            case EDF_TRAP_RESULT_TAG_ERR:
                (void)edf_trap_detach_child(&trap->super);
                return child_result;
            case EDF_TRAP_RESULT_TAG_YIELD:
                return child_result;
            default:
                (void)edf_trap_detach_child(&trap->super);
                return TRAP_ERR(EXCP_ERROR_F(
                    caller_env,
                    "Fatal error: corrupted child result while in state EDF_CHANNEL_RX_STATE_DIST_HEADER (result.tag was %d)\n",
                    (int)(child_result.tag)));
            }
        }
        case EDF_CHANNEL_RX_STATE_FRAGMENT_CONTINUATION: {
            if (!edf_external_sequence_is_linked(trap->external)) {
                return TRAP_ERR(EXCP_ERROR(caller_env,
                                           "Fatal error: corrupted external state for fragment mode in state "
                                           "EDF_CHANNEL_RX_STATE_FRAGMENT_CONTINUATION (trap->external is not linked)\n"));
            }
            if (trap->external->fragment_id_next == 0) {
                if (trap->external->fragment_count > 1) {
                    goto transition_to_rewrite_fragment_header;
                }
                goto transition_to_external_recv;
            }
            if (trap->external->fragment_id_next + 1 == trap->external->fragment_count &&
                (trap->external->flags & EDF_EXTERNAL_FLAG_ATOM_CACHE_WRITE) != 0) {
                trap->external->flags |= EDF_EXTERNAL_FLAG_ATOM_CACHE_NEED_COMMIT;
                goto transition_to_emit_atom_cache_commit;
            }
            trap->external = NULL;
            goto transition_to_packet_header;
        }
        case EDF_CHANNEL_RX_STATE_DECODE_CONTROL_LENGTH: {
            edf_trap_result_t child_result;
            if (!edf_trap_has_child(&trap->super)) {
                ERL_NIF_TERM child_trap_term;
                etf_decode_term_length_trap_t *child_trap = NULL;
                bool is_external_term = false;
                vec_t control_slice;
                (void)vec_init_free(&control_slice);
                if (!edf_external_slice_control_get(trap->external, &is_external_term, &control_slice)) {
                    return TRAP_ERR(EXCP_ERROR(
                        caller_env, "Call to edf_external_slice_control_get() failed: unable to get slice for control message\n"));
                }
                child_trap_term = etf_decode_term_length_trap_open(caller_env, is_external_term, &control_slice,
                                                                   decode_control_length_callback, (void *)trap, &child_trap);
                (void)vec_destroy(&control_slice);
                if (child_trap == NULL || enif_is_exception(caller_env, child_trap_term)) {
                    return TRAP_ERR(child_trap_term);
                }
                if (!edf_trap_attach_child(&trap->super, &child_trap->super)) {
                    return TRAP_ERR(EXCP_ERROR(caller_env, "Call to edf_trap_attach_child() failed\n"));
                }
            }
            child_result = edf_trap_child_next(caller_env, &trap->super);
            switch (child_result.tag) {
            case EDF_TRAP_RESULT_TAG_OK:
                (void)edf_trap_detach_child(&trap->super);
                if (trap->external->mode == EDF_EXTERNAL_MODE_FRAGMENT) {
                    goto transition_to_fragment_continuation;
                }
                goto transition_to_external_recv;
            case EDF_TRAP_RESULT_TAG_ERR:
                (void)edf_trap_detach_child(&trap->super);
                return child_result;
            case EDF_TRAP_RESULT_TAG_YIELD:
                return child_result;
            default:
                (void)edf_trap_detach_child(&trap->super);
                return TRAP_ERR(EXCP_ERROR_F(caller_env,
                                             "Fatal error: corrupted child result while in state "
                                             "EDF_CHANNEL_RX_STATE_DECODE_CONTROL_LENGTH (result.tag was %d)\n",
                                             (int)(child_result.tag)));
            }
        }
        default:
            return TRAP_ERR(
                EXCP_ERROR_F(caller_env, "Fatal error: unknown channel->rx.state value %d\n", (int)(trap->channel->rx.state)));
        }

        goto escaped_without_local_jump;

    escaped_without_local_jump: {
        return TRAP_ERR(EXCP_ERROR(caller_env, "Fatal error: escaped switch statement without local jump\n"));
    }

    transition_to_packet_header: {
        vec_t *rx_vec = &(trap->channel->rx.vec);
        if (!vec_is_free(rx_vec)) {
            (void)vec_destroy(rx_vec);
        }
        trap->channel->rx.state = EDF_CHANNEL_RX_STATE_PACKET_HEADER;
        goto next_state;
    }

    transition_to_packet_data: {
        trap->channel->rx.state = EDF_CHANNEL_RX_STATE_PACKET_DATA;
        goto next_state;
    }

    transition_to_emit_atom_cache_commit: {
        trap->channel->rx.state = EDF_CHANNEL_RX_STATE_EMIT_ATOM_CACHE_COMMIT;
        goto next_state;
    }

    transition_to_rewrite_fragment_header: {
        trap->channel->rx.state = EDF_CHANNEL_RX_STATE_REWRITE_FRAGMENT_HEADER;
        goto next_state;
    }

    transition_to_rollback_atom_cache: {
        trap->channel->rx.state = EDF_CHANNEL_RX_STATE_ROLLBACK_ATOM_CACHE;
        goto next_state;
    }

    transition_to_external_recv: {
        trap->channel->rx.state = EDF_CHANNEL_RX_STATE_EXTERNAL_RECV;
        goto next_state;
    }

    transition_to_external_emit: {
        trap->channel->rx.state = EDF_CHANNEL_RX_STATE_EXTERNAL_EMIT;
        goto next_state;
    }

    transition_to_external_drop: {
        trap->channel->rx.state = EDF_CHANNEL_RX_STATE_EXTERNAL_DROP;
        goto next_state;
    }

    transition_to_dist_frame: {
        trap->channel->rx.state = EDF_CHANNEL_RX_STATE_DIST_FRAME;
        goto next_state;
    }

    transition_to_dist_header: {
        trap->channel->rx.state = EDF_CHANNEL_RX_STATE_DIST_HEADER;
        goto next_state;
    }

    transition_to_fragment_continuation: {
        trap->channel->rx.state = EDF_CHANNEL_RX_STATE_FRAGMENT_CONTINUATION;
        goto next_state;
    }

    transition_to_decode_control_length: {
        trap->channel->rx.state = EDF_CHANNEL_RX_STATE_DECODE_CONTROL_LENGTH;
        goto next_state;
    }

    next_state: {
        if (TRAP_SHOULD_YIELD(trap)) {
            return TRAP_YIELD();
        }
        continue;
    }
    } while (1);

#undef TRAP_ACTIONS
}

inline int
get_packet_length(edf_channel_recv_trap_t *trap, ioq_reader_t *ir, size_t *szp)
{
    union {
        uint8_t u8;
        uint16_t u16;
        uint32_t u32;
        uint64_t u64;
    } packet_length;

    if (trap->channel->rx.packet_size != 0) {
        if (ioq_reader_remaining_bytes(ir) < trap->channel->rx.packet_size) {
            *szp = 0;
            return 0;
        }
        if (trap->channel->rx.packet_size == 1) {
            if (!ioq_reader_read_u8(ir, &packet_length.u8)) {
                *szp = 0;
                return 0;
            }
            *szp = (size_t)packet_length.u8;
            return 1;
        } else if (trap->channel->rx.packet_size == 2) {
            if (!ioq_reader_read_u16(ir, &packet_length.u16)) {
                *szp = 0;
                return 0;
            }
            *szp = (size_t)packet_length.u16;
            return 1;
        } else if (trap->channel->rx.packet_size == 4) {
            if (!ioq_reader_read_u32(ir, &packet_length.u32)) {
                *szp = 0;
                return 0;
            }
            *szp = (size_t)packet_length.u32;
            return 1;
        } else if (trap->channel->rx.packet_size == 8) {
            if (!ioq_reader_read_u64(ir, &packet_length.u64)) {
                *szp = 0;
                return 0;
            }
            *szp = (size_t)packet_length.u64;
            return 1;
        } else {
            *szp = 0;
            return 0;
        }
    }

    *szp = ioq_reader_remaining_bytes(ir);
    return 1;
}

edf_trap_result_t
trap_actions(ErlNifEnv *caller_env, edf_channel_recv_trap_t *trap)
{
    ERL_NIF_TERM actions_term;

    if (!avec_into_list_term(caller_env, &trap->actions, &actions_term)) {
        return TRAP_ERR(EXCP_ERROR(caller_env, "Call to avec_into_list_term() failed: unable to create list from trap->actions\n"));
    }

    return TRAP_OK(actions_term);
}

void
decode_control_length_callback(ErlNifEnv *caller_env, etf_decode_term_length_trap_t *child, void *arg, edf_trap_result_t *result)
{
    edf_channel_recv_trap_t *parent = (void *)arg;
    edf_external_t *ext = parent->external;
    ERL_NIF_TERM err_term = THE_NON_VALUE;
    vec_t slice[1];

    (void)caller_env;
    (void)result;

    if (result->tag != EDF_TRAP_RESULT_TAG_OK) {
        return;
    }

    if ((child->flags & ETF_DECODE_TERM_LENGTH_FLAG_HAS_EXPORT_EXT) != 0) {
        CHANNEL_RX_STATS_COUNT(ext->channel, control_has_export_ext, 1);
    }
    if ((child->flags & ETF_DECODE_TERM_LENGTH_FLAG_HAS_NEW_FUN_EXT) != 0) {
        CHANNEL_RX_STATS_COUNT(ext->channel, control_has_new_fun_ext, 1);
    }

    (void)vec_init_free(slice);
    if (!vec_create_from_slice(slice, child->head, child->tail)) {
        *result = TRAP_ERR(EXCP_ERROR(caller_env, "Call to vec_create_from_slice() failed\n"));
        return;
    }
    if (ext->vtenv != NULL) {
        (void)vec_destroy(slice);
        *result = TRAP_ERR(EXCP_ERROR(caller_env, "Corrupted state: ext->vtenv must be NULL after decoding control length\n"));
        return;
    }
    ext->vtenv = vterm_env_alloc(&ext->attab);
    if (ext->vtenv == NULL) {
        (void)vec_destroy(slice);
        *result = TRAP_ERR(EXCP_ERROR(caller_env, "Call to vterm_env_alloc() failed: unable to decode control message\n"));
        return;
    }
    if (!etf_decode_udist_control(caller_env, ext->vtenv, edf_external_is_pass_through(ext), true, slice, ext->up, &err_term)) {
        (void)vec_destroy(slice);
        *result = TRAP_ERR(err_term);
        return;
    }
    if (!edf_external_slice_control_set(ext, child->head, child->tail)) {
        (void)vec_destroy(slice);
        *result = TRAP_ERR(EXCP_ERROR(caller_env, "Call to edf_external_slice_control_set() failed\n"));
        return;
    }

    (void)vec_destroy(slice);

    parent->external->control_heap_size = child->heap_size;

    (void)channel_rx_stats_dop_seen(parent->external);

    return;
}

inline int
channel_rx_stats_dop_seen(edf_external_t *external)
{
    edf_channel_stats_dop_t *statsdop = NULL;
    edf_world_slot_t *slot = NULL;
    if (!udist_get_channel_stats_dop(external->up, &external->channel->rx.stats, &statsdop) || statsdop == NULL) {
        return 0;
    }
    statsdop->seen += 1;
    slot = edf_world_get();
    if (!udist_get_channel_stats_dop(external->up, &slot->stats.channel.rx_stats, &statsdop) || statsdop == NULL) {
        return 0;
    }
    statsdop->seen += 1;
    return 1;
}

inline int
channel_rx_stats_dop_emit(edf_external_t *external)
{
    edf_channel_stats_dop_t *statsdop = NULL;
    edf_world_slot_t *slot = NULL;
    if (!udist_get_channel_stats_dop(external->up, &external->channel->rx.stats, &statsdop) || statsdop == NULL) {
        return 0;
    }
    statsdop->emit += 1;
    slot = edf_world_get();
    if (!udist_get_channel_stats_dop(external->up, &slot->stats.channel.rx_stats, &statsdop) || statsdop == NULL) {
        return 0;
    }
    statsdop->emit += 1;
    return 1;
}

inline int
channel_rx_stats_dop_drop(edf_external_t *external)
{
    edf_channel_stats_dop_t *statsdop = NULL;
    edf_world_slot_t *slot = NULL;
    if (!udist_get_channel_stats_dop(external->up, &external->channel->rx.stats, &statsdop) || statsdop == NULL) {
        return 0;
    }
    statsdop->drop += 1;
    slot = edf_world_get();
    if (!udist_get_channel_stats_dop(external->up, &slot->stats.channel.rx_stats, &statsdop) || statsdop == NULL) {
        return 0;
    }
    statsdop->drop += 1;
    return 1;
}
