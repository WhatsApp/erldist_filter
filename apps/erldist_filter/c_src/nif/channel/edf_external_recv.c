/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "edf_external_recv.h"
#include "../channel/edf_channel.h"
#include "../config/edf_config.h"
#include "../logger/edf_logger.h"
#include "../uterm/uterm.h"
#include "../vdist/vdist.h"

#include "../erts/dist.h"
#include "../erts/external.h"

static void edf_external_recv_trap_clear(ErlNifEnv *caller_env, edf_external_recv_trap_t *trap);
static void edf_external_recv_trap_dtor(ErlNifEnv *caller_env, edf_trap_t *super, void *arg);
static void edf_external_recv_trap_edit(ErlNifEnv *caller_env, edf_trap_t *super, void *arg, edf_trap_result_t *result);
static edf_trap_result_t edf_external_recv_trap_next(ErlNifEnv *caller_env, edf_trap_t *super, void *arg);

ERL_NIF_TERM
edf_external_recv_trap_open(ErlNifEnv *env, edf_external_t *external, edf_external_recv_trap_t **trapp)
{
    edf_trap_state_t trap_state = {
        .resource = NULL,
        .acquire = NULL,
        .release = NULL,
        .dtor = edf_external_recv_trap_dtor,
        .edit = edf_external_recv_trap_edit,
        .next = edf_external_recv_trap_next,
        .arg = NULL,
    };
    edf_external_recv_trap_t *trap = NULL;
    ERL_NIF_TERM trap_term;

    trap_term = edf_trap_open(env, &trap_state, sizeof(edf_external_recv_trap_t), (edf_trap_t **)(&trap));
    if (trap == NULL) {
        return trap_term;
    }

    trap->super.state.arg = (void *)trap;
    trap->state = EDF_EXTERNAL_RECV_TRAP_STATE_INIT;
    trap->external = external;
    trap->fragment_index = 0;
    trap->old_frag_size = 0;
    trap->new_frag_size.skip = 0;
    trap->new_frag_size.headers_length = 0;
    trap->new_frag_size.control_length = 0;
    trap->new_frag_size.payload_length = 0;
    trap->new_frag_size.total = 0;

    if (trapp != NULL) {
        *trapp = trap;
    }

    return trap_term;
}

void
edf_external_recv_trap_clear(ErlNifEnv *caller_env, edf_external_recv_trap_t *trap)
{
    XNIF_TRACE_F("%s:%d [edf_external_recv_trap] clear callback\n", __FILE__, __LINE__);

    (void)caller_env;

    trap->external = NULL;

    return;
}

void
edf_external_recv_trap_dtor(ErlNifEnv *caller_env, edf_trap_t *super, void *arg)
{
    edf_external_recv_trap_t *trap = (void *)arg;

    XNIF_TRACE_F("%s:%d [edf_external_recv_trap] dtor callback\n", __FILE__, __LINE__);

    (void)super;

    (void)edf_external_recv_trap_clear(caller_env, trap);

    return;
}

void
edf_external_recv_trap_edit(ErlNifEnv *caller_env, edf_trap_t *super, void *arg, edf_trap_result_t *result)
{
    (void)caller_env;
    (void)super;
    (void)arg;
    (void)result;

    return;
}

static void decode_payload_length_callback(ErlNifEnv *caller_env, etf_decode_term_length_trap_t *child, void *arg,
                                           edf_trap_result_t *result);

edf_trap_result_t
edf_external_recv_trap_next(ErlNifEnv *caller_env, edf_trap_t *super, void *arg)
{
    edf_external_recv_trap_t *trap = (void *)arg;
    edf_external_t *ext = trap->external;
    ERL_NIF_TERM err_term = THE_NON_VALUE;
    ERL_NIF_TERM *err_termp = &err_term;

    do {
        switch (trap->state) {
        case EDF_EXTERNAL_RECV_TRAP_STATE_NONE: {
            XNIF_TRACE_F("%s:%d EDF_EXTERNAL_RECV_TRAP_STATE_NONE\n", __FILE__, __LINE__);
            return TRAP_ERR(EXCP_ERROR(caller_env, "Corrupted trap state: must not be NONE\n"));
        }
        case EDF_EXTERNAL_RECV_TRAP_STATE_INIT: {
            XNIF_TRACE_F("%s:%d EDF_EXTERNAL_RECV_TRAP_STATE_INIT\n", __FILE__, __LINE__);
            if (ext != NULL && ext->fragment_capacity > 1 && !ext->fragments_compacted &&
                edf_config_is_compact_fragments_enabled()) {
                goto transition_to_compact_realloc;
            }
            goto transition_to_maybe_inspect;
        }
        case EDF_EXTERNAL_RECV_TRAP_STATE_COMPACT_REALLOC: {
            XNIF_TRACE_F("%s:%d EDF_EXTERNAL_RECV_TRAP_STATE_COMPACT_REALLOC\n", __FILE__, __LINE__);
            size_t new_frag_size;

            if (ext == NULL) {
                return TRAP_ERR(EXCP_ERROR(caller_env, "Corrupted trap->external state: must not be NULL\n"));
            }
            if (ext->fragment_capacity < 1) {
                return TRAP_ERR(EXCP_ERROR(
                    caller_env, "Corrupted trap->external state: fragment_capacity must be greater than or equal to 1\n"));
            }
            if (ext->fragment_capacity == 1) {
                goto transition_to_maybe_inspect;
            }

            trap->old_frag_size = vec_len(&ext->fragments[0].vec);
            trap->new_frag_size.skip = ext->fragments[0].skip;
            trap->new_frag_size.framing_length = ext->slices.framing.length;
            trap->new_frag_size.headers_length = ext->slices.headers.length;
            trap->new_frag_size.control_length = ext->slices.control.length;
            trap->new_frag_size.payload_length = ext->slices.expected_payload_length;
            trap->new_frag_size.total = ext->fragments[0].skip + ext->slices.headers.length + ext->slices.control.length +
                                        ext->slices.expected_payload_length;
            new_frag_size = ext->fragments[0].skip + ext->slices.headers.length + ext->slices.control.length +
                            ext->slices.expected_payload_length;

            if (!edf_external_compact_start(ext, new_frag_size)) {
                return TRAP_ERR(EXCP_ERROR_F(
                    caller_env, "Call to edf_external_compact_start() failed: old_frag_size=%llu, new_frag_size=%llu\n",
                    trap->old_frag_size, new_frag_size));
            }

            trap->fragment_index = 1;

            goto transition_to_compact_copy;
        }
        case EDF_EXTERNAL_RECV_TRAP_STATE_COMPACT_COPY: {
            XNIF_TRACE_F("%s:%d EDF_EXTERNAL_RECV_TRAP_STATE_COMPACT_COPY\n", __FILE__, __LINE__);
            edf_fragment_t *frag = NULL;
            edf_fragment_t *next_frag = NULL;

            if (ext == NULL) {
                return TRAP_ERR(EXCP_ERROR(caller_env, "Corrupted trap->external state: MUST NOT be NULL\n"));
            }

            frag = ext->primary;

            while (trap->fragment_index < ext->fragment_capacity) {
                if (!vec_is_writable(&frag->vec)) {
                    return TRAP_ERR(EXCP_ERROR_F(caller_env,
                                                 "Corrupted trap->external state: first fragment MUST be writable, owned=%d, "
                                                 "fragment_index=%u, fragment_capacity=%u\n",
                                                 vec_is_owned(&frag->vec), trap->fragment_index, ext->fragment_capacity));
                }
                next_frag = &ext->fragments[trap->fragment_index];
                if (!vec_write_from_vec(&frag->vec, &next_frag->vec, next_frag->skip)) {
                    return TRAP_ERR(EXCP_ERROR(caller_env, "Call to vec_write_from_vec() failed\n"));
                }
                next_frag->skip = 0;
                CHANNEL_RX_STATS_COUNT(ext->channel, compact_fragment_count, 1);
                TRAP_REDUCE(trap, 1);
                trap->fragment_index += 1;
                if (TRAP_SHOULD_YIELD(trap)) {
                    return TRAP_YIELD();
                }
            }

            if (vec_is_writable(&frag->vec) || vec_len(&frag->vec) != (ext->slices.framing.length + ext->slices.headers.length +
                                                                       ext->slices.control.length + ext->slices.payload.length)) {
                *err_termp = EXCP_ERROR_F(caller_env,
                                          "Corrupted trap->external state: first fragment MUST NOT be writable "
                                          "(old_frag_size=%llu, new_frag_size=%llu, remaining_bytes=%llu).\n",
                                          trap->old_frag_size, trap->new_frag_size.total, vec_remaining_writable_bytes(&frag->vec));
                return TRAP_ERR(*err_termp);
            }

            // Rewrite the fragment_id to be 1.
            if (!edf_external_set_fragment_id(ext, frag, 1)) {
                return TRAP_ERR(EXCP_ERROR(caller_env, "Call to edf_external_set_fragment_id() failed\n"));
            }

            ext->fragments_compacted = true;
            CHANNEL_RX_STATS_COUNT(ext->channel, compact_external_count, 1);

            trap->fragment_index = 0;

            goto transition_to_maybe_inspect;
        }
        case EDF_EXTERNAL_RECV_TRAP_STATE_MAYBE_INSPECT: {
            XNIF_TRACE_F("%s:%d EDF_EXTERNAL_RECV_TRAP_STATE_MAYBE_INSPECT\n", __FILE__, __LINE__);
            if (ext != NULL && (ext->fragment_capacity == 1 || ext->fragments_compacted) &&
                edf_config_is_deep_packet_inspection_enabled()) {
                if (edf_external_has_payload(ext)) {
                    goto transition_to_decode_payload_length;
                }
                goto transition_to_inspect;
            }
            goto transition_to_emit;
        }
        case EDF_EXTERNAL_RECV_TRAP_STATE_DECODE_PAYLOAD_LENGTH: {
            XNIF_TRACE_F("%s:%d EDF_EXTERNAL_RECV_TRAP_STATE_DECODE_PAYLOAD_LENGTH\n", __FILE__, __LINE__);
            edf_trap_result_t child_result;
            if (!edf_trap_has_child(&trap->super)) {
                ERL_NIF_TERM child_trap_term;
                etf_decode_term_length_trap_t *child_trap = NULL;
                bool is_external_term = false;
                vec_t payload_vec[1];
                (void)vec_init_free(payload_vec);
                if (!edf_external_slice_payload_get(trap->external, &is_external_term, payload_vec)) {
                    return TRAP_ERR(EXCP_ERROR(
                        caller_env, "Call to edf_external_slice_payload_get() failed: unable to get slice for payload message\n"));
                }
                child_trap_term = etf_decode_term_length_trap_open(caller_env, is_external_term, payload_vec,
                                                                   decode_payload_length_callback, (void *)trap, &child_trap);
                (void)vec_destroy(payload_vec);
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
                goto transition_to_inspect;
            case EDF_TRAP_RESULT_TAG_ERR:
                (void)edf_trap_detach_child(&trap->super);
                return child_result;
            case EDF_TRAP_RESULT_TAG_YIELD:
                return child_result;
            default:
                (void)edf_trap_detach_child(&trap->super);
                return TRAP_ERR(EXCP_ERROR_F(caller_env,
                                             "Fatal error: corrupted child result while in state "
                                             "EDF_EXTERNAL_RECV_TRAP_STATE_DECODE_PAYLOAD_LENGTH (result.tag was %d)\n",
                                             (int)(child_result.tag)));
            }
        }
        case EDF_EXTERNAL_RECV_TRAP_STATE_INSPECT: {
            XNIF_TRACE_F("%s:%d EDF_EXTERNAL_RECV_TRAP_STATE_INSPECT\n", __FILE__, __LINE__);
            vec_t payload_vec[1];
            slice_t payload_slice[1];
            slice_t *payload = NULL;
            bool untrusted = edf_config_is_untrusted_enabled();
            bool is_external_term = false;
            if (ext->up->info.payload) {
                (void)vec_init_free(payload_vec);
                if (!edf_external_slice_payload_get(ext, &is_external_term, payload_vec)) {
                    return TRAP_ERR(EXCP_ERROR(
                        caller_env, "Call to edf_external_slice_payload_get() failed: unable to get slice for payload message\n"));
                }
                payload_slice->head = vec_buf(payload_vec);
                payload_slice->tail = vec_buf_tail(payload_vec);
                payload = payload_slice;
            }
            if (!udist_classify(caller_env, ext->vtenv, ext->up, untrusted, is_external_term, payload, err_termp)) {
                (void)vec_destroy(payload_vec);
                return TRAP_ERR(*err_termp);
            }
            (void)vec_destroy(payload_vec);
            if ((ext->up->flags & UDIST_CLASSIFY_FLAG_LOG_EVENT) != 0) {
                goto transition_to_maybe_log_event;
            } else if ((ext->up->flags & (UDIST_CLASSIFY_FLAG_REDIRECT_DOP | UDIST_CLASSIFY_FLAG_REDIRECT_SPAWN_REQUEST)) != 0) {
                goto transition_to_maybe_redirect;
            } else if ((ext->up->flags & UDIST_CLASSIFY_FLAG_DROP) != 0) {
                goto transition_to_drop;
            }
            goto transition_to_emit;
        }
        case EDF_EXTERNAL_RECV_TRAP_STATE_MAYBE_LOG_EVENT: {
            XNIF_TRACE_F("%s:%d EDF_EXTERNAL_RECV_TRAP_STATE_MAYBE_LOG_EVENT\n", __FILE__, __LINE__);
            if (edf_config_is_logging_enabled() && (ext->up->flags & UDIST_CLASSIFY_FLAG_LOG_EVENT) != 0) {
                goto transition_to_log_event;
            }
            goto transition_to_maybe_redirect;
        }
        case EDF_EXTERNAL_RECV_TRAP_STATE_LOG_EVENT: {
            XNIF_TRACE_F("%s:%d EDF_EXTERNAL_RECV_TRAP_STATE_LOG_EVENT\n", __FILE__, __LINE__);
            if (!edf_logger_event_create(ext->channel->sysname, ext->attab.size, edf_external_is_pass_through(ext),
                                         ext->slices.control.length, ext->slices.payload.length, &ext->logger_event)) {
                return TRAP_ERR(EXCP_ERROR(caller_env, "Call to edf_logger_event_create() failed\n"));
            }
            if (ext->attab.size > 0 &&
                !edf_atom_translation_table_fill_array(&ext->attab, ext->logger_event->atoms, ext->logger_event->number_of_atoms)) {
                return TRAP_ERR(EXCP_ERROR(caller_env, "Call to edf_atom_translation_table_fill_array() failed\n"));
            }
            TRAP_REDUCE(trap, ext->attab.size);
            if (!vec_writer_write_exact(&ext->logger_event->control.writer, vec_buf(&ext->slices.control.vec),
                                        vec_len(&ext->slices.control.vec))) {
                return TRAP_ERR(EXCP_ERROR(caller_env, "Unable to write control to logger event\n"));
            }
            TRAP_REDUCE(trap, ext->slices.control.length);
            if (vec_is_writable(&ext->logger_event->control.vec)) {
                return TRAP_ERR(EXCP_ERROR(caller_env, "Corrupted ext->logger_event state: control.vec must not be writable\n"));
            }
            if (edf_external_has_payload(ext)) {
                if (!vec_writer_write_exact(&ext->logger_event->payload.writer, vec_buf(&ext->slices.payload.vec),
                                            vec_len(&ext->slices.payload.vec))) {
                    return TRAP_ERR(EXCP_ERROR(caller_env, "Unable to write payload to logger event\n"));
                }
                TRAP_REDUCE(trap, ext->slices.payload.length);
                if (vec_is_writable(&ext->logger_event->payload.vec)) {
                    return TRAP_ERR(
                        EXCP_ERROR(caller_env, "Corrupted ext->logger_event state: payload.vec must not be writable\n"));
                }
            }
            if (!ipc_queue_send(&edf_logger_queue_global->super, &ext->logger_event->super)) {
                return TRAP_ERR(EXCP_ERROR(caller_env, "Corrupted ext->logger_event state: call to ipc_queue_send() failed\n"));
            }
            ext->logger_event = NULL;
            goto transition_to_maybe_redirect;
        }
        case EDF_EXTERNAL_RECV_TRAP_STATE_MAYBE_REDIRECT: {
            XNIF_TRACE_F("%s:%d EDF_EXTERNAL_RECV_TRAP_STATE_MAYBE_REDIRECT\n", __FILE__, __LINE__);
            if ((ext->up->flags & UDIST_CLASSIFY_FLAG_REDIRECT_DOP) != 0 && edf_config_is_redirect_dist_operations_enabled()) {
                goto transition_to_redirect_dop;
            } else if ((ext->up->flags & UDIST_CLASSIFY_FLAG_REDIRECT_SPAWN_REQUEST) != 0) {
                goto transition_to_redirect_spawn_request;
            } else if ((ext->up->flags & UDIST_CLASSIFY_FLAG_DROP) != 0) {
                goto transition_to_drop;
            }
            goto transition_to_emit;
        }
        case EDF_EXTERNAL_RECV_TRAP_STATE_REDIRECT_DOP: {
            XNIF_TRACE_F("%s:%d EDF_EXTERNAL_RECV_TRAP_STATE_REDIRECT_DOP\n", __FILE__, __LINE__);
            if (!etf_redirect_dop(caller_env, ext, err_termp)) {
                return TRAP_ERR(*err_termp);
            }
            goto transition_to_emit;
        }
        case EDF_EXTERNAL_RECV_TRAP_STATE_REDIRECT_SPAWN_REQUEST: {
            XNIF_TRACE_F("%s:%d EDF_EXTERNAL_RECV_TRAP_STATE_REDIRECT_SPAWN_REQUEST\n", __FILE__, __LINE__);
            if (!etf_redirect_spawn_request(caller_env, ext, err_termp)) {
                return TRAP_ERR(*err_termp);
            }
            goto transition_to_emit;
        }
        case EDF_EXTERNAL_RECV_TRAP_STATE_EMIT: {
            XNIF_TRACE_F("%s:%d EDF_EXTERNAL_RECV_TRAP_STATE_EMIT\n", __FILE__, __LINE__);
            ext->emit = 1;
            goto transition_to_done;
        }
        case EDF_EXTERNAL_RECV_TRAP_STATE_DROP: {
            XNIF_TRACE_F("%s:%d EDF_EXTERNAL_RECV_TRAP_STATE_DROP\n", __FILE__, __LINE__);
            ext->emit = 0;
            goto transition_to_done;
        }
        case EDF_EXTERNAL_RECV_TRAP_STATE_DONE: {
            XNIF_TRACE_F("%s:%d EDF_EXTERNAL_RECV_TRAP_STATE_DONE\n", __FILE__, __LINE__);
            (void)edf_external_recv_trap_clear(caller_env, trap);
            return TRAP_OK(THE_NON_VALUE);
        }
        default: {
            return TRAP_ERR(
                EXCP_ERROR_F(caller_env, "Fatal error: unknown edf_external_recv_trap_t->state value %d\n", (int)(trap->state)));
        }
        }

        goto escaped_without_local_jump;

    escaped_without_local_jump: {
        return TRAP_ERR(EXCP_ERROR(caller_env, "Fatal error: escaped switch statement without local jump\n"));
    }

    transition_to_compact_realloc: {
        trap->state = EDF_EXTERNAL_RECV_TRAP_STATE_COMPACT_REALLOC;
        goto next_state;
    }
    transition_to_compact_copy: {
        trap->state = EDF_EXTERNAL_RECV_TRAP_STATE_COMPACT_COPY;
        goto next_state;
    }
    transition_to_maybe_inspect: {
        trap->state = EDF_EXTERNAL_RECV_TRAP_STATE_MAYBE_INSPECT;
        goto next_state;
    }
    transition_to_decode_payload_length: {
        trap->state = EDF_EXTERNAL_RECV_TRAP_STATE_DECODE_PAYLOAD_LENGTH;
        goto next_state;
    }
    transition_to_inspect: {
        trap->state = EDF_EXTERNAL_RECV_TRAP_STATE_INSPECT;
        goto next_state;
    }
    transition_to_maybe_log_event: {
        trap->state = EDF_EXTERNAL_RECV_TRAP_STATE_MAYBE_LOG_EVENT;
        goto next_state;
    }
    transition_to_log_event: {
        trap->state = EDF_EXTERNAL_RECV_TRAP_STATE_LOG_EVENT;
        goto next_state;
    }
    transition_to_maybe_redirect: {
        trap->state = EDF_EXTERNAL_RECV_TRAP_STATE_MAYBE_REDIRECT;
        goto next_state;
    }
    transition_to_redirect_dop: {
        trap->state = EDF_EXTERNAL_RECV_TRAP_STATE_REDIRECT_DOP;
        goto next_state;
    }
    transition_to_redirect_spawn_request: {
        trap->state = EDF_EXTERNAL_RECV_TRAP_STATE_REDIRECT_SPAWN_REQUEST;
        goto next_state;
    }
    transition_to_emit: {
        trap->state = EDF_EXTERNAL_RECV_TRAP_STATE_EMIT;
        goto next_state;
    }
    transition_to_drop: {
        trap->state = EDF_EXTERNAL_RECV_TRAP_STATE_DROP;
        goto next_state;
    }
    transition_to_done: {
        trap->state = EDF_EXTERNAL_RECV_TRAP_STATE_DONE;
        goto next_state;
    }
    next_state: {
        if (TRAP_SHOULD_YIELD(trap)) {
            return TRAP_YIELD();
        }
        continue;
    }
    } while (1);
}

void
decode_payload_length_callback(ErlNifEnv *caller_env, etf_decode_term_length_trap_t *child, void *arg, edf_trap_result_t *result)
{
    edf_external_recv_trap_t *parent = (void *)arg;
    edf_external_t *ext = parent->external;

    (void)caller_env;
    (void)result;

    if (result->tag != EDF_TRAP_RESULT_TAG_OK) {
        return;
    }

    if ((child->flags & ETF_DECODE_TERM_LENGTH_FLAG_HAS_EXPORT_EXT) != 0) {
        CHANNEL_RX_STATS_COUNT(ext->channel, payload_has_export_ext, 1);
    }
    if ((child->flags & ETF_DECODE_TERM_LENGTH_FLAG_HAS_NEW_FUN_EXT) != 0) {
        CHANNEL_RX_STATS_COUNT(ext->channel, payload_has_new_fun_ext, 1);
    }

    if (!edf_external_slice_payload_set(ext, child->head, child->tail)) {
        *result = TRAP_ERR(EXCP_ERROR(caller_env, "Call to edf_external_slice_payload_set() failed\n"));
        return;
    }

    ext->payload_heap_size = child->heap_size;

    return;
}
