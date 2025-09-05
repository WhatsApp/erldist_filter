/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "etf_rewrite_fragment_header.h"

#include "../erts/atom.h"
#include "../erts/dist.h"
#include "../erts/external.h"

#include "../channel/edf_channel.h"

static void etf_rewrite_fragment_header_trap_dtor(ErlNifEnv *caller_env, edf_trap_t *super, void *arg);
static edf_trap_result_t etf_rewrite_fragment_header_trap_next(ErlNifEnv *caller_env, edf_trap_t *super, void *arg);

ERL_NIF_TERM
etf_rewrite_fragment_header_trap_open(ErlNifEnv *env, edf_external_t *external, etf_rewrite_fragment_header_trap_t **trapp)
{
    edf_trap_state_t trap_state = {
        .resource = NULL,
        .acquire = NULL,
        .release = NULL,
        .dtor = etf_rewrite_fragment_header_trap_dtor,
        .edit = NULL,
        .next = etf_rewrite_fragment_header_trap_next,
        .arg = NULL,
    };
    etf_rewrite_fragment_header_trap_t *trap = NULL;
    ERL_NIF_TERM trap_term;

    trap_term = edf_trap_open(env, &trap_state, sizeof(etf_rewrite_fragment_header_trap_t), (edf_trap_t **)(&trap));
    if (trap == NULL) {
        return trap_term;
    }

    trap->super.state.arg = (void *)trap;
    trap->state = ETF_REWRITE_FRAGMENT_HEADER_TRAP_STATE_INIT;
    trap->external = external;
    trap->internal_index = 0;
    trap->maybe_rewrite = false;
    trap->rewrite_size = 0;

    if (trapp != NULL) {
        *trapp = trap;
    }

    return trap_term;
}

void
etf_rewrite_fragment_header_trap_dtor(ErlNifEnv *caller_env, edf_trap_t *super, void *arg)
{
    etf_rewrite_fragment_header_trap_t *trap = (void *)arg;

    XNIF_TRACE_F("%s:%d [trap] dtor callback\n", __FILE__, __LINE__);

    (void)caller_env;
    (void)super;

    trap->external = NULL;

    return;
}

edf_trap_result_t
etf_rewrite_fragment_header_trap_next(ErlNifEnv *caller_env, edf_trap_t *super, void *arg)
{
    etf_rewrite_fragment_header_trap_t *trap = (void *)arg;
    uint64_t dflags = trap->external->channel->dflags;
    ErlNifCharEncoding atom_encoding = ((!(dflags & DFLAG_UTF8_ATOMS)) ? ERL_NIF_LATIN1 : ERL_NIF_UTF8);
    edf_atom_cache_t *cache = trap->external->channel->rx.cache;
    edf_atom_translation_table_t *attab = &(trap->external->attab);
    edf_atom_translation_table_t *rollback = &(trap->external->rollback);
    ERL_NIF_TERM err_term = THE_NON_VALUE;

    do {
        switch (trap->state) {
        case ETF_REWRITE_FRAGMENT_HEADER_TRAP_STATE_INIT: {
            if (attab == NULL || attab->size == 0) {
                trap->state = ETF_REWRITE_FRAGMENT_HEADER_TRAP_STATE_DONE;
                goto next_state;
            }
            trap->internal_index = 0;
            trap->maybe_rewrite = false;
            trap->rewrite_size = 0;
            trap->state = ETF_REWRITE_FRAGMENT_HEADER_TRAP_STATE_FIND_CONFLICTS;
            goto next_state;
        }
        case ETF_REWRITE_FRAGMENT_HEADER_TRAP_STATE_FIND_CONFLICTS: {
            while (trap->internal_index < (int)(attab->size)) {
                edf_atom_translation_table_entry_t *entry = &(attab->entries[trap->internal_index]);
                edf_atom_translation_table_entry_t *conflict = &(rollback->entries[rollback->size]);
                if (cache->entries[entry->cache_index] == THE_NON_VALUE) {
                    err_term = EXCP_ERROR_F(caller_env,
                                            "Corrupted atom cache at index %d while attempting to detect conflicts: atom cache "
                                            "entry is THE_NON_VALUE, atom translation table entry is %T\n",
                                            entry->cache_index, entry->atom);
                    return TRAP_ERR(err_term);
                }
                if (entry->atom == cache->entries[entry->cache_index]) {
                    if (entry->new_entry == true) {
                        trap->maybe_rewrite = true;
                        entry->new_entry = false;
                    }
                    entry->conflict = NULL;
                } else {
                    if (entry->new_entry == false) {
                        trap->external->flags |= EDF_EXTERNAL_FLAG_ATOM_CACHE_NEED_REWRITE;
                        entry->new_entry = true;
                    }
                    trap->external->flags |= EDF_EXTERNAL_FLAG_ATOM_CACHE_NEED_ROLLBACK;
                    conflict->atom = cache->entries[entry->cache_index];
                    conflict->cache_index = entry->cache_index;
                    conflict->new_entry = true;
                    conflict->conflict = entry;
                    entry->conflict = conflict;
                    rollback->size += 1;
                }
                trap->internal_index += 1;
                TRAP_REDUCE(trap, 1);
                if (TRAP_SHOULD_YIELD(trap)) {
                    return TRAP_YIELD();
                }
            }
            if (trap->maybe_rewrite == true && (trap->external->flags & EDF_EXTERNAL_FLAG_ATOM_CACHE_NEED_ROLLBACK) != 0) {
                trap->external->flags |= EDF_EXTERNAL_FLAG_ATOM_CACHE_NEED_REWRITE;
            }
            if ((trap->external->flags & EDF_EXTERNAL_FLAG_ATOM_CACHE_NEED_REWRITE) != 0) {
                trap->internal_index = 0;
                trap->rewrite_size = 0;
                trap->state = ETF_REWRITE_FRAGMENT_HEADER_TRAP_STATE_RESOLVE_ATOMS;
                goto next_state;
            }
            trap->state = ETF_REWRITE_FRAGMENT_HEADER_TRAP_STATE_DONE;
            goto next_state;
        }
        case ETF_REWRITE_FRAGMENT_HEADER_TRAP_STATE_RESOLVE_ATOMS: {
            while (trap->internal_index < (int)(attab->size)) {
                edf_atom_translation_table_entry_t *entry = &(attab->entries[trap->internal_index]);
                size_t atom_text_len = 0;
                if (entry->new_entry == true) {
                    if (!edf_atom_text_get_length(entry->atom, atom_encoding, &atom_text_len)) {
                        err_term = EXCP_ERROR_F(caller_env,
                                                "Call to edf_atom_text_get_length() failed: unable to resolve length for atom %T\n",
                                                entry->atom);
                        return TRAP_ERR(err_term);
                    }
                    if (atom_text_len > 255) {
                        attab->long_atoms = true;
                    }
                }
                if (entry->conflict != NULL) {
                    atom_text_len = 0;
                    if (!edf_atom_text_get_length(entry->conflict->atom, atom_encoding, &atom_text_len)) {
                        err_term = EXCP_ERROR_F(
                            caller_env,
                            "Call to edf_atom_text_get_length() failed: unable to resolve length for conflict atom %T\n",
                            entry->conflict->atom);
                        return TRAP_ERR(err_term);
                    }
                    if (atom_text_len > 255) {
                        rollback->long_atoms = true;
                    }
                }
                trap->internal_index += 1;
                TRAP_REDUCE(trap, 1);
                if (TRAP_SHOULD_YIELD(trap)) {
                    return TRAP_YIELD();
                }
            }
            trap->internal_index = 0;
            trap->state = ETF_REWRITE_FRAGMENT_HEADER_TRAP_STATE_ALLOC;
            goto next_state;
        }
        case ETF_REWRITE_FRAGMENT_HEADER_TRAP_STATE_ALLOC: {
            edf_external_t *ext = trap->external;
            size_t old_capacity;
            slice_t old_framing[1] = {SLICE_INIT_EMPTY()};
            slice_t old_headers[1] = {SLICE_INIT_EMPTY()};
            slice_t old_control[1] = {SLICE_INIT_EMPTY()};
            slice_t old_payload[1] = {SLICE_INIT_EMPTY()};
            size_t new_capacity;
            slice_t new_framing[1] = {SLICE_INIT_EMPTY()};
            slice_t new_headers[1] = {SLICE_INIT_EMPTY()};
            slice_t new_control[1] = {SLICE_INIT_EMPTY()};
            slice_t new_payload[1] = {SLICE_INIT_EMPTY()};
            vec_t vec[1];
            vec_writer_t vw[1];

            while (trap->internal_index < (int)(attab->size)) {
                edf_atom_translation_table_entry_t *entry = &(attab->entries[trap->internal_index]);
                size_t atom_text_len = 0;
                if (entry->new_entry == true) {
                    if (!edf_atom_text_get_length(entry->atom, atom_encoding, &atom_text_len)) {
                        err_term = EXCP_ERROR_F(caller_env,
                                                "Call to edf_atom_text_get_length() failed: unable to resolve length for atom %T\n",
                                                entry->atom);
                        return TRAP_ERR(err_term);
                    }
                    XNIF_TRACE_F("[REWRITE] ATOM TEXT LEN = %d\n", atom_text_len);
                    trap->rewrite_size += 1 + ((attab->long_atoms == true) ? 2 : 1) + atom_text_len;
                } else {
                    trap->rewrite_size += 1;
                }
                trap->internal_index += 1;
                TRAP_REDUCE(trap, 1);
            }

            XNIF_TRACE_F("[REWRITE] %d BYTES ARE = %d\n", attab->size, ERTS_DIST_HDR_ATOM_CACHE_FLAG_BYTES(attab->size));
            trap->rewrite_size += ERTS_DIST_HDR_ATOM_CACHE_FLAG_BYTES(attab->size);
            trap->rewrite_size += 1;

            old_framing->head = vec_buf(&ext->slices.framing.vec);
            old_framing->tail = vec_buf_tail(&ext->slices.framing.vec);
            old_headers->head = vec_buf(&ext->slices.headers.vec);
            old_headers->tail = vec_buf_tail(&ext->slices.headers.vec);
            old_control->head = vec_buf(&ext->slices.control.vec);
            old_control->tail = vec_buf_tail(&ext->slices.control.vec);
            old_payload->head = old_control->tail;
            old_payload->tail = vec_buf_tail(&ext->primary->vec);
            XNIF_TRACE_F("[REWRITE] new_headers_size = %d, old_headers_size = %d\n", trap->rewrite_size, slice_len(old_headers));

            old_capacity = vec_len(&ext->primary->vec);
            new_capacity = slice_len(old_framing) + trap->rewrite_size + slice_len(old_control) + slice_len(old_payload);
            XNIF_TRACE_F("[REWRITE] new_capacity = %d, old_capacity = %d\n", new_capacity, old_capacity);
            (void)old_capacity;

            (void)vec_init_free(vec);

            if (!vec_create_owned(vec, new_capacity)) {
                err_term = EXCP_ERROR(caller_env, "Call to vec_create_owned() failed: unable to rewrite fragment header\n");
                return TRAP_ERR(err_term);
            }
            if (!vec_writer_create(vw, vec, 0)) {
                (void)vec_destroy(vec);
                err_term = EXCP_ERROR(caller_env, "Call to vec_writer_create() failed: unable to rewrite fragment header\n");
                return TRAP_ERR(err_term);
            }
            new_framing->head = vec_writer_raw_bytes(vw);
            if (!vec_writer_write_exact(vw, old_framing->head, slice_len(old_framing))) {
                (void)vec_destroy(vec);
                err_term = EXCP_ERROR(caller_env, "Call to vec_writer_write_exact() failed: unable to rewrite fragment header\n");
                return TRAP_ERR(err_term);
            }
            new_framing->tail = vec_writer_raw_bytes(vw);
            new_headers->head = vec_writer_raw_bytes(vw);
            if (!vec_writer_skip_exact(vw, trap->rewrite_size)) {
                (void)vec_destroy(vec);
                err_term = EXCP_ERROR(caller_env, "Call to vec_writer_skip_exact() failed: unable to rewrite fragment header\n");
                return TRAP_ERR(err_term);
            }
            new_headers->tail = vec_writer_raw_bytes(vw);
            new_control->head = vec_writer_raw_bytes(vw);
            if (!vec_writer_write_exact(vw, old_control->head, slice_len(old_control))) {
                (void)vec_destroy(vec);
                err_term = EXCP_ERROR(caller_env, "Call to vec_writer_write_exact() failed: unable to rewrite fragment header\n");
                return TRAP_ERR(err_term);
            }
            new_control->tail = new_control->head + slice_len(old_control);
            if (edf_external_has_payload(ext)) {
                new_payload->head = vec_writer_raw_bytes(vw);
                if (!vec_writer_write_exact(vw, old_payload->head, slice_len(old_payload))) {
                    (void)vec_destroy(vec);
                    err_term =
                        EXCP_ERROR(caller_env, "Call to vec_writer_write_exact() failed: unable to rewrite fragment header\n");
                    return TRAP_ERR(err_term);
                }
                new_payload->tail = new_payload->head + slice_len(old_payload);
            }

            if (vec_is_writable(vec)) {
                (void)vec_destroy(vec);
                err_term = EXCP_ERROR(caller_env, "Call to vec_is_writable() failed: unable to rewrite fragment header\n");
                return TRAP_ERR(err_term);
            }

            (void)vec_destroy(&ext->primary->vec);
            if (!vec_move(&ext->primary->vec, vec)) {
                (void)vec_destroy(vec);
                err_term =
                    EXCP_ERROR(caller_env, "Call to vec_move() failed: unable to move vec after rewriting the fragment header\n");
                return TRAP_ERR(err_term);
            }
            if (!edf_external_slice_framing_set(ext, new_framing->head, new_framing->tail)) {
                err_term =
                    EXCP_ERROR(caller_env, "Call to edf_external_slice_framing_set() failed: unable to rewrite fragment header\n");
                return TRAP_ERR(err_term);
            }
            if (!edf_external_slice_headers_set(ext, new_headers->head, new_headers->tail)) {
                err_term =
                    EXCP_ERROR(caller_env, "Call to edf_external_slice_headers_set() failed: unable to rewrite fragment header\n");
                return TRAP_ERR(err_term);
            }
            if (!edf_external_slice_control_set(ext, new_control->head, new_control->tail)) {
                err_term =
                    EXCP_ERROR(caller_env, "Call to edf_external_slice_control_set() failed: unable to rewrite fragment header\n");
                return TRAP_ERR(err_term);
            }
            if (edf_external_has_payload(ext) && !edf_external_slice_payload_set(ext, new_payload->head, new_payload->tail)) {
                err_term =
                    EXCP_ERROR(caller_env, "Call to edf_external_slice_payload_set() failed: unable to rewrite fragment header\n");
                return TRAP_ERR(err_term);
            }

            trap->internal_index = 0;
            trap->state = ETF_REWRITE_FRAGMENT_HEADER_TRAP_STATE_ENCODE;
            goto next_state;
        }
        case ETF_REWRITE_FRAGMENT_HEADER_TRAP_STATE_ENCODE: {
            uint32_t
                flags_buf[((ERTS_DIST_HDR_ATOM_CACHE_FLAG_BYTES(ERTS_MAX_INTERNAL_ATOM_CACHE_ENTRIES) - 1) / sizeof(uint32_t)) + 1];
            register uint8_t *ep = (uint8_t *)vec_buf_tail(&trap->external->slices.headers.vec);
            int flags_buf_index = 0;
            register uint32_t flags = 0;
            int flags_bytes = ERTS_DIST_HDR_ATOM_CACHE_FLAG_BYTES(attab->size);
            int used_half_bytes;
            int internal_index;
            const uint8_t *atom_text_name = NULL;
            size_t atom_text_len = 0;

            flags = ((attab->long_atoms == true) ? 1 : 0);
            if ((attab->size & 1) == 0) {
                used_half_bytes = 2;
            } else {
                used_half_bytes = 1;
            }
            internal_index = (int)(attab->size) - 1;
            while (internal_index >= 0) {
                edf_atom_translation_table_entry_t *entry = &(attab->entries[internal_index]);

                if (used_half_bytes != 8) {
                    flags <<= 4;
                } else {
                    flags_buf[flags_buf_index] = flags;
                    flags_buf_index += 1;
                    flags = 0;
                    used_half_bytes = 0;
                }

                if (entry->new_entry == true) {
                    if (!edf_atom_text_get_name(entry->atom, atom_encoding, &atom_text_name, &atom_text_len)) {
                        err_term = EXCP_ERROR_F(caller_env,
                                                "Call to edf_atom_text_get_name() failed: unable to resolve text for atom %T\n",
                                                entry->atom);
                        return TRAP_ERR(err_term);
                    }
                    ep -= atom_text_len;
                    XNIF_TRACE_F("[REWRITE] ep -= atom_text_len=%d\n", atom_text_len);
                    (void)memcpy(ep, atom_text_name, atom_text_len);
                    (void)edf_atom_text_drop_name(&atom_text_name);
                    if (attab->long_atoms == true) {
                        uint16_t atlen = htobe16((uint16_t)(atom_text_len));
                        ep -= 2;
                        XNIF_TRACE_F("[REWRITE] ep -= 2=%d (atom_text_len)\n", 2);
                        ep[0] = ((uint8_t *)&atlen)[0];
                        ep[1] = ((uint8_t *)&atlen)[1];
                    } else {
                        ep -= 1;
                        XNIF_TRACE_F("[REWRITE] ep -= 1=%d (atom_text_len)\n", 1);
                        ep[0] = (uint8_t)(atom_text_len);
                    }
                    ep -= 1;
                    XNIF_TRACE_F("[REWRITE] ep -= 1=%d (cache_index)\n", 1);
                    ep[0] = (uint8_t)(entry->cache_index);
                    flags |= (8 | ((entry->cache_index >> 8) & 7));
                } else {
                    ep -= 1;
                    XNIF_TRACE_F("[REWRITE] ep -= 1=%d (cache_index)\n", 1);
                    ep[0] = (uint8_t)(entry->cache_index);
                    flags |= ((entry->cache_index >> 8) & 7);
                }

                internal_index -= 1;
                used_half_bytes += 1;
            }
            flags_buf[flags_buf_index] = flags;

            flags_buf_index = 0;
            while (1) {
                flags = flags_buf[flags_buf_index];
                if (flags_bytes > 4) {
                    ep -= 1;
                    ep[0] = (uint8_t)((flags >> 24) & 0xff);
                    ep -= 1;
                    ep[0] = (uint8_t)((flags >> 16) & 0xff);
                    ep -= 1;
                    ep[0] = (uint8_t)((flags >> 8) & 0xff);
                    ep -= 1;
                    ep[0] = (uint8_t)(flags & 0xff);
                    flags_buf_index += 1;
                    flags_bytes -= 4;
                } else {
                    switch (flags_bytes) {
                    case 4:
                        ep -= 1;
                        ep[0] = (uint8_t)((flags >> 24) & 0xff);
                        [[fallthrough]];
                    case 3:
                        ep -= 1;
                        ep[0] = (uint8_t)((flags >> 16) & 0xff);
                        [[fallthrough]];
                    case 2:
                        ep -= 1;
                        ep[0] = (uint8_t)((flags >> 8) & 0xff);
                        [[fallthrough]];
                    case 1:
                        ep -= 1;
                        ep[0] = (uint8_t)(flags & 0xff);
                    }
                    break;
                }
            }
            ep -= 1;
            ep[0] = (uint8_t)(attab->size);

            if (ep != vec_buf_tail(&trap->external->slices.headers.vec)) {
                XNIF_TRACE_F("[REWRITE] WHY IN THE WORLD: %d\n", vec_buf_tail(&trap->external->slices.headers.vec) - ep);
            }

            TRAP_REDUCE(trap, trap->rewrite_size);
            CHANNEL_RX_STATS_COUNT(trap->external->channel, rewrite_fragment_header_count, 1);
            trap->state = ETF_REWRITE_FRAGMENT_HEADER_TRAP_STATE_DONE;
            goto next_state;
        }
        case ETF_REWRITE_FRAGMENT_HEADER_TRAP_STATE_DONE: {
            return TRAP_OK(THE_NON_VALUE);
        }
        default:
            return TRAP_ERR(EXCP_ERROR_F(caller_env, "Fatal error: unknown etf_rewrite_fragment_header_trap_t->state value %d\n",
                                         (int)(trap->state)));
        }
    next_state: {
        if (TRAP_SHOULD_YIELD(trap)) {
            return TRAP_YIELD();
        }
        continue;
    }
    } while (1);
}
