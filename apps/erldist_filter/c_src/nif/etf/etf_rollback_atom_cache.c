/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "etf_rollback_atom_cache.h"

#include "../erts/atom.h"
#include "../erts/dist.h"
#include "../erts/external.h"

#include "../channel/edf_channel.h"

static void etf_rollback_atom_cache_trap_dtor(ErlNifEnv *caller_env, edf_trap_t *super, void *arg);
static edf_trap_result_t etf_rollback_atom_cache_trap_next(ErlNifEnv *caller_env, edf_trap_t *super, void *arg);

ERL_NIF_TERM
etf_rollback_atom_cache_trap_open(ErlNifEnv *env, edf_external_t *external, etf_rollback_atom_cache_trap_t **trapp)
{
    edf_trap_state_t trap_state = {
        .resource = NULL,
        .acquire = NULL,
        .release = NULL,
        .dtor = etf_rollback_atom_cache_trap_dtor,
        .edit = NULL,
        .next = etf_rollback_atom_cache_trap_next,
        .arg = NULL,
    };
    etf_rollback_atom_cache_trap_t *trap = NULL;
    ERL_NIF_TERM trap_term;

    trap_term = edf_trap_open(env, &trap_state, sizeof(etf_rollback_atom_cache_trap_t), (edf_trap_t **)(&trap));
    if (trap == NULL) {
        return trap_term;
    }

    trap->super.state.arg = (void *)trap;
    trap->state = ETF_ROLLBACK_ATOM_CACHE_TRAP_STATE_INIT;
    trap->external = external;
    trap->internal_index = 0;
    trap->rollback_size = 0;

    if (trapp != NULL) {
        *trapp = trap;
    }

    return trap_term;
}

void
etf_rollback_atom_cache_trap_dtor(ErlNifEnv *caller_env, edf_trap_t *super, void *arg)
{
    etf_rollback_atom_cache_trap_t *trap = (void *)arg;

    XNIF_TRACE_F("%s:%d [trap] dtor callback\n", __FILE__, __LINE__);

    (void)caller_env;
    (void)super;

    trap->external = NULL;

    return;
}

edf_trap_result_t
etf_rollback_atom_cache_trap_next(ErlNifEnv *caller_env, edf_trap_t *super, void *arg)
{
    static const uint8_t reg_send_noop[36] = {104, 4, 97,  6, 88, 118, 0, 0, 0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
                                              0,   0, 118, 0, 0,  118, 0, 9, 117, 110, 100, 101, 102, 105, 110, 101, 100, 106};
    etf_rollback_atom_cache_trap_t *trap = (void *)arg;
    uint64_t dflags = trap->external->channel->dflags;
    ErlNifCharEncoding atom_encoding = ((!(dflags & DFLAG_UTF8_ATOMS)) ? ERL_NIF_LATIN1 : ERL_NIF_UTF8);
    edf_atom_translation_table_t *rollback = &(trap->external->rollback);
    ERL_NIF_TERM err_term = THE_NON_VALUE;

    do {
        switch (trap->state) {
        case ETF_ROLLBACK_ATOM_CACHE_TRAP_STATE_INIT: {
            if (rollback == NULL || rollback->size == 0) {
                trap->state = ETF_ROLLBACK_ATOM_CACHE_TRAP_STATE_DONE;
                goto next_state;
            }
            trap->internal_index = 0;
            trap->rollback_size = 0;
            trap->state = ETF_ROLLBACK_ATOM_CACHE_TRAP_STATE_ALLOC;
            goto next_state;
        }
        case ETF_ROLLBACK_ATOM_CACHE_TRAP_STATE_ALLOC: {
            size_t frame_size;

            while (trap->internal_index < (int)(rollback->size)) {
                edf_atom_translation_table_entry_t *conflict = &(rollback->entries[trap->internal_index]);
                size_t atom_text_len = 0;
                if (!edf_atom_text_get_length(conflict->atom, atom_encoding, &atom_text_len)) {
                    err_term = EXCP_ERROR_F(
                        caller_env, "Call to edf_atom_text_get_length() failed: unable to resolve length for conflict atom %T\n",
                        conflict->atom);
                    return TRAP_ERR(err_term);
                }
                trap->rollback_size += 1 + ((rollback->long_atoms == true) ? 2 : 1) + atom_text_len;
                trap->internal_index += 1;
                TRAP_REDUCE(trap, 1);
                if (TRAP_SHOULD_YIELD(trap)) {
                    return TRAP_YIELD();
                }
            }

            XNIF_TRACE_F("[ROLLBACK] %d BYTES ARE = %d\n", rollback->size, ERTS_DIST_HDR_ATOM_CACHE_FLAG_BYTES(rollback->size));
            trap->rollback_size += ERTS_DIST_HDR_ATOM_CACHE_FLAG_BYTES(rollback->size);
            trap->rollback_size += 1;
            XNIF_TRACE_F("[ROLLBACK] SIZE = %d\n", trap->rollback_size);

            frame_size = 1 + 1 + 8 + 8 + trap->rollback_size + sizeof(reg_send_noop);

            if (!vec_create_owned(&trap->external->rollback_vec, frame_size)) {
                return TRAP_ERR(EXCP_ERROR(caller_env, "Call to vec_create_owned() failed: out of memory\n"));
            }

            trap->internal_index = 0;
            trap->state = ETF_ROLLBACK_ATOM_CACHE_TRAP_STATE_ENCODE;
            goto next_state;
        }
        case ETF_ROLLBACK_ATOM_CACHE_TRAP_STATE_ENCODE: {
            uint32_t
                flags_buf[((ERTS_DIST_HDR_ATOM_CACHE_FLAG_BYTES(ERTS_MAX_INTERNAL_ATOM_CACHE_ENTRIES) - 1) / sizeof(uint32_t)) + 1];
            register uint8_t *ep = NULL;
            int flags_buf_index = 0;
            register uint32_t flags = 0;
            int flags_bytes = ERTS_DIST_HDR_ATOM_CACHE_FLAG_BYTES(rollback->size);
            int used_half_bytes;
            int internal_index;
            const uint8_t *atom_text_name = NULL;
            size_t atom_text_len = 0;
            vec_t *ev = &trap->external->rollback_vec;
            vec_writer_t emit_vec_writer;
            vec_writer_t *vw = &emit_vec_writer;

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

            ep = vec_writer_raw_bytes(vw) + trap->rollback_size;
            flags = ((rollback->long_atoms == true) ? 1 : 0);
            if ((rollback->size & 1) == 0) {
                used_half_bytes = 2;
            } else {
                used_half_bytes = 1;
            }
            internal_index = (int)(rollback->size) - 1;
            while (internal_index >= 0) {
                edf_atom_translation_table_entry_t *conflict = &(rollback->entries[internal_index]);

                if (used_half_bytes != 8) {
                    flags <<= 4;
                } else {
                    flags_buf[flags_buf_index] = flags;
                    flags_buf_index += 1;
                    flags = 0;
                    used_half_bytes = 0;
                }
                if (!edf_atom_text_get_name(conflict->atom, atom_encoding, &atom_text_name, &atom_text_len)) {
                    err_term = EXCP_ERROR_F(
                        caller_env, "Call to edf_atom_text_get_name() failed: unable to resolve text for conflict atom %T\n",
                        conflict->atom);
                    return TRAP_ERR(err_term);
                }
                ep -= atom_text_len;
                (void)memcpy(ep, atom_text_name, atom_text_len);
                (void)edf_atom_text_drop_name(&atom_text_name);
                if (rollback->long_atoms == true) {
                    uint16_t atlen = htobe16((uint16_t)(atom_text_len));
                    ep -= 2;
                    ep[0] = ((uint8_t *)&atlen)[0];
                    ep[1] = ((uint8_t *)&atlen)[1];
                } else {
                    ep -= 1;
                    ep[0] = (uint8_t)(atom_text_len);
                }
                ep -= 1;
                ep[0] = (uint8_t)(conflict->cache_index);
                flags |= (8 | ((conflict->cache_index >> 8) & 7));
                used_half_bytes += 1;

                internal_index -= 1;
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
            ep[0] = (uint8_t)(rollback->size);

            if (ep != vec_writer_raw_bytes(vw)) {
                XNIF_TRACE_F("[ROLLBACK] WHY IN THE WORLD: %d %d\n", vec_writer_raw_bytes(vw) - ep, vec_writer_raw_bytes(vw) - ep);
            }

            TRAP_REDUCE(trap, trap->rollback_size);

            (void)vec_writer_skip_exact(vw, trap->rollback_size);

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
            CHANNEL_RX_STATS_COUNT(trap->external->channel, rollback_atom_cache_count, 1);
            trap->state = ETF_ROLLBACK_ATOM_CACHE_TRAP_STATE_DONE;
            goto next_state;
#undef TRAP_PREP_ERR
        }
        case ETF_ROLLBACK_ATOM_CACHE_TRAP_STATE_DONE: {
            return TRAP_OK(THE_NON_VALUE);
        }
        default:
            return TRAP_ERR(EXCP_ERROR_F(caller_env, "Fatal error: unknown etf_rollback_atom_cache_trap_t->state value %d\n",
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
