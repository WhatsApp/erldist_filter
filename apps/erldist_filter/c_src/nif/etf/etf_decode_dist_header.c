/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "etf_decode_dist_header.h"

#include "../channel/edf_channel.h"

#include "../core/xnif_trace.h"

static void etf_decode_dist_header_trap_dtor(ErlNifEnv *caller_env, edf_trap_t *super, void *arg);
static edf_trap_result_t etf_decode_dist_header_trap_next(ErlNifEnv *caller_env, edf_trap_t *super, void *arg);

ERL_NIF_TERM
etf_decode_dist_header_trap_open(ErlNifEnv *env, edf_external_t *external, etf_decode_dist_header_trap_t **trapp)
{
    edf_trap_state_t trap_state = {
        .resource = NULL,
        .acquire = NULL,
        .release = NULL,
        .dtor = etf_decode_dist_header_trap_dtor,
        .edit = NULL,
        .next = etf_decode_dist_header_trap_next,
        .arg = NULL,
    };
    etf_decode_dist_header_trap_t *trap = NULL;
    ERL_NIF_TERM err_term = THE_NON_VALUE;

    if (!edf_trap_open_x(env, &trap_state, sizeof(etf_decode_dist_header_trap_t), (edf_trap_t **)(&trap), &err_term)) {
        return err_term;
    }

    trap->super.state.arg = (void *)trap;
    trap->state = ETF_DECODE_DIST_HEADER_TRAP_STATE_INIT;
    trap->external = external;
    (void)vec_init_free(&trap->vec);
    trap->skip = 0;
    trap->number_of_atom_cache_refs = 0;
    trap->flags_size = 0;
    trap->flagsp = NULL;
    trap->long_atoms = false;
    trap->table_index = 0;
    trap->head = NULL;
    trap->tail = NULL;

    if (!edf_external_slice_headers_get(trap->external, &trap->vec)) {
        err_term = EXCP_ERROR(env, "Call to edf_external_slice_headers_get() failed");
        (void)enif_release_resource((void *)trap);
        return err_term;
    }

    if (trapp != NULL) {
        *trapp = trap;
    }

    return edf_trap_open_x_make_term(env, &trap->super);
}

void
etf_decode_dist_header_trap_dtor(ErlNifEnv *caller_env, edf_trap_t *super, void *arg)
{
    etf_decode_dist_header_trap_t *trap = (void *)arg;

    XNIF_TRACE_F("%s:%d [trap] dtor callback\n", __FILE__, __LINE__);

    (void)caller_env;
    (void)super;

    trap->external = NULL;
    (void)vec_destroy(&trap->vec);

    return;
}

edf_trap_result_t
etf_decode_dist_header_trap_next(ErlNifEnv *caller_env, edf_trap_t *super, void *arg)
{
    etf_decode_dist_header_trap_t *trap = (void *)arg;
    uint64_t dflags = trap->external->channel->dflags;
    edf_atom_cache_t *cache = trap->external->channel->rx.cache;
    edf_atom_translation_table_t *attab = &(trap->external->attab);
    vec_reader_t vec_reader;
    vec_reader_t *vr = &vec_reader;
    ERL_NIF_TERM err_term = THE_NON_VALUE;

#define RAW_BYTES() vec_reader_raw_bytes(vr)

#define READ_U8(val)                                                                                                               \
    do {                                                                                                                           \
        if (!vec_reader_read_u8(vr, (val))) {                                                                                      \
            err_term = EXCP_ERROR(caller_env, "Call to vec_reader_read_u8() failed: unable to decode dist header\n");              \
            (void)vec_reader_destroy(vr);                                                                                          \
            return TRAP_ERR(err_term);                                                                                             \
        }                                                                                                                          \
        TRAP_REDUCE(trap, 1);                                                                                                      \
    } while (0)

#define READ_U16(val)                                                                                                              \
    do {                                                                                                                           \
        if (!vec_reader_read_u16(vr, (val))) {                                                                                     \
            err_term = EXCP_ERROR(caller_env, "Call to vec_reader_read_u16() failed: unable to decode dist header\n");             \
            (void)vec_reader_destroy(vr);                                                                                          \
            return TRAP_ERR(err_term);                                                                                             \
        }                                                                                                                          \
        TRAP_REDUCE(trap, 2);                                                                                                      \
    } while (0)

#define READ_U32(val)                                                                                                              \
    do {                                                                                                                           \
        if (!vec_reader_read_u32(vr, (val))) {                                                                                     \
            err_term = EXCP_ERROR(caller_env, "Call to vec_reader_read_u32() failed: unable to decode dist header\n");             \
            (void)vec_reader_destroy(vr);                                                                                          \
            return TRAP_ERR(err_term);                                                                                             \
        }                                                                                                                          \
        TRAP_REDUCE(trap, 4);                                                                                                      \
    } while (0)

#define SKIP(sz)                                                                                                                   \
    do {                                                                                                                           \
        if (!vec_reader_skip_exact(vr, (sz))) {                                                                                    \
            err_term = EXCP_ERROR(caller_env, "Call to vec_reader_skip_exact() failed: unable to decode dist header\n");           \
            (void)vec_reader_destroy(vr);                                                                                          \
            return TRAP_ERR(err_term);                                                                                             \
        }                                                                                                                          \
        TRAP_REDUCE(trap, (sz));                                                                                                   \
    } while (0)

    if (!vec_reader_create(vr, &trap->vec, trap->skip)) {
        return TRAP_ERR(EXCP_ERROR(caller_env, "Call to vec_reader_create() failed\n"));
    }

    do {
        switch (trap->state) {
        case ETF_DECODE_DIST_HEADER_TRAP_STATE_INIT: {
            trap->head = RAW_BYTES();
            READ_U8(&trap->number_of_atom_cache_refs);
            (void)edf_atom_translation_table_set_size(attab, (size_t)(trap->number_of_atom_cache_refs));
            trap->skip += 1;
            if (trap->number_of_atom_cache_refs > 0) {
                int byte_ix;
                int bit_ix;
                trap->flags_size = (size_t)(ERTS_DIST_HDR_ATOM_CACHE_FLAG_BYTES(trap->number_of_atom_cache_refs));
                trap->flagsp = RAW_BYTES();
                SKIP(trap->flags_size);
                trap->skip += trap->flags_size;
                byte_ix = (int)(ERTS_DIST_HDR_ATOM_CACHE_FLAG_BYTE_IX(trap->number_of_atom_cache_refs));
                bit_ix = (int)(ERTS_DIST_HDR_ATOM_CACHE_FLAG_BIT_IX(trap->number_of_atom_cache_refs));
                if (trap->flagsp[byte_ix] & (((uint8_t)ERTS_DIST_HDR_LONG_ATOMS_FLG) << bit_ix)) {
                    trap->long_atoms = true;
                }
                trap->external->flags |= EDF_EXTERNAL_FLAG_ATOM_CACHE_REFS;
            }
            trap->state = ETF_DECODE_DIST_HEADER_TRAP_STATE_READ_FLAGS;
            goto next_state;
        }
        case ETF_DECODE_DIST_HEADER_TRAP_STATE_READ_FLAGS: {
            if (trap->number_of_atom_cache_refs > 0) {
                register uint32_t flags = 0;
                int got_flags = 0;
                while (trap->table_index < (int)(trap->number_of_atom_cache_refs)) {
                    uint8_t internal_segment_index;
                    size_t cache_index;
                    size_t atom_length;
                    ERL_NIF_TERM atom_term;

                    if (got_flags == 0 && TRAP_SHOULD_YIELD(trap)) {
                        // Since we're storing `flags` in a register, only yield if it's time to fetch the next segment.
                        (void)vec_reader_destroy(vr);
                        return TRAP_YIELD();
                    }

                    if (!got_flags) {
                        int half_bytes_remaining = ((int)(trap->number_of_atom_cache_refs)) - trap->table_index;
                        if (half_bytes_remaining > 6) {
                            flags = ((((uint32_t)trap->flagsp[3]) << 24) | (((uint32_t)trap->flagsp[2]) << 16) |
                                     (((uint32_t)trap->flagsp[1]) << 8) | ((uint32_t)trap->flagsp[0]));
                            trap->flagsp += 4;
                        } else {
                            flags = 0;
                            switch (half_bytes_remaining) {
                            case 6:
                                [[fallthrough]];
                            case 5:
                                flags |= (((uint32_t)trap->flagsp[2]) << 16);
                                [[fallthrough]];
                            case 4:
                                [[fallthrough]];
                            case 3:
                                flags |= (((uint32_t)trap->flagsp[1]) << 8);
                                [[fallthrough]];
                            case 2:
                                [[fallthrough]];
                            case 1:
                                flags |= ((uint32_t)trap->flagsp[0]);
                            }
                        }
                        got_flags = 8;
                    }

                    cache_index = (size_t)((flags & 7) << 8);
                    READ_U8(&internal_segment_index);
                    trap->skip += 1;
                    cache_index += (size_t)internal_segment_index;
                    if (cache_index >= ERTS_ATOM_CACHE_SIZE) {
                        err_term = EXCP_ERROR_F(
                            caller_env,
                            "Dist Header atom cache entry cache_index=%u is greater than or equal to ERTS_ATOM_CACHE_SIZE=%u\n",
                            cache_index, ERTS_ATOM_CACHE_SIZE);
                        (void)vec_reader_destroy(vr);
                        return TRAP_ERR(err_term);
                    }
                    if ((flags & 8) == 0) {
                        /* atom already cached */
                        atom_term = cache->entries[cache_index];
                        if (atom_term == THE_NON_VALUE) {
                            err_term = EXCP_ERROR_F(caller_env,
                                                    "Dist Header existing atom cache entry cache_index=%u points to THE_NON_VALUE "
                                                    "in the cache, expected cached atom\n",
                                                    cache_index);
                            (void)vec_reader_destroy(vr);
                            return TRAP_ERR(err_term);
                        }
                        if (!edf_atom_translation_table_set_entry(attab, (int)cache_index, trap->table_index, atom_term, false)) {
                            err_term = EXCP_ERROR_F(
                                caller_env,
                                "Call to edf_atom_translation_table_set_entry() failed: Dist Header existing atom cache entry "
                                "cache_index=%u with atom_term=%T unable to set translation table\n",
                                cache_index, atom_term);
                            (void)vec_reader_destroy(vr);
                            return TRAP_ERR(err_term);
                        }
                        trap->external->flags |= EDF_EXTERNAL_FLAG_ATOM_CACHE_READ;
                        CHANNEL_RX_STATS_COUNT(trap->external->channel, atom_cache_read_count, 1);
                    } else {
                        /* new cached atom */
                        const uint8_t *atom_text = NULL;
                        ErlNifCharEncoding atom_encoding;
                        if (trap->long_atoms) {
                            uint16_t long_atom_length;
                            READ_U16(&long_atom_length);
                            trap->skip += 2;
                            atom_length = (size_t)long_atom_length;
                        } else {
                            uint8_t short_atom_length;
                            READ_U8(&short_atom_length);
                            trap->skip += 1;
                            atom_length = (size_t)short_atom_length;
                        }
                        if (atom_length > MAX_ATOM_SZ_LIMIT) {
                            err_term = EXCP_ERROR_F(caller_env,
                                                    "Dist Header new atom cache entry cache_index=%u has an atom_length=%u which "
                                                    "is greater than MAX_ATOM_SZ_LIMIT=%u\n",
                                                    cache_index, atom_length, MAX_ATOM_SZ_LIMIT);
                            (void)vec_reader_destroy(vr);
                            return TRAP_ERR(err_term);
                        }
                        atom_text = RAW_BYTES();
                        SKIP(atom_length);
                        trap->skip += atom_length;
                        if (!(dflags & DFLAG_UTF8_ATOMS)) {
                            atom_encoding = ERL_NIF_LATIN1;
                        } else {
                            atom_encoding = ERL_NIF_UTF8;
                        }
                        if (!edf_atom_text_put(atom_text, atom_length, atom_encoding, &atom_term)) {
                            err_term = EXCP_ERROR_F(caller_env,
                                                    "Call to edf_atom_text_put() failed: Dist Header new atom cache entry "
                                                    "cache_index=%u with atom_length=%u unable to create atom\n",
                                                    cache_index, atom_length);
                            (void)vec_reader_destroy(vr);
                            return TRAP_ERR(err_term);
                        }
                        if (cache->entries[cache_index] != THE_NON_VALUE) {
                            trap->external->flags |= EDF_EXTERNAL_FLAG_ATOM_CACHE_OVERWRITE;
                            CHANNEL_RX_STATS_COUNT(trap->external->channel, atom_cache_overwrite_count, 1);
                        }
                        // NOTE: for now, don't enforce this; parallel SPBT shows that it's possible to overwrite the atom cache
                        // if (cache->entries[cache_index] != THE_NON_VALUE) {
                        //     // Atom cache entries must be write-once, and then read-only.
                        //     err_term = EXCP_ERROR_F(caller_env,
                        //                             "Dist Header new atom cache entry cache_index=%u with atom_length=%u, "
                        //                             "new_atom=%T attempting to overwrite existing entry existing_atom=%T\n",
                        //                             cache_index, atom_length, atom_term, cache->entries[cache_index]);
                        //     (void)vec_reader_destroy(vr);
                        //     return TRAP_ERR(err_term);
                        // }
                        // cache->entries[cache_index] = atom_term;
                        if (!edf_atom_cache_maybe_overwrite(cache, cache_index, atom_term)) {
                            err_term =
                                EXCP_ERROR_F(caller_env,
                                             "Call to edf_atom_cache_maybe_overwrite() failed: Dist Header new atom cache entry "
                                             "cache_index=%u with atom_length=%u unable to create atom\n",
                                             cache_index, atom_length);
                            (void)vec_reader_destroy(vr);
                            return TRAP_ERR(err_term);
                        }
                        if (!edf_atom_translation_table_set_entry(attab, (int)cache_index, trap->table_index, atom_term, true)) {
                            err_term = EXCP_ERROR_F(
                                caller_env,
                                "Call to edf_atom_translation_table_set_entry() failed: Dist Header new atom cache entry "
                                "cache_index=%u with atom_length=%u unable to create atom\n",
                                cache_index, atom_length);
                            (void)vec_reader_destroy(vr);
                            return TRAP_ERR(err_term);
                        }
                        // attab->atom[trap->table_index] = atom_term;
                        // attab->cix[trap->table_index] = cache_index;
                        // if (acmp != NULL) {
                        //     (void)edf_atom_cache_map_set_entry(acmp, cache_index, trap->table_index, atom_term, true);
                        // }
                        trap->external->flags |= EDF_EXTERNAL_FLAG_ATOM_CACHE_WRITE;
                        CHANNEL_RX_STATS_COUNT(trap->external->channel, atom_cache_write_count, 1);
                    }
                    flags >>= 4;
                    got_flags -= 1;
                    trap->table_index += 1;
                }
            }
            trap->state = ETF_DECODE_DIST_HEADER_TRAP_STATE_DONE;
            goto next_state;
        }
        case ETF_DECODE_DIST_HEADER_TRAP_STATE_DONE: {
            trap->tail = RAW_BYTES();
            if (!edf_external_slice_headers_set(trap->external, trap->head, trap->tail)) {
                return TRAP_ERR(EXCP_ERROR(caller_env, "Call to edf_external_slice_headers_set() failed\n"));
            }
            return TRAP_OK(THE_NON_VALUE);
        }
        default:
            (void)vec_reader_destroy(vr);
            return TRAP_ERR(EXCP_ERROR_F(caller_env, "Fatal error: unknown etf_decode_dist_header_trap_t->state value %d\n",
                                         (int)(trap->state)));
        }
    next_state: {
        if (TRAP_SHOULD_YIELD(trap)) {
            (void)vec_reader_destroy(vr);
            return TRAP_YIELD();
        }
        continue;
    }
    } while (1);

#undef SKIP
#undef READ_U32
#undef READ_U16
#undef READ_U8
#undef RAW_BYTES
}

int
etf_fast_decode_dist_header(ErlNifEnv *caller_env, edf_channel_t *channel, edf_atom_translation_table_t *attab, vec_reader_t *vr,
                            int *external_flags, slice_t *headers, ERL_NIF_TERM *err_termp)
{
    uint64_t dflags = channel->dflags;
    edf_atom_cache_t *cache = channel->rx.cache;
    uint8_t number_of_atom_cache_refs = 0;
    size_t flags_size = 0;
    const uint8_t *flagsp = NULL;
    bool long_atoms = false;
    int got_flags = 0;
    int table_index = 0;

#define RAW_BYTES() vec_reader_raw_bytes(vr)

#define READ_U8(val)                                                                                                               \
    do {                                                                                                                           \
        if (!vec_reader_read_u8(vr, (val))) {                                                                                      \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_read_u8() failed: unable to decode dist header\n");            \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define READ_U16(val)                                                                                                              \
    do {                                                                                                                           \
        if (!vec_reader_read_u16(vr, (val))) {                                                                                     \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_read_u16() failed: unable to decode dist header\n");           \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define READ_U32(val)                                                                                                              \
    do {                                                                                                                           \
        if (!vec_reader_read_u32(vr, (val))) {                                                                                     \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_read_u32() failed: unable to decode dist header\n");           \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define SKIP(sz)                                                                                                                   \
    do {                                                                                                                           \
        if (!vec_reader_skip_exact(vr, (sz))) {                                                                                    \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_skip_exact() failed: unable to decode dist header\n");         \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

    headers->head = RAW_BYTES();
    READ_U8(&number_of_atom_cache_refs);
    (void)edf_atom_translation_table_set_size(attab, (size_t)(number_of_atom_cache_refs));
    if (number_of_atom_cache_refs > 0) {
        int byte_ix;
        int bit_ix;
        flags_size = (size_t)(ERTS_DIST_HDR_ATOM_CACHE_FLAG_BYTES(number_of_atom_cache_refs));
        flagsp = RAW_BYTES();
        SKIP(flags_size);
        byte_ix = (int)(ERTS_DIST_HDR_ATOM_CACHE_FLAG_BYTE_IX(number_of_atom_cache_refs));
        bit_ix = (int)(ERTS_DIST_HDR_ATOM_CACHE_FLAG_BIT_IX(number_of_atom_cache_refs));
        if (flagsp[byte_ix] & (((uint8_t)ERTS_DIST_HDR_LONG_ATOMS_FLG) << bit_ix)) {
            long_atoms = true;
        }
        *external_flags |= EDF_EXTERNAL_FLAG_ATOM_CACHE_REFS;
    }
    if (number_of_atom_cache_refs > 0) {
        register uint32_t flags = 0;
        while (table_index < (int)(number_of_atom_cache_refs)) {
            uint8_t internal_segment_index;
            size_t cache_index;
            size_t atom_length;
            ERL_NIF_TERM atom_term;

            if (!got_flags) {
                int half_bytes_remaining = ((int)(number_of_atom_cache_refs)) - table_index;
                if (half_bytes_remaining > 6) {
                    flags = ((((uint32_t)flagsp[3]) << 24) | (((uint32_t)flagsp[2]) << 16) | (((uint32_t)flagsp[1]) << 8) |
                             ((uint32_t)flagsp[0]));
                    flagsp += 4;
                } else {
                    flags = 0;
                    switch (half_bytes_remaining) {
                    case 6:
                        [[fallthrough]];
                    case 5:
                        flags |= (((uint32_t)flagsp[2]) << 16);
                        [[fallthrough]];
                    case 4:
                        [[fallthrough]];
                    case 3:
                        flags |= (((uint32_t)flagsp[1]) << 8);
                        [[fallthrough]];
                    case 2:
                        [[fallthrough]];
                    case 1:
                        flags |= ((uint32_t)flagsp[0]);
                    }
                }
                got_flags = 8;
            }

            cache_index = (size_t)((flags & 7) << 8);
            READ_U8(&internal_segment_index);
            cache_index += (size_t)internal_segment_index;
            if (cache_index >= ERTS_ATOM_CACHE_SIZE) {
                *err_termp = EXCP_ERROR_F(
                    caller_env, "Dist Header atom cache entry cache_index=%u is greater than or equal to ERTS_ATOM_CACHE_SIZE=%u\n",
                    cache_index, ERTS_ATOM_CACHE_SIZE);
                return 0;
            }
            if ((flags & 8) == 0) {
                /* atom already cached */
                atom_term = cache->entries[cache_index];
                if (atom_term == THE_NON_VALUE) {
                    *err_termp = EXCP_ERROR_F(caller_env,
                                              "Dist Header existing atom cache entry cache_index=%u points to THE_NON_VALUE in the "
                                              "cache, expected cached atom\n",
                                              cache_index);
                    return 0;
                }
                if (!edf_atom_translation_table_set_entry(attab, (int)cache_index, table_index, atom_term, false)) {
                    *err_termp = EXCP_ERROR_F(caller_env,
                                              "Call to edf_atom_translation_table_set_entry() failed: Dist Header existing atom "
                                              "cache entry cache_index=%u with atom_term=%T unable to set translation table\n",
                                              cache_index, atom_term);
                    return 0;
                }
                *external_flags |= EDF_EXTERNAL_FLAG_ATOM_CACHE_READ;
                CHANNEL_RX_STATS_COUNT(channel, atom_cache_read_count, 1);
            } else {
                /* new cached atom */
                const uint8_t *atom_text = NULL;
                ErlNifCharEncoding atom_encoding;
                if (long_atoms) {
                    uint16_t long_atom_length;
                    READ_U16(&long_atom_length);
                    atom_length = (size_t)long_atom_length;
                } else {
                    uint8_t short_atom_length;
                    READ_U8(&short_atom_length);
                    atom_length = (size_t)short_atom_length;
                }
                if (atom_length > MAX_ATOM_SZ_LIMIT) {
                    *err_termp = EXCP_ERROR_F(caller_env,
                                              "Dist Header new atom cache entry cache_index=%u has an atom_length=%u which "
                                              "is greater than MAX_ATOM_SZ_LIMIT=%u\n",
                                              cache_index, atom_length, MAX_ATOM_SZ_LIMIT);
                    return 0;
                }
                atom_text = RAW_BYTES();
                SKIP(atom_length);
                if (!(dflags & DFLAG_UTF8_ATOMS)) {
                    atom_encoding = ERL_NIF_LATIN1;
                } else {
                    atom_encoding = ERL_NIF_UTF8;
                }
                if (!edf_atom_text_put(atom_text, atom_length, atom_encoding, &atom_term)) {
                    *err_termp = EXCP_ERROR_F(caller_env,
                                              "Call to edf_atom_text_put() failed: Dist Header new atom cache entry "
                                              "cache_index=%u with atom_length=%u unable to create atom\n",
                                              cache_index, atom_length);
                    return 0;
                }
                if (cache->entries[cache_index] != THE_NON_VALUE) {
                    *external_flags |= EDF_EXTERNAL_FLAG_ATOM_CACHE_OVERWRITE;
                    CHANNEL_RX_STATS_COUNT(channel, atom_cache_overwrite_count, 1);
                }
                if (!edf_atom_cache_maybe_overwrite(cache, cache_index, atom_term)) {
                    *err_termp = EXCP_ERROR_F(caller_env,
                                              "Call to edf_atom_cache_maybe_overwrite() failed: Dist Header new atom cache entry "
                                              "cache_index=%u with atom_length=%u unable to create atom\n",
                                              cache_index, atom_length);
                    return 0;
                }
                if (!edf_atom_translation_table_set_entry(attab, (int)cache_index, table_index, atom_term, true)) {
                    *err_termp =
                        EXCP_ERROR_F(caller_env,
                                     "Call to edf_atom_translation_table_set_entry() failed: Dist Header new atom cache entry "
                                     "cache_index=%u with atom_length=%u unable to create atom\n",
                                     cache_index, atom_length);
                    return 0;
                }
                *external_flags |= EDF_EXTERNAL_FLAG_ATOM_CACHE_WRITE;
                CHANNEL_RX_STATS_COUNT(channel, atom_cache_write_count, 1);
            }
            flags >>= 4;
            got_flags -= 1;
            table_index += 1;
        }
    }
    headers->tail = RAW_BYTES();
    return 1;

#undef SKIP
#undef READ_U32
#undef READ_U16
#undef READ_U8
#undef RAW_BYTES
}
