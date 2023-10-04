/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "etf_decode_term_length.h"
#include "../vterm/vterm.h"

#include "../core/xnif_trace.h"

#define MAP_SMALL_MAP_LIMIT (32)

static void etf_decode_term_length_trap_dtor(ErlNifEnv *caller_env, edf_trap_t *super, void *arg);
static void etf_decode_term_length_trap_edit(ErlNifEnv *caller_env, edf_trap_t *super, void *arg, edf_trap_result_t *result);
static edf_trap_result_t etf_decode_term_length_trap_next(ErlNifEnv *caller_env, edf_trap_t *super, void *arg);

ERL_NIF_TERM
etf_decode_term_length_trap_open(ErlNifEnv *env, bool is_external_term, vec_t *slice, etf_decode_term_length_done_t done_cb,
                                 void *done_arg, etf_decode_term_length_trap_t **trapp)
{
    edf_trap_state_t trap_state = {
        .resource = NULL,
        .acquire = NULL,
        .release = NULL,
        .dtor = etf_decode_term_length_trap_dtor,
        .edit = etf_decode_term_length_trap_edit,
        .next = etf_decode_term_length_trap_next,
        .arg = NULL,
    };
    etf_decode_term_length_trap_t *trap = NULL;
    ERL_NIF_TERM trap_term;

    if (!vec_is_slice(slice)) {
        return EXCP_ERROR(env, "Call to etf_decode_term_length_trap_open() failed: slice must be a vec_t with tag=VEC_TAG_SLICE\n");
    }

    trap_term = edf_trap_open(env, &trap_state, sizeof(etf_decode_term_length_trap_t), (edf_trap_t **)(&trap));
    if (trap == NULL) {
        return trap_term;
    }

    trap->super.state.arg = (void *)trap;
    trap->state = ETF_DECODE_TERM_LENGTH_TRAP_STATE_INIT;
    trap->is_external_term = is_external_term;
    (void)vec_init_free(&trap->slice);
    (void)vec_reader_destroy(&trap->reader);
    trap->done.cb = done_cb;
    trap->done.arg = done_arg;
    trap->flags = 0;
    trap->heap_size = 0;
    trap->atom_extra_skip = 0;
    trap->terms = 0;
    trap->head = NULL;
    trap->tail = NULL;

    if (!vec_clone_slice(&trap->slice, slice)) {
        return EXCP_ERROR(env, "Call to vec_clone_slice() failed: unable to clone slice\n");
    }
    if (!vec_reader_create(&trap->reader, &trap->slice, 0)) {
        return EXCP_ERROR(env, "Call to vec_reader_create() failed: unable to read slice\n");
    }

    if (trapp != NULL) {
        *trapp = trap;
    }

    return trap_term;
}

void
etf_decode_term_length_trap_dtor(ErlNifEnv *caller_env, edf_trap_t *super, void *arg)
{
    etf_decode_term_length_trap_t *trap = (void *)arg;

    XNIF_TRACE_F("%s:%d [trap] dtor callback\n", __FILE__, __LINE__);

    (void)caller_env;
    (void)super;

    (void)vec_reader_destroy(&trap->reader);
    (void)vec_destroy(&trap->slice);

    return;
}

void
etf_decode_term_length_trap_edit(ErlNifEnv *caller_env, edf_trap_t *super, void *arg, edf_trap_result_t *result)
{
    etf_decode_term_length_trap_t *trap = (void *)arg;

    if (result->tag == EDF_TRAP_RESULT_TAG_OK && trap->state == ETF_DECODE_TERM_LENGTH_TRAP_STATE_DONE && trap->done.cb != NULL) {
        (void)trap->done.cb(caller_env, trap, trap->done.arg, result);
    }

    return;
}

edf_trap_result_t
etf_decode_term_length_trap_next(ErlNifEnv *caller_env, edf_trap_t *super, void *arg)
{
    etf_decode_term_length_trap_t *trap = (void *)arg;
    vec_reader_t *vr = &trap->reader;
    ERL_NIF_TERM err_term = THE_NON_VALUE;
    ERL_NIF_TERM *err_termp = &err_term;
    uint32_t *terms = &trap->terms;
    size_t *atom_extra_skip = &trap->atom_extra_skip;
    size_t n;

#define RAW_BYTES() (void *)(vec_reader_raw_bytes(vr))

/* Increment the number of terms that remain to decode
 * and check for the term counter wrapping around. */
#define ADDTERMS(sz)                                                                                                               \
    do {                                                                                                                           \
        uint32_t before = (*terms);                                                                                                \
        (*terms) += (sz);                                                                                                          \
        if ((*terms) < before) {                                                                                                   \
            *err_termp = EXCP_ERROR(caller_env, "Call to etf_decode_term_length() failed: more than 2^32 - 1 terms overflow\n");   \
            return TRAP_ERR(*err_termp);                                                                                           \
        }                                                                                                                          \
    } while (0)

#define CHKSIZE(sz)                                                                                                                \
    do {                                                                                                                           \
        if (vec_reader_remaining_bytes(vr) < (sz)) {                                                                               \
            *err_termp = EXCP_ERROR(caller_env, "Call to etf_decode_term_length() failed: check size failed\n");                   \
            return TRAP_ERR(*err_termp);                                                                                           \
        }                                                                                                                          \
    } while (0)

#define READ_U8(val)                                                                                                               \
    do {                                                                                                                           \
        if (!vec_reader_read_u8(vr, (val))) {                                                                                      \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_read_u8() failed: unable to decode term length\n");            \
            return TRAP_ERR(*err_termp);                                                                                           \
        }                                                                                                                          \
        TRAP_REDUCE(trap, 1);                                                                                                      \
    } while (0)

#define READ_U16(val)                                                                                                              \
    do {                                                                                                                           \
        if (!vec_reader_read_u16(vr, (val))) {                                                                                     \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_read_u16() failed: unable to decode term length\n");           \
            return TRAP_ERR(*err_termp);                                                                                           \
        }                                                                                                                          \
        TRAP_REDUCE(trap, 2);                                                                                                      \
    } while (0)

#define READ_U32(val)                                                                                                              \
    do {                                                                                                                           \
        if (!vec_reader_read_u32(vr, (val))) {                                                                                     \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_read_u32() failed: unable to decode term length\n");           \
            return TRAP_ERR(*err_termp);                                                                                           \
        }                                                                                                                          \
        TRAP_REDUCE(trap, 4);                                                                                                      \
    } while (0)

#define SKIP(sz)                                                                                                                   \
    do {                                                                                                                           \
        if (!vec_reader_skip_exact(vr, (sz))) {                                                                                    \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_skip_exact() failed: unable to decode term length\n");         \
            return TRAP_ERR(*err_termp);                                                                                           \
        }                                                                                                                          \
        TRAP_REDUCE(trap, (sz));                                                                                                   \
    } while (0)

    do {
        switch (trap->state) {
        case ETF_DECODE_TERM_LENGTH_TRAP_STATE_INIT: {
            trap->heap_size = 0;
            trap->atom_extra_skip = 0;
            trap->terms = 1;
            trap->head = RAW_BYTES();
            if (trap->is_external_term == true) {
                trap->state = ETF_DECODE_TERM_LENGTH_TRAP_STATE_VERSION_MAGIC;
            } else {
                trap->state = ETF_DECODE_TERM_LENGTH_TRAP_STATE_DECODE;
            }
            goto next_state;
        }
        case ETF_DECODE_TERM_LENGTH_TRAP_STATE_VERSION_MAGIC: {
            uint8_t version_magic;
            READ_U8(&version_magic);
            if (version_magic != VERSION_MAGIC) {
                err_term = EXCP_ERROR_F(
                    caller_env,
                    "Call to etf_decode_term_length() failed: expected version_magic=%u to be VERSION_MAGIC=%u for external term\n",
                    version_magic, VERSION_MAGIC);
                return TRAP_ERR(err_term);
            }
            trap->state = ETF_DECODE_TERM_LENGTH_TRAP_STATE_DECODE;
            goto next_state;
        }
        case ETF_DECODE_TERM_LENGTH_TRAP_STATE_DECODE: {
            while ((*terms) != 0) {
                uint8_t tag;
                READ_U8(&tag);
                switch (tag) {
                case SMALL_INTEGER_EXT: {
                    SKIP(1);
                    trap->heap_size += VTERM_SIZEOF_SMALL_INTEGER_EXT();
                    break;
                }
                case INTEGER_EXT: {
                    SKIP(4);
                    trap->heap_size += VTERM_SIZEOF_INTEGER_EXT();
                    break;
                }
                case FLOAT_EXT: {
                    SKIP(31);
                    trap->heap_size += VTERM_SIZEOF_FLOAT_EXT();
                    break;
                }
                case ATOM_EXT: {
                    uint16_t len;
                    READ_U16(&len);
                    n = (size_t)len;
                    if (n > MAX_ATOM_CHARACTERS) {
                        *err_termp = EXCP_ERROR_F(
                            caller_env,
                            "Call to etf_decode_term_length() failed: ATOM_EXT len=%u is greater than MAX_ATOM_CHARACTERS=%u\n", n,
                            MAX_ATOM_CHARACTERS);
                        return TRAP_ERR(*err_termp);
                    }
                    SKIP(n + (*atom_extra_skip));
                    (*atom_extra_skip) = 0;
                    trap->heap_size += VTERM_SIZEOF_ATOM_EXT();
                    break;
                }
                case SMALL_ATOM_EXT: {
                    uint8_t len;
                    READ_U8(&len);
                    n = (size_t)len;
                    if (n > MAX_ATOM_CHARACTERS) {
                        *err_termp = EXCP_ERROR_F(caller_env,
                                                  "Call to etf_decode_term_length() failed: SMALL_ATOM_EXT len=%u is greater than "
                                                  "MAX_ATOM_CHARACTERS=%u\n",
                                                  n, MAX_ATOM_CHARACTERS);
                        return TRAP_ERR(*err_termp);
                    }
                    SKIP(n + (*atom_extra_skip));
                    (*atom_extra_skip) = 0;
                    trap->heap_size += VTERM_SIZEOF_SMALL_ATOM_EXT();
                    break;
                }
                case REFERENCE_EXT: {
                    (*atom_extra_skip) = 5;
                    ADDTERMS(1);
                    trap->heap_size += VTERM_SIZEOF_REFERENCE_EXT();
                    break;
                }
                case NEW_REFERENCE_EXT: {
                    uint16_t len;
                    READ_U16(&len);
                    if (len > ERTS_MAX_REF_NUMBERS) {
                        *err_termp = EXCP_ERROR_F(caller_env,
                                                  "Call to etf_decode_term_length() failed: NEW_REFERENCE_EXT len=%u is greater "
                                                  "than ERTS_MAX_REF_NUMBERS=%u",
                                                  len, ERTS_MAX_REF_NUMBERS);
                        return TRAP_ERR(*err_termp);
                    }
                    (*atom_extra_skip) = 1 + (4 * len);
                    ADDTERMS(1);
                    trap->heap_size += VTERM_SIZEOF_NEW_REFERENCE_EXT(len);
                    break;
                }
                case NEWER_REFERENCE_EXT: {
                    uint16_t len;
                    READ_U16(&len);
                    if (len > ERTS_MAX_REF_NUMBERS) {
                        *err_termp = EXCP_ERROR_F(caller_env,
                                                  "Call to etf_decode_term_length() failed: NEWER_REFERENCE_EXT len=%u is greater "
                                                  "than ERTS_MAX_REF_NUMBERS=%u",
                                                  len, ERTS_MAX_REF_NUMBERS);
                        return TRAP_ERR(*err_termp);
                    }
                    (*atom_extra_skip) = 4 + (4 * len);
                    ADDTERMS(1);
                    trap->heap_size += VTERM_SIZEOF_NEWER_REFERENCE_EXT(len);
                    break;
                }
                case PORT_EXT: {
                    (*atom_extra_skip) = 4 + 1;
                    ADDTERMS(1);
                    trap->heap_size += VTERM_SIZEOF_PORT_EXT();
                    break;
                }
                case NEW_PORT_EXT: {
                    (*atom_extra_skip) = 4 + 4;
                    ADDTERMS(1);
                    trap->heap_size += VTERM_SIZEOF_NEW_PORT_EXT();
                    break;
                }
                case NEW_FLOAT_EXT: {
                    SKIP(8);
                    trap->heap_size += VTERM_SIZEOF_NEW_FLOAT_EXT();
                    break;
                }
                case PID_EXT: {
                    (*atom_extra_skip) = 4 + 4 + 1;
                    ADDTERMS(1);
                    trap->heap_size += VTERM_SIZEOF_PID_EXT();
                    break;
                }
                case NEW_PID_EXT: {
                    (*atom_extra_skip) = 4 + 4 + 4;
                    ADDTERMS(1);
                    trap->heap_size += VTERM_SIZEOF_NEW_PID_EXT();
                    break;
                }
                case SMALL_TUPLE_EXT: {
                    uint8_t arity;
                    READ_U8(&arity);
                    n = (size_t)arity;
                    CHKSIZE(n);
                    ADDTERMS((uint32_t)arity);
                    trap->heap_size += VTERM_SIZEOF_SMALL_TUPLE_EXT(arity);
                    break;
                }
                case LARGE_TUPLE_EXT: {
                    uint32_t arity;
                    READ_U32(&arity);
                    n = (size_t)arity;
                    CHKSIZE(n);
                    ADDTERMS(arity);
                    trap->heap_size += VTERM_SIZEOF_LARGE_TUPLE_EXT(arity);
                    break;
                }
                case NIL_EXT: {
                    trap->heap_size += VTERM_SIZEOF_NIL_EXT();
                    break;
                }
                case STRING_EXT: {
                    uint16_t len;
                    READ_U16(&len);
                    n = (size_t)len;
                    SKIP(n);
                    trap->heap_size += VTERM_SIZEOF_STRING_EXT();
                    break;
                }
                case LIST_EXT: {
                    uint32_t len;
                    READ_U32(&len);
                    n = (size_t)len;
                    CHKSIZE(n);
                    /* Count terms in two operations to avoid overflow. */
                    ADDTERMS(len);
                    ADDTERMS(1);
                    trap->heap_size += VTERM_SIZEOF_LIST_EXT(len);
                    break;
                }
                case BINARY_EXT: {
                    uint32_t len;
                    READ_U32(&len);
                    n = (size_t)len;
                    SKIP(n);
                    trap->heap_size += VTERM_SIZEOF_BINARY_EXT();
                    break;
                }
                case BIT_BINARY_EXT: {
                    uint32_t len;
                    READ_U32(&len);
                    n = (size_t)len;
                    SKIP(1);
                    SKIP(n);
                    trap->heap_size += VTERM_SIZEOF_BIT_BINARY_EXT();
                    break;
                }
                case SMALL_BIG_EXT: {
                    uint8_t len;
                    READ_U8(&len);
                    n = (size_t)len;
                    SKIP(1);
                    SKIP(n);
                    trap->heap_size += VTERM_SIZEOF_SMALL_BIG_EXT();
                    break;
                }
                case LARGE_BIG_EXT: {
                    uint32_t len;
                    READ_U32(&len);
                    n = (size_t)len;
                    SKIP(1);
                    SKIP(n);
                    trap->heap_size += VTERM_SIZEOF_LARGE_BIG_EXT();
                    break;
                }
                case NEW_FUN_EXT: {
                    uint32_t num_free;
                    /* Ignore faulty Size field as we have bugs since OTP 23.0
                     * that can encode it too large if fun contains EXPORT_EXT or
                     * BIT_BINARY_EXT hopefully encoded for pending connection.
                     */
                    SKIP(4 + 1 + 16 + 4);
                    READ_U32(&num_free);
                    if (num_free > MAX_ARG) {
                        *err_termp = EXCP_ERROR_F(
                            caller_env,
                            "Call to etf_decode_term_length() failed: NEW_FUN_EXT num_free=%u is greater than MAX_ARG=%u\n",
                            num_free, MAX_ARG);
                        return TRAP_ERR(*err_termp);
                    }
                    /* Count terms in two operations to avoid overflow. */
                    ADDTERMS(4);
                    ADDTERMS(num_free);
                    trap->flags |= ETF_DECODE_TERM_LENGTH_FLAG_HAS_NEW_FUN_EXT;
                    trap->heap_size += VTERM_SIZEOF_NEW_FUN_EXT(num_free);
                    break;
                }
                case EXPORT_EXT: {
                    ADDTERMS(3);
                    trap->flags |= ETF_DECODE_TERM_LENGTH_FLAG_HAS_EXPORT_EXT;
                    trap->heap_size += VTERM_SIZEOF_EXPORT_EXT();
                    break;
                }
                case MAP_EXT: {
                    uint32_t arity;
                    READ_U32(&arity);
                    n = (size_t)arity;
                    if (n <= MAP_SMALL_MAP_LIMIT) {
                        CHKSIZE(n * 2);
#if defined(ARCH_WORD_BITS_64)
                    } else if ((n >> 31) != 0) {
                        /* Avoid overflow by limiting the number of elements in
                         * a map to 2^31-1 (about 2 billions). */
                        *err_termp = EXCP_ERROR_F(
                            caller_env, "Call to etf_decode_term_length() failed: MAP_EXT arity=%u is greater than 2^31 - 1\n",
                            arity);
                        return TRAP_ERR(*err_termp);
#else
                    } else if ((n >> 30) != 0) {
                        /* Can't possibly fit in memory on 32-bit machine. */
                        *err_termp = EXCP_ERROR_F(
                            caller_env, "Call to etf_decode_term_length() failed: MAP_EXT arity=%u cannot fit on 32-bit machine\n",
                            arity);
                        return TRAP_ERR(*err_termp);
#endif
                    } else {
                        CHKSIZE(n * 2);
                    }
                    /* Count terms in two operations to avoid overflow. */
                    ADDTERMS(arity);
                    ADDTERMS(arity);
                    trap->heap_size += VTERM_SIZEOF_MAP_EXT(arity);
                    break;
                }
                case ATOM_UTF8_EXT: {
                    uint16_t len;
                    READ_U16(&len);
                    n = (size_t)len;
                    if (n > MAX_ATOM_SZ_LIMIT) {
                        *err_termp = EXCP_ERROR_F(
                            caller_env,
                            "Call to etf_decode_term_length() failed: ATOM_UTF8_EXT len=%u is greater than MAX_ATOM_SZ_LIMIT=%u\n",
                            n, MAX_ATOM_SZ_LIMIT);
                        return TRAP_ERR(*err_termp);
                    }
                    SKIP(n + (*atom_extra_skip));
                    (*atom_extra_skip) = 0;
                    trap->heap_size += VTERM_SIZEOF_ATOM_UTF8_EXT();
                    break;
                }
                case SMALL_ATOM_UTF8_EXT: {
                    uint8_t len;
                    READ_U8(&len);
                    n = (size_t)len;
                    if (n > MAX_ATOM_SZ_LIMIT) {
                        *err_termp = EXCP_ERROR_F(caller_env,
                                                  "Call to etf_decode_term_length() failed: SMALL_ATOM_UTF8_EXT len=%u is greater "
                                                  "than MAX_ATOM_SZ_LIMIT=%u\n",
                                                  n, MAX_ATOM_SZ_LIMIT);
                        return TRAP_ERR(*err_termp);
                    }
                    SKIP(n + (*atom_extra_skip));
                    (*atom_extra_skip) = 0;
                    trap->heap_size += VTERM_SIZEOF_SMALL_ATOM_UTF8_EXT();
                    break;
                }
                case V4_PORT_EXT: {
                    (*atom_extra_skip) = 8 + 4;
                    ADDTERMS(1);
                    trap->heap_size += VTERM_SIZEOF_V4_PORT_EXT();
                    break;
                }
                case ATOM_CACHE_REF: {
                    SKIP(1 + (*atom_extra_skip));
                    (*atom_extra_skip) = 0;
                    trap->heap_size += VTERM_SIZEOF_ATOM_CACHE_REF_RESOLVED();
                    break;
                }
                default:
                    *err_termp = EXCP_ERROR_F(caller_env, "Call to etf_decode_term_length() failed: unknown term tag=%u\n", tag);
                    return TRAP_ERR(*err_termp);
                }
                (*terms) -= 1;
                if (TRAP_SHOULD_YIELD(trap)) {
                    return TRAP_YIELD();
                }
            }

            trap->state = ETF_DECODE_TERM_LENGTH_TRAP_STATE_DONE;
            goto next_state;
        }
        case ETF_DECODE_TERM_LENGTH_TRAP_STATE_DONE: {
            trap->tail = RAW_BYTES();
            return TRAP_OK(THE_NON_VALUE);
        }
        default:
            return TRAP_ERR(EXCP_ERROR_F(caller_env, "Fatal error: unknown etf_decode_term_length_trap_t->state value %d\n",
                                         (int)(trap->state)));
        }
    next_state: {
        if (TRAP_SHOULD_YIELD(trap)) {
            return TRAP_YIELD();
        }
        continue;
    }
    } while (1);

#undef SKIP
#undef READ_U32
#undef READ_U16
#undef READ_U8
#undef CHKSIZE
#undef ADDTERMS
#undef RAW_BYTES
}

int
etf_fast_skip_terms(ErlNifEnv *caller_env, bool is_external_term, vec_reader_t *vr, uint32_t skip, ERL_NIF_TERM *err_termp)
{
    uint32_t terms[1] = {skip};
    size_t atom_extra_skip[1] = {0};
    size_t n;

/* Increment the number of terms that remain to decode
 * and check for the term counter wrapping around. */
#define ADDTERMS(sz)                                                                                                               \
    do {                                                                                                                           \
        uint32_t before = (*terms);                                                                                                \
        (*terms) += (sz);                                                                                                          \
        if ((*terms) < before) {                                                                                                   \
            *err_termp = EXCP_ERROR(caller_env, "Call to etf_decode_term_length() failed: more than 2^32 - 1 terms overflow\n");   \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define CHKSIZE(sz)                                                                                                                \
    do {                                                                                                                           \
        if (vec_reader_remaining_bytes(vr) < (sz)) {                                                                               \
            *err_termp = EXCP_ERROR(caller_env, "Call to etf_decode_term_length() failed: check size failed\n");                   \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define READ_U8(val)                                                                                                               \
    do {                                                                                                                           \
        if (!vec_reader_read_u8(vr, (val))) {                                                                                      \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_read_u8() failed: unable to decode term length\n");            \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define READ_U16(val)                                                                                                              \
    do {                                                                                                                           \
        if (!vec_reader_read_u16(vr, (val))) {                                                                                     \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_read_u16() failed: unable to decode term length\n");           \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define READ_U32(val)                                                                                                              \
    do {                                                                                                                           \
        if (!vec_reader_read_u32(vr, (val))) {                                                                                     \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_read_u32() failed: unable to decode term length\n");           \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define SKIP(sz)                                                                                                                   \
    do {                                                                                                                           \
        if (!vec_reader_skip_exact(vr, (sz))) {                                                                                    \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_skip_exact() failed: unable to decode term length\n");         \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

    if (is_external_term) {
        uint8_t version_magic;
        READ_U8(&version_magic);
        if (version_magic != VERSION_MAGIC) {
            *err_termp = EXCP_ERROR_F(
                caller_env,
                "Call to etf_fast_skip_terms() failed: expected version_magic=%u to be VERSION_MAGIC=%u for external term\n",
                version_magic, VERSION_MAGIC);
            return 0;
        }
    }

    while ((*terms) != 0) {
        uint8_t tag;
        READ_U8(&tag);
        switch (tag) {
        case SMALL_INTEGER_EXT: {
            SKIP(1);
            break;
        }
        case INTEGER_EXT: {
            SKIP(4);
            break;
        }
        case FLOAT_EXT: {
            SKIP(31);
            break;
        }
        case ATOM_EXT: {
            uint16_t len;
            READ_U16(&len);
            n = (size_t)len;
            if (n > MAX_ATOM_CHARACTERS) {
                *err_termp = EXCP_ERROR_F(
                    caller_env, "Call to etf_decode_term_length() failed: ATOM_EXT len=%u is greater than MAX_ATOM_CHARACTERS=%u\n",
                    n, MAX_ATOM_CHARACTERS);
                return 0;
            }
            SKIP(n + (*atom_extra_skip));
            (*atom_extra_skip) = 0;
            break;
        }
        case SMALL_ATOM_EXT: {
            uint8_t len;
            READ_U8(&len);
            n = (size_t)len;
            if (n > MAX_ATOM_CHARACTERS) {
                *err_termp = EXCP_ERROR_F(
                    caller_env,
                    "Call to etf_decode_term_length() failed: SMALL_ATOM_EXT len=%u is greater than MAX_ATOM_CHARACTERS=%u\n", n,
                    MAX_ATOM_CHARACTERS);
                return 0;
            }
            SKIP(n + (*atom_extra_skip));
            (*atom_extra_skip) = 0;
            break;
        }
        case REFERENCE_EXT: {
            (*atom_extra_skip) = 5;
            ADDTERMS(1);
            break;
        }
        case NEW_REFERENCE_EXT: {
            uint16_t len;
            READ_U16(&len);
            if (len > ERTS_MAX_REF_NUMBERS) {
                *err_termp = EXCP_ERROR_F(
                    caller_env,
                    "Call to etf_decode_term_length() failed: NEW_REFERENCE_EXT len=%u is greater than ERTS_MAX_REF_NUMBERS=%u",
                    len, ERTS_MAX_REF_NUMBERS);
                return 0;
            }
            (*atom_extra_skip) = 1 + (4 * len);
            ADDTERMS(1);
            break;
        }
        case NEWER_REFERENCE_EXT: {
            uint16_t len;
            READ_U16(&len);
            if (len > ERTS_MAX_REF_NUMBERS) {
                *err_termp = EXCP_ERROR_F(
                    caller_env,
                    "Call to etf_decode_term_length() failed: NEWER_REFERENCE_EXT len=%u is greater than ERTS_MAX_REF_NUMBERS=%u",
                    len, ERTS_MAX_REF_NUMBERS);
                return 0;
            }
            (*atom_extra_skip) = 4 + (4 * len);
            ADDTERMS(1);
            break;
        }
        case PORT_EXT: {
            (*atom_extra_skip) = 4 + 1;
            ADDTERMS(1);
            break;
        }
        case NEW_PORT_EXT: {
            (*atom_extra_skip) = 4 + 4;
            ADDTERMS(1);
            break;
        }
        case NEW_FLOAT_EXT: {
            SKIP(8);
            break;
        }
        case PID_EXT: {
            (*atom_extra_skip) = 4 + 4 + 1;
            ADDTERMS(1);
            break;
        }
        case NEW_PID_EXT: {
            (*atom_extra_skip) = 4 + 4 + 4;
            ADDTERMS(1);
            break;
        }
        case SMALL_TUPLE_EXT: {
            uint8_t arity;
            READ_U8(&arity);
            n = (size_t)arity;
            CHKSIZE(n);
            ADDTERMS((uint32_t)arity);
            break;
        }
        case LARGE_TUPLE_EXT: {
            uint32_t arity;
            READ_U32(&arity);
            n = (size_t)arity;
            CHKSIZE(n);
            ADDTERMS(arity);
            break;
        }
        case NIL_EXT: {
            break;
        }
        case STRING_EXT: {
            uint16_t len;
            READ_U16(&len);
            n = (size_t)len;
            SKIP(n);
            break;
        }
        case LIST_EXT: {
            uint32_t len;
            READ_U32(&len);
            n = (size_t)len;
            CHKSIZE(n);
            /* Count terms in two operations to avoid overflow. */
            ADDTERMS(len);
            ADDTERMS(1);
            break;
        }
        case BINARY_EXT: {
            uint32_t len;
            READ_U32(&len);
            n = (size_t)len;
            SKIP(n);
            break;
        }
        case BIT_BINARY_EXT: {
            uint32_t len;
            READ_U32(&len);
            n = (size_t)len;
            SKIP(1);
            SKIP(n);
            break;
        }
        case SMALL_BIG_EXT: {
            uint8_t len;
            READ_U8(&len);
            n = (size_t)len;
            SKIP(1);
            SKIP(n);
            break;
        }
        case LARGE_BIG_EXT: {
            uint32_t len;
            READ_U32(&len);
            n = (size_t)len;
            SKIP(1);
            SKIP(n);
            break;
        }
        case NEW_FUN_EXT: {
            uint32_t num_free;
            /* Ignore faulty Size field as we have bugs since OTP 23.0
             * that can encode it too large if fun contains EXPORT_EXT or
             * BIT_BINARY_EXT hopefully encoded for pending connection.
             */
            SKIP(4 + 1 + 16 + 4);
            READ_U32(&num_free);
            if (num_free > MAX_ARG) {
                *err_termp = EXCP_ERROR_F(
                    caller_env, "Call to etf_decode_term_length() failed: NEW_FUN_EXT num_free=%u is greater than MAX_ARG=%u\n",
                    num_free, MAX_ARG);
                return 0;
            }
            /* Count terms in two operations to avoid overflow. */
            ADDTERMS(4);
            ADDTERMS(num_free);
            break;
        }
        case EXPORT_EXT: {
            ADDTERMS(3);
            break;
        }
        case MAP_EXT: {
            uint32_t arity;
            READ_U32(&arity);
            n = (size_t)arity;
            if (n <= MAP_SMALL_MAP_LIMIT) {
                CHKSIZE(n * 2);
#if defined(ARCH_WORD_BITS_64)
            } else if ((n >> 31) != 0) {
                /* Avoid overflow by limiting the number of elements in
                 * a map to 2^31-1 (about 2 billions). */
                *err_termp = EXCP_ERROR_F(
                    caller_env, "Call to etf_decode_term_length() failed: MAP_EXT arity=%u is greater than 2^31 - 1\n", arity);
                return 0;
#else
            } else if ((n >> 30) != 0) {
                /* Can't possibly fit in memory on 32-bit machine. */
                *err_termp = EXCP_ERROR_F(
                    caller_env, "Call to etf_decode_term_length() failed: MAP_EXT arity=%u cannot fit on 32-bit machine\n", arity);
                return 0;
#endif
            } else {
                CHKSIZE(n * 2);
            }
            /* Count terms in two operations to avoid overflow. */
            ADDTERMS(arity);
            ADDTERMS(arity);
            break;
        }
        case ATOM_UTF8_EXT: {
            uint16_t len;
            READ_U16(&len);
            n = (size_t)len;
            if (n > MAX_ATOM_SZ_LIMIT) {
                *err_termp = EXCP_ERROR_F(
                    caller_env,
                    "Call to etf_decode_term_length() failed: ATOM_UTF8_EXT len=%u is greater than MAX_ATOM_SZ_LIMIT=%u\n", n,
                    MAX_ATOM_SZ_LIMIT);
                return 0;
            }
            SKIP(n + (*atom_extra_skip));
            (*atom_extra_skip) = 0;
            break;
        }
        case SMALL_ATOM_UTF8_EXT: {
            uint8_t len;
            READ_U8(&len);
            n = (size_t)len;
            if (n > MAX_ATOM_SZ_LIMIT) {
                *err_termp = EXCP_ERROR_F(
                    caller_env,
                    "Call to etf_decode_term_length() failed: SMALL_ATOM_UTF8_EXT len=%u is greater than MAX_ATOM_SZ_LIMIT=%u\n", n,
                    MAX_ATOM_SZ_LIMIT);
                return 0;
            }
            SKIP(n + (*atom_extra_skip));
            (*atom_extra_skip) = 0;
            break;
        }
        case V4_PORT_EXT: {
            (*atom_extra_skip) = 8 + 4;
            ADDTERMS(1);
            break;
        }
        case ATOM_CACHE_REF: {
            SKIP(1 + (*atom_extra_skip));
            (*atom_extra_skip) = 0;
            break;
        }
        default:
            *err_termp = EXCP_ERROR_F(caller_env, "Call to etf_fast_skip_terms() failed: unknown term tag=%u", tag);
            return 0;
        }
        (*terms) -= 1;
    }

    return 1;

#undef SKIP
#undef READ_U32
#undef READ_U16
#undef READ_U8
#undef CHKSIZE
#undef ADDTERMS
}
