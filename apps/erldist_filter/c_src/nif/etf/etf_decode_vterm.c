/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "etf_decode_vterm.h"
#include "etf_decode_term_length.h"

#include "../core/xnif_trace.h"

static void etf_decode_vterm_trap_dtor(ErlNifEnv *caller_env, edf_trap_t *super, void *arg);
static void etf_decode_vterm_trap_edit(ErlNifEnv *caller_env, edf_trap_t *super, void *arg, edf_trap_result_t *result);
static edf_trap_result_t etf_decode_vterm_trap_next(ErlNifEnv *caller_env, edf_trap_t *super, void *arg);
static int etf_decode_vterm_atom(ErlNifEnv *caller_env, etf_decode_vterm_trap_t *trap, vterm_t *objp, ERL_NIF_TERM *err_termp);
static int etf_decode_vterm_fixed_integer(ErlNifEnv *caller_env, etf_decode_vterm_trap_t *trap, vterm_t *objp,
                                          ERL_NIF_TERM *err_termp);
static int etf_decode_vterm_pid(ErlNifEnv *caller_env, etf_decode_vterm_trap_t *trap, vterm_t *objp, ERL_NIF_TERM *err_termp);
static void decode_term_length_callback(ErlNifEnv *caller_env, etf_decode_term_length_trap_t *child, void *arg,
                                        edf_trap_result_t *result);

ERL_NIF_TERM
etf_decode_vterm_trap_open(ErlNifEnv *env, vterm_env_t *vtenv, bool is_external_term, edf_atom_translation_table_t *attab,
                           vec_t *slice, ssize_t limit, etf_decode_vterm_done_t done_cb, void *done_arg,
                           etf_decode_vterm_trap_t **trapp)
{
    edf_trap_state_t trap_state = {
        .resource = NULL,
        .acquire = NULL,
        .release = NULL,
        .dtor = etf_decode_vterm_trap_dtor,
        .edit = etf_decode_vterm_trap_edit,
        .next = etf_decode_vterm_trap_next,
        .arg = NULL,
    };
    etf_decode_vterm_trap_t *trap = NULL;
    ERL_NIF_TERM trap_term;

    if (vtenv == NULL) {
        return EXCP_ERROR(env, "Call to etf_decode_vterm_trap_open() failed: vtenv not be NULL\n");
    }
    if (!vec_is_slice(slice)) {
        return EXCP_ERROR(env, "Call to etf_decode_vterm_trap_open() failed: slice must be a vec_t with tag=VEC_TAG_SLICE\n");
    }

    trap_term = edf_trap_open(env, &trap_state, sizeof(etf_decode_vterm_trap_t), (edf_trap_t **)(&trap));
    if (trap == NULL) {
        return trap_term;
    }

    trap->super.state.arg = (void *)trap;
    trap->state = ETF_DECODE_VTERM_TRAP_STATE_INIT;
    trap->is_external_term = is_external_term;
    trap->attab = attab;
    (void)vec_init_free(&trap->slice);
    (void)vec_reader_destroy(&trap->reader);
    trap->limit = limit;
    trap->done.cb = done_cb;
    trap->done.arg = done_arg;
    trap->vtenv = vtenv;
    trap->vterm = NULL;
    trap->objp = NULL;
    trap->next = NULL;
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

edf_trap_result_t
etf_decode_vterm_blocking(ErlNifEnv *caller_env, vterm_env_t *vtenv, bool is_external_term, edf_atom_translation_table_t *attab,
                          vec_t *slice, ssize_t limit, etf_decode_vterm_done_t done_cb, void *done_arg)
{
    etf_decode_vterm_trap_t *trap = NULL;
    ERL_NIF_TERM trap_term;
    edf_trap_result_t trap_result;
    trap_term = etf_decode_vterm_trap_open(caller_env, vtenv, is_external_term, attab, slice, limit, done_cb, done_arg, &trap);
    if (trap == NULL || enif_is_exception(caller_env, trap_term)) {
        return TRAP_ERR(trap_term);
    }
    trap_result = edf_trap_block_on_next(caller_env, &trap->super);
    return trap_result;
}

void
etf_decode_vterm_trap_dtor(ErlNifEnv *caller_env, edf_trap_t *super, void *arg)
{
    etf_decode_vterm_trap_t *trap = (void *)arg;

    XNIF_TRACE_F("%s:%d [trap] dtor callback\n", __FILE__, __LINE__);

    (void)caller_env;
    (void)super;

    (void)vec_reader_destroy(&trap->reader);
    (void)vec_destroy(&trap->slice);

    return;
}

void
etf_decode_vterm_trap_edit(ErlNifEnv *caller_env, edf_trap_t *super, void *arg, edf_trap_result_t *result)
{
    etf_decode_vterm_trap_t *trap = (void *)arg;

    if (result->tag == EDF_TRAP_RESULT_TAG_OK && trap->state == ETF_DECODE_VTERM_TRAP_STATE_DONE && trap->done.cb != NULL) {
        (void)trap->done.cb(caller_env, trap, trap->done.arg, result);
    }

    return;
}

edf_trap_result_t
etf_decode_vterm_trap_next(ErlNifEnv *caller_env, edf_trap_t *super, void *arg)
{
    etf_decode_vterm_trap_t *trap = (void *)arg;
    vterm_env_t *vtenv = trap->vtenv;
    vec_reader_t *vr = &trap->reader;
    edf_atom_translation_table_t *attab = trap->attab;
    ERL_NIF_TERM err_term = THE_NON_VALUE;
    ERL_NIF_TERM *err_termp = &err_term;
    size_t n;

#define RAW_BYTES() (void *)(vec_reader_raw_bytes(vr))

#define CHKSIZE(sz)                                                                                                                \
    do {                                                                                                                           \
        if (vec_reader_remaining_bytes(vr) < (sz)) {                                                                               \
            *err_termp = EXCP_ERROR(caller_env, "Call to etf_decode_vterm() failed: check size failed\n");                         \
            return TRAP_ERR(*err_termp);                                                                                           \
        }                                                                                                                          \
    } while (0)

#define READ_U8(val)                                                                                                               \
    do {                                                                                                                           \
        if (!vec_reader_read_u8(vr, (val))) {                                                                                      \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_read_u8() failed: unable to decode vterm\n");                  \
            return TRAP_ERR(*err_termp);                                                                                           \
        }                                                                                                                          \
        TRAP_REDUCE(trap, 1);                                                                                                      \
    } while (0)

#define READ_U16(val)                                                                                                              \
    do {                                                                                                                           \
        if (!vec_reader_read_u16(vr, (val))) {                                                                                     \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_read_u16() failed: unable to decode vterm\n");                 \
            return TRAP_ERR(*err_termp);                                                                                           \
        }                                                                                                                          \
        TRAP_REDUCE(trap, 2);                                                                                                      \
    } while (0)

#define READ_I32(val)                                                                                                              \
    do {                                                                                                                           \
        if (!vec_reader_read_i32(vr, (val))) {                                                                                     \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_read_i32() failed: unable to decode vterm\n");                 \
            return TRAP_ERR(*err_termp);                                                                                           \
        }                                                                                                                          \
        TRAP_REDUCE(trap, 4);                                                                                                      \
    } while (0)

#define READ_U32(val)                                                                                                              \
    do {                                                                                                                           \
        if (!vec_reader_read_u32(vr, (val))) {                                                                                     \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_read_u32() failed: unable to decode vterm\n");                 \
            return TRAP_ERR(*err_termp);                                                                                           \
        }                                                                                                                          \
        TRAP_REDUCE(trap, 4);                                                                                                      \
    } while (0)

#define READ_U64(val)                                                                                                              \
    do {                                                                                                                           \
        if (!vec_reader_read_u64(vr, (val))) {                                                                                     \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_read_u64() failed: unable to decode vterm\n");                 \
            return TRAP_ERR(*err_termp);                                                                                           \
        }                                                                                                                          \
        TRAP_REDUCE(trap, 8);                                                                                                      \
    } while (0)

#define BACK(sz)                                                                                                                   \
    do {                                                                                                                           \
        if (!vec_reader_back_exact(vr, (sz))) {                                                                                    \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_back_exact() failed: unable to decode vterm\n");               \
            return TRAP_ERR(*err_termp);                                                                                           \
        }                                                                                                                          \
    } while (0)

#define SKIP(sz)                                                                                                                   \
    do {                                                                                                                           \
        if (!vec_reader_skip_exact(vr, (sz))) {                                                                                    \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_skip_exact() failed: unable to decode vterm\n");               \
            return TRAP_ERR(*err_termp);                                                                                           \
        }                                                                                                                          \
        TRAP_REDUCE(trap, (sz));                                                                                                   \
    } while (0)

#define READ_ATOM(vtptr)                                                                                                           \
    do {                                                                                                                           \
        if (!etf_decode_vterm_atom(caller_env, trap, (vtptr), err_termp)) {                                                        \
            return TRAP_ERR(*err_termp);                                                                                           \
        }                                                                                                                          \
    } while (0)

#define READ_FIXED_INTEGER(vtptr)                                                                                                  \
    do {                                                                                                                           \
        if (!etf_decode_vterm_fixed_integer(caller_env, trap, (vtptr), err_termp)) {                                               \
            return TRAP_ERR(*err_termp);                                                                                           \
        }                                                                                                                          \
    } while (0)

#define READ_PID(vtptr)                                                                                                            \
    do {                                                                                                                           \
        if (!etf_decode_vterm_pid(caller_env, trap, (vtptr), err_termp)) {                                                         \
            return TRAP_ERR(*err_termp);                                                                                           \
        }                                                                                                                          \
    } while (0)

    do {
        switch (trap->state) {
        case ETF_DECODE_VTERM_TRAP_STATE_INIT: {
            trap->objp = &trap->vterm;
            trap->next = trap->objp;
            *(trap->next) = NULL;
            if (trap->is_external_term == true) {
                trap->state = ETF_DECODE_VTERM_TRAP_STATE_VERSION_MAGIC;
            } else {
                trap->state = ETF_DECODE_VTERM_TRAP_STATE_DECODE;
            }
            goto next_state;
        }
        case ETF_DECODE_VTERM_TRAP_STATE_VERSION_MAGIC: {
            uint8_t version_magic;
            READ_U8(&version_magic);
            if (version_magic != VERSION_MAGIC) {
                err_term = EXCP_ERROR_F(
                    caller_env,
                    "Call to etf_decode_vterm() failed: expected version_magic=%u to be VERSION_MAGIC=%u for external term\n",
                    version_magic, VERSION_MAGIC);
                return TRAP_ERR(err_term);
            }
            trap->state = ETF_DECODE_VTERM_TRAP_STATE_DECODE;
            goto next_state;
        }
        case ETF_DECODE_VTERM_TRAP_STATE_DECODE: {
            vterm_t *objp = trap->objp;
            vterm_t *next = trap->next;
            while (next != NULL) {
                uint8_t tag;
                objp = next;
                next = (vterm_t *)(*(objp));
                READ_U8(&tag);
                switch (tag) {
                case SMALL_INTEGER_EXT: {
                    uint8_t value;
                    READ_U8(&value);
                    *objp = vterm_make_small_integer_ext(vtenv, value);
                    break;
                }
                case INTEGER_EXT: {
                    int32_t value;
                    READ_I32(&value);
                    *objp = vterm_make_integer_ext(vtenv, value);
                    break;
                }
                case FLOAT_EXT: {
                    uint8_t *float_string = NULL;
                    float_string = RAW_BYTES();
                    SKIP(31);
                    *objp = vterm_make_float_ext(vtenv, float_string);
                    break;
                }
                case ATOM_EXT: {
                    uint16_t len;
                    uint8_t *name = NULL;
                    READ_U16(&len);
                    n = (size_t)len;
                    if (n > MAX_ATOM_CHARACTERS) {
                        *err_termp = EXCP_ERROR_F(
                            caller_env,
                            "Call to etf_decode_vterm() failed: ATOM_EXT len=%u is greater than MAX_ATOM_CHARACTERS=%u\n", n,
                            MAX_ATOM_CHARACTERS);
                        return TRAP_ERR(*err_termp);
                    }
                    name = RAW_BYTES();
                    SKIP(n);
                    *objp = vterm_make_atom_ext(vtenv, len, name);
                    break;
                }
                case SMALL_ATOM_EXT: {
                    uint8_t len;
                    uint8_t *name = NULL;
                    READ_U8(&len);
                    n = (size_t)len;
                    if (n > MAX_ATOM_CHARACTERS) {
                        *err_termp = EXCP_ERROR_F(
                            caller_env,
                            "Call to etf_decode_vterm() failed: SMALL_ATOM_EXT len=%u is greater than MAX_ATOM_CHARACTERS=%u\n", n,
                            MAX_ATOM_CHARACTERS);
                        return TRAP_ERR(*err_termp);
                    }
                    name = RAW_BYTES();
                    SKIP(n);
                    *objp = vterm_make_small_atom_ext(vtenv, len, name);
                    break;
                }
                case REFERENCE_EXT: {
                    vterm_t node = NULL;
                    uint32_t id;
                    uint8_t creation;
                    READ_ATOM(&node);
                    READ_U32(&id);
                    READ_U8(&creation);
                    *objp = vterm_make_reference_ext(vtenv, node, id, creation);
                    break;
                }
                case NEW_REFERENCE_EXT: {
                    uint16_t len;
                    vterm_t node = NULL;
                    uint8_t creation;
                    uint32_t *ids = NULL;
                    size_t i;
                    READ_U16(&len);
                    READ_ATOM(&node);
                    READ_U8(&creation);
                    n = (size_t)len;
                    CHKSIZE(sizeof(uint32_t) * n);
                    *objp = vterm_make_new_reference_ext(vtenv, len, node, creation, &ids);
                    for (i = 0; i < n; i++) {
                        READ_U32(&ids[i]);
                    }
                    break;
                }
                case NEWER_REFERENCE_EXT: {
                    uint16_t len;
                    vterm_t node = NULL;
                    uint32_t creation;
                    uint32_t *ids = NULL;
                    size_t i;
                    READ_U16(&len);
                    READ_ATOM(&node);
                    READ_U32(&creation);
                    n = (size_t)len;
                    CHKSIZE(sizeof(uint32_t) * n);
                    *objp = vterm_make_newer_reference_ext(vtenv, len, node, creation, &ids);
                    for (i = 0; i < n; i++) {
                        READ_U32(&ids[i]);
                    }
                    break;
                }
                case PORT_EXT: {
                    vterm_t node = NULL;
                    uint32_t id;
                    uint8_t creation;
                    READ_ATOM(&node);
                    READ_U32(&id);
                    READ_U8(&creation);
                    *objp = vterm_make_port_ext(vtenv, node, id, creation);
                    break;
                }
                case NEW_PORT_EXT: {
                    vterm_t node = NULL;
                    uint32_t id;
                    uint32_t creation;
                    READ_ATOM(&node);
                    READ_U32(&id);
                    READ_U32(&creation);
                    *objp = vterm_make_new_port_ext(vtenv, node, id, creation);
                    break;
                }
                case NEW_FLOAT_EXT: {
                    uint8_t *ieee_float = NULL;
                    ieee_float = RAW_BYTES();
                    SKIP(8);
                    *objp = vterm_make_new_float_ext(vtenv, ieee_float);
                    break;
                }
                case PID_EXT: {
                    vterm_t node = NULL;
                    uint32_t id;
                    uint32_t serial;
                    uint8_t creation;
                    READ_ATOM(&node);
                    READ_U32(&id);
                    READ_U32(&serial);
                    READ_U8(&creation);
                    *objp = vterm_make_pid_ext(vtenv, node, id, serial, creation);
                    break;
                }
                case NEW_PID_EXT: {
                    vterm_t node = NULL;
                    uint32_t id;
                    uint32_t serial;
                    uint32_t creation;
                    READ_ATOM(&node);
                    READ_U32(&id);
                    READ_U32(&serial);
                    READ_U32(&creation);
                    *objp = vterm_make_new_pid_ext(vtenv, node, id, serial, creation);
                    break;
                }
                case SMALL_TUPLE_EXT: {
                    uint8_t arity;
                    vterm_t *elements = NULL;
                    READ_U8(&arity);
                    n = (size_t)arity;
                    CHKSIZE(n);
                    if (trap->limit == 0 && n > 0) {
                        BACK(1 + 1);
                        trap->objp = objp;
                        trap->next = next;
                        trap->state = ETF_DECODE_VTERM_TRAP_STATE_DECODE_SKIP;
                        goto next_state;
                    }
                    *objp = vterm_make_small_tuple_ext(vtenv, arity, &elements);
                    if (n == 0) {
                        break;
                    }
                    objp = &elements[n - 1];
                    while (n-- > 0) {
                        objp[0] = (vterm_t)(next);
                        next = objp;
                        objp -= 1;
                    }
                    break;
                }
                case LARGE_TUPLE_EXT: {
                    uint32_t arity;
                    vterm_t *elements = NULL;
                    READ_U32(&arity);
                    n = (size_t)arity;
                    CHKSIZE(n);
                    if (trap->limit == 0 && n > 0) {
                        BACK(1 + 4);
                        trap->objp = objp;
                        trap->next = next;
                        trap->state = ETF_DECODE_VTERM_TRAP_STATE_DECODE_SKIP;
                        goto next_state;
                    }
                    *objp = vterm_make_large_tuple_ext(vtenv, arity, &elements);
                    if (n == 0) {
                        break;
                    }
                    objp = &elements[n - 1];
                    while (n-- > 0) {
                        objp[0] = (vterm_t)(next);
                        next = objp;
                        objp -= 1;
                    }
                    break;
                }
                case NIL_EXT: {
                    *objp = vterm_make_nil_ext(vtenv);
                    break;
                }
                case STRING_EXT: {
                    uint16_t len;
                    uint8_t *characters = NULL;
                    READ_U16(&len);
                    n = (size_t)len;
                    characters = RAW_BYTES();
                    SKIP(n);
                    *objp = vterm_make_string_ext(vtenv, len, characters);
                    break;
                }
                case LIST_EXT: {
                    uint32_t len;
                    vterm_t *elements = NULL;
                    vterm_t list = NULL;
                    READ_U32(&len);
                    n = (size_t)len;
                    CHKSIZE(n + 1);
                    if (trap->limit == 0 && n > 0) {
                        BACK(1 + 4);
                        trap->objp = objp;
                        trap->next = next;
                        trap->state = ETF_DECODE_VTERM_TRAP_STATE_DECODE_SKIP;
                        goto next_state;
                    }
                    *objp = list = vterm_make_list_ext(vtenv, len, &elements, (vterm_t)(next));
                    next = &list->data.list_ext.tail;
                    if (n == 0) {
                        break;
                    }
                    objp = &elements[n - 1];
                    while (n-- > 0) {
                        objp[0] = (vterm_t)(next);
                        next = objp;
                        objp -= 1;
                    }
                    break;
                }
                case BINARY_EXT: {
                    uint32_t len;
                    uint8_t *data = NULL;
                    READ_U32(&len);
                    n = (size_t)len;
                    data = RAW_BYTES();
                    SKIP(n);
                    *objp = vterm_make_binary_ext(vtenv, len, data);
                    break;
                }
                case BIT_BINARY_EXT: {
                    uint32_t len;
                    uint8_t bits;
                    uint8_t *data = NULL;
                    READ_U32(&len);
                    READ_U8(&bits);
                    n = (size_t)len;
                    data = RAW_BYTES();
                    SKIP(n);
                    *objp = vterm_make_bit_binary_ext(vtenv, len, bits, data);
                    break;
                }
                case SMALL_BIG_EXT: {
                    uint8_t len;
                    uint8_t sign;
                    uint8_t *d = NULL;
                    READ_U8(&len);
                    READ_U8(&sign);
                    n = (size_t)len;
                    d = RAW_BYTES();
                    SKIP(n);
                    *objp = vterm_make_small_big_ext(vtenv, len, sign, d);
                    break;
                }
                case LARGE_BIG_EXT: {
                    uint32_t len;
                    uint8_t sign;
                    uint8_t *d = NULL;
                    READ_U32(&len);
                    READ_U8(&sign);
                    n = (size_t)len;
                    d = RAW_BYTES();
                    SKIP(n);
                    *objp = vterm_make_large_big_ext(vtenv, len, sign, d);
                    break;
                }
                case NEW_FUN_EXT: {
                    uint32_t size;
                    uint8_t arity;
                    uint8_t *uniq = NULL;
                    uint32_t index;
                    uint32_t num_free;
                    vterm_t mod = NULL;
                    vterm_t old_index = NULL;
                    vterm_t old_uniq = NULL;
                    vterm_t pid = NULL;
                    vterm_t *free_vars = NULL;
                    if (trap->limit == 0) {
                        BACK(1);
                        trap->objp = objp;
                        trap->next = next;
                        trap->state = ETF_DECODE_VTERM_TRAP_STATE_DECODE_SKIP;
                        goto next_state;
                    }
                    READ_U32(&size);
                    READ_U8(&arity);
                    uniq = RAW_BYTES();
                    SKIP(16);
                    READ_U32(&index);
                    READ_U32(&num_free);
                    READ_ATOM(&mod);
                    READ_FIXED_INTEGER(&old_index);
                    READ_FIXED_INTEGER(&old_uniq);
                    READ_PID(&pid);
                    n = (size_t)num_free;
                    CHKSIZE(n);
                    *objp = vterm_make_new_fun_ext(vtenv, size, arity, uniq, index, num_free, mod, old_index, old_uniq, pid,
                                                   &free_vars);
                    if (n == 0) {
                        break;
                    }
                    objp = &free_vars[n - 1];
                    while (n-- > 0) {
                        objp[0] = (vterm_t)(next);
                        next = objp;
                        objp -= 1;
                    }
                    break;
                }
                case EXPORT_EXT: {
                    vterm_t mod = NULL;
                    vterm_t fun = NULL;
                    vterm_t arity = NULL;
                    READ_ATOM(&mod);
                    READ_ATOM(&fun);
                    READ_FIXED_INTEGER(&arity);
                    *objp = vterm_make_export_ext(vtenv, mod, fun, arity);
                    break;
                }
                case MAP_EXT: {
                    uint32_t arity;
                    vterm_t *pairs = NULL;
                    READ_U32(&arity);
                    n = (size_t)arity;
                    CHKSIZE(n * 2);
                    if (trap->limit == 0 && n > 0) {
                        BACK(1 + 4);
                        trap->objp = objp;
                        trap->next = next;
                        trap->state = ETF_DECODE_VTERM_TRAP_STATE_DECODE_SKIP;
                        goto next_state;
                    }
                    *objp = vterm_make_map_ext(vtenv, arity, &pairs);
                    if (n == 0) {
                        break;
                    }
                    n *= 2;
                    objp = &pairs[n - 1];
                    while (n-- > 0) {
                        objp[0] = (vterm_t)(next);
                        next = objp;
                        objp -= 1;
                    }
                    break;
                }
                case ATOM_UTF8_EXT: {
                    uint16_t len;
                    uint8_t *name = NULL;
                    READ_U16(&len);
                    n = (size_t)len;
                    if (n > MAX_ATOM_SZ_LIMIT) {
                        *err_termp = EXCP_ERROR_F(
                            caller_env,
                            "Call to etf_decode_vterm() failed: ATOM_UTF8_EXT len=%u is greater than MAX_ATOM_SZ_LIMIT=%u\n", n,
                            MAX_ATOM_SZ_LIMIT);
                        return TRAP_ERR(*err_termp);
                    }
                    name = RAW_BYTES();
                    SKIP(n);
                    *objp = vterm_make_atom_utf8_ext(vtenv, len, name);
                    break;
                }
                case SMALL_ATOM_UTF8_EXT: {
                    uint8_t len;
                    uint8_t *name = NULL;
                    READ_U8(&len);
                    n = (size_t)len;
                    if (n > MAX_ATOM_SZ_LIMIT) {
                        *err_termp = EXCP_ERROR_F(
                            caller_env,
                            "Call to etf_decode_vterm() failed: SMALL_ATOM_UTF8_EXT len=%u is greater than MAX_ATOM_SZ_LIMIT=%u\n",
                            n, MAX_ATOM_SZ_LIMIT);
                        return TRAP_ERR(*err_termp);
                    }
                    name = RAW_BYTES();
                    SKIP(n);
                    *objp = vterm_make_small_atom_utf8_ext(vtenv, len, name);
                    break;
                }
                case V4_PORT_EXT: {
                    vterm_t node = NULL;
                    uint64_t id;
                    uint32_t creation;
                    READ_ATOM(&node);
                    READ_U64(&id);
                    READ_U32(&creation);
                    *objp = vterm_make_v4_port_ext(vtenv, node, id, creation);
                    break;
                }
                case ATOM_CACHE_REF: {
                    uint8_t index;
                    READ_U8(&index);
                    n = (size_t)index;
                    if (attab == NULL || attab->size <= n || attab->entries[n].atom == THE_NON_VALUE) {
                        *err_termp = EXCP_ERROR_F(
                            caller_env, "Call to etf_decode_vterm() failed: ATOM_CACHE_REF index=%u not found in atom cache\n", n);
                        return TRAP_ERR(*err_termp);
                    }
                    *objp = vterm_make_atom_cache_ref_resolved(vtenv, index, attab->entries[n].atom);
                    break;
                }
                default:
                    err_term = EXCP_ERROR_F(caller_env, "Call to etf_decode_vterm() failed: unknown term tag=%u\n", tag);
                    return TRAP_ERR(err_term);
                }
                if (trap->limit > 0) {
                    trap->limit -= 1;
                }
                if (TRAP_SHOULD_YIELD(trap)) {
                    trap->objp = objp;
                    trap->next = next;
                    return TRAP_YIELD();
                }
            }

            trap->state = ETF_DECODE_VTERM_TRAP_STATE_DONE;
            goto next_state;
        }
        case ETF_DECODE_VTERM_TRAP_STATE_DECODE_SKIP: {
            edf_trap_result_t child_result;
            if (!edf_trap_has_child(&trap->super)) {
                ERL_NIF_TERM child_trap_term;
                etf_decode_term_length_trap_t *child_trap = NULL;
                vec_t control_slice;
                (void)vec_init_free(&control_slice);
                if (!vec_create_from_slice(&control_slice, RAW_BYTES(), vec_buf_tail(&trap->slice))) {
                    return TRAP_ERR(EXCP_ERROR(caller_env, "Call to vec_create_from_slice() failed\n"));
                }
                child_trap_term = etf_decode_term_length_trap_open(caller_env, false, &control_slice, decode_term_length_callback,
                                                                   (void *)trap, &child_trap);
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
                trap->state = ETF_DECODE_VTERM_TRAP_STATE_DECODE;
                goto next_state;
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
        case ETF_DECODE_VTERM_TRAP_STATE_DONE: {
            return TRAP_OK(THE_NON_VALUE);
        }
        default:
            return TRAP_ERR(
                EXCP_ERROR_F(caller_env, "Fatal error: unknown etf_decode_vterm_trap_t->state value %d\n", (int)(trap->state)));
        }
    next_state : {
        if (TRAP_SHOULD_YIELD(trap)) {
            return TRAP_YIELD();
        }
        continue;
    }
    } while (1);

#undef READ_PID
#undef READ_FIXED_INTEGER
#undef READ_ATOM
#undef SKIP
#undef BACK
#undef READ_U64
#undef READ_U32
#undef READ_I32
#undef READ_U16
#undef READ_U8
#undef CHKSIZE
#undef RAW_BYTES
}

int
etf_decode_vterm_atom(ErlNifEnv *caller_env, etf_decode_vterm_trap_t *trap, vterm_t *objp, ERL_NIF_TERM *err_termp)
{
    vterm_env_t *vtenv = trap->vtenv;
    vec_reader_t *vr = &trap->reader;
    edf_atom_translation_table_t *attab = trap->attab;
    uint8_t tag;
    size_t n;

#define RAW_BYTES() (void *)(vec_reader_raw_bytes(vr))

#define READ_U8(val)                                                                                                               \
    do {                                                                                                                           \
        if (!vec_reader_read_u8(vr, (val))) {                                                                                      \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_read_u8() failed: unable to decode vterm\n");                  \
            return 0;                                                                                                              \
        }                                                                                                                          \
        TRAP_REDUCE(trap, 1);                                                                                                      \
    } while (0)

#define READ_U16(val)                                                                                                              \
    do {                                                                                                                           \
        if (!vec_reader_read_u16(vr, (val))) {                                                                                     \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_read_u16() failed: unable to decode vterm\n");                 \
            return 0;                                                                                                              \
        }                                                                                                                          \
        TRAP_REDUCE(trap, 2);                                                                                                      \
    } while (0)

#define SKIP(sz)                                                                                                                   \
    do {                                                                                                                           \
        if (!vec_reader_skip_exact(vr, (sz))) {                                                                                    \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_skip_exact() failed: unable to decode vterm\n");               \
            return 0;                                                                                                              \
        }                                                                                                                          \
        TRAP_REDUCE(trap, (sz));                                                                                                   \
    } while (0)

    READ_U8(&tag);
    switch (tag) {
    case ATOM_EXT: {
        uint16_t len;
        uint8_t *name = NULL;
        READ_U16(&len);
        n = (size_t)len;
        if (n > MAX_ATOM_CHARACTERS) {
            *err_termp = EXCP_ERROR_F(caller_env,
                                      "Call to etf_decode_vterm() failed: ATOM_EXT len=%u is greater than MAX_ATOM_CHARACTERS=%u\n",
                                      n, MAX_ATOM_CHARACTERS);
            return 0;
        }
        name = RAW_BYTES();
        SKIP(n);
        *objp = vterm_make_atom_ext(vtenv, len, name);
        break;
    }
    case SMALL_ATOM_EXT: {
        uint8_t len;
        uint8_t *name = NULL;
        READ_U8(&len);
        n = (size_t)len;
        if (n > MAX_ATOM_CHARACTERS) {
            *err_termp = EXCP_ERROR_F(
                caller_env, "Call to etf_decode_vterm() failed: SMALL_ATOM_EXT len=%u is greater than MAX_ATOM_CHARACTERS=%u\n", n,
                MAX_ATOM_CHARACTERS);
            return 0;
        }
        name = RAW_BYTES();
        SKIP(n);
        *objp = vterm_make_small_atom_ext(vtenv, len, name);
        break;
    }
    case ATOM_UTF8_EXT: {
        uint16_t len;
        uint8_t *name = NULL;
        READ_U16(&len);
        n = (size_t)len;
        if (n > MAX_ATOM_SZ_LIMIT) {
            *err_termp = EXCP_ERROR_F(
                caller_env, "Call to etf_decode_vterm() failed: ATOM_UTF8_EXT len=%u is greater than MAX_ATOM_SZ_LIMIT=%u\n", n,
                MAX_ATOM_SZ_LIMIT);
            return 0;
        }
        name = RAW_BYTES();
        SKIP(n);
        *objp = vterm_make_atom_utf8_ext(vtenv, len, name);
        break;
    }
    case SMALL_ATOM_UTF8_EXT: {
        uint8_t len;
        uint8_t *name = NULL;
        READ_U8(&len);
        n = (size_t)len;
        if (n > MAX_ATOM_SZ_LIMIT) {
            *err_termp = EXCP_ERROR_F(
                caller_env, "Call to etf_decode_vterm() failed: SMALL_ATOM_UTF8_EXT len=%u is greater than MAX_ATOM_SZ_LIMIT=%u\n",
                n, MAX_ATOM_SZ_LIMIT);
            return 0;
        }
        name = RAW_BYTES();
        SKIP(n);
        *objp = vterm_make_small_atom_utf8_ext(vtenv, len, name);
        break;
    }
    case ATOM_CACHE_REF: {
        uint8_t index;
        READ_U8(&index);
        n = (size_t)index;
        if (attab == NULL || attab->size <= n || attab->entries[n].atom == THE_NON_VALUE) {
            *err_termp =
                EXCP_ERROR_F(caller_env, "Call to etf_decode_vterm() failed: ATOM_CACHE_REF index=%u not found in atom cache\n", n);
            return 0;
        }
        *objp = vterm_make_atom_cache_ref_resolved(vtenv, index, attab->entries[n].atom);
        break;
    }
    default: {
        *err_termp = EXCP_ERROR_F(caller_env, "Call to etf_decode_vterm_atom() failed: unknown term tag=%u\n", tag);
        return 0;
    }
    }

    if (!vterm_is_atom(trap->vtenv, objp)) {
        *err_termp = EXCP_ERROR(caller_env, "Call to etf_decode_vterm_atom() failed: decoded vterm is not an atom\n");
        return 0;
    }

    return 1;

#undef SKIP
#undef READ_U16
#undef READ_U8
#undef RAW_BYTES
}

int
etf_decode_vterm_fixed_integer(ErlNifEnv *caller_env, etf_decode_vterm_trap_t *trap, vterm_t *objp, ERL_NIF_TERM *err_termp)
{
    vterm_env_t *vtenv = trap->vtenv;
    vec_reader_t *vr = &trap->reader;
    uint8_t tag;

#define READ_U8(val)                                                                                                               \
    do {                                                                                                                           \
        if (!vec_reader_read_u8(vr, (val))) {                                                                                      \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_read_u8() failed: unable to decode vterm\n");                  \
            return 0;                                                                                                              \
        }                                                                                                                          \
        TRAP_REDUCE(trap, 1);                                                                                                      \
    } while (0)

#define READ_I32(val)                                                                                                              \
    do {                                                                                                                           \
        if (!vec_reader_read_i32(vr, (val))) {                                                                                     \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_read_i32() failed: unable to decode vterm\n");                 \
            return 0;                                                                                                              \
        }                                                                                                                          \
        TRAP_REDUCE(trap, 4);                                                                                                      \
    } while (0)

    READ_U8(&tag);
    switch (tag) {
    case SMALL_INTEGER_EXT: {
        uint8_t value;
        READ_U8(&value);
        *objp = vterm_make_small_integer_ext(vtenv, value);
        break;
    }
    case INTEGER_EXT: {
        int32_t value;
        READ_I32(&value);
        *objp = vterm_make_integer_ext(vtenv, value);
        break;
    }
    default: {
        *err_termp = EXCP_ERROR_F(caller_env, "Call to etf_decode_vterm_fixed_integer() failed: unknown term tag=%u\n", tag);
        return 0;
    }
    }

    if (!vterm_is_fixed_integer(trap->vtenv, objp)) {
        *err_termp =
            EXCP_ERROR(caller_env, "Call to etf_decode_vterm_fixed_integer() failed: decoded vterm is not a fixed integer\n");
        return 0;
    }

    return 1;

#undef READ_I32
#undef READ_U8
}

int
etf_decode_vterm_pid(ErlNifEnv *caller_env, etf_decode_vterm_trap_t *trap, vterm_t *objp, ERL_NIF_TERM *err_termp)
{
    vterm_env_t *vtenv = trap->vtenv;
    vec_reader_t *vr = &trap->reader;
    uint8_t tag;

#define READ_U8(val)                                                                                                               \
    do {                                                                                                                           \
        if (!vec_reader_read_u8(vr, (val))) {                                                                                      \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_read_u8() failed: unable to decode vterm\n");                  \
            return 0;                                                                                                              \
        }                                                                                                                          \
        TRAP_REDUCE(trap, 1);                                                                                                      \
    } while (0)

#define READ_U32(val)                                                                                                              \
    do {                                                                                                                           \
        if (!vec_reader_read_u32(vr, (val))) {                                                                                     \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_read_u32() failed: unable to decode vterm\n");                 \
            return 0;                                                                                                              \
        }                                                                                                                          \
        TRAP_REDUCE(trap, 4);                                                                                                      \
    } while (0)

#define READ_ATOM(vtptr)                                                                                                           \
    do {                                                                                                                           \
        if (!etf_decode_vterm_atom(caller_env, trap, (vtptr), err_termp)) {                                                        \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

    READ_U8(&tag);
    switch (tag) {
    case PID_EXT: {
        vterm_t node = NULL;
        uint32_t id;
        uint32_t serial;
        uint8_t creation;
        READ_ATOM(&node);
        READ_U32(&id);
        READ_U32(&serial);
        READ_U8(&creation);
        *objp = vterm_make_pid_ext(vtenv, node, id, serial, creation);
        break;
    }
    case NEW_PID_EXT: {
        vterm_t node = NULL;
        uint32_t id;
        uint32_t serial;
        uint32_t creation;
        READ_ATOM(&node);
        READ_U32(&id);
        READ_U32(&serial);
        READ_U32(&creation);
        *objp = vterm_make_new_pid_ext(vtenv, node, id, serial, creation);
        break;
    }
    default: {
        *err_termp = EXCP_ERROR_F(caller_env, "Call to etf_decode_vterm_pid() failed: unknown term tag=%u\n", tag);
        return 0;
    }
    }

    if (!vterm_is_pid(trap->vtenv, objp)) {
        *err_termp = EXCP_ERROR(caller_env, "Call to etf_decode_vterm_pid() failed: decoded vterm is not a pid\n");
        return 0;
    }

    return 1;

#undef READ_ATOM
#undef READ_U32
#undef READ_U8
}

void
decode_term_length_callback(ErlNifEnv *caller_env, etf_decode_term_length_trap_t *child, void *arg, edf_trap_result_t *result)
{
    etf_decode_vterm_trap_t *parent = (void *)arg;
    vterm_env_t *vtenv = parent->vtenv;
    vterm_t *objp = parent->objp;
    vec_reader_t *vr = &parent->reader;
    size_t term_length = 0;

    if (result->tag != EDF_TRAP_RESULT_TAG_OK) {
        return;
    }

    term_length = (size_t)(child->tail - child->head);

    if (child->heap_size <= VTERM_SIZEOF_LAZY_TERM()) {
        (void)enif_fprintf(
            stderr,
            "THIS SHOULD NEVER HAPPEN: child->head[0] = %c, child->heap_size = %llu, while VTERM_SIZEOF_LAZY_TERM() = %llu\n",
            (char)(child->head[0]), child->heap_size, VTERM_SIZEOF_LAZY_TERM());
        (void)vterm_env_dump_mem(child->head, term_length);
        (void)fflush(stderr);
        abort();
    }

    *objp = vterm_make_lazy_term(vtenv, child->head, child->tail, child->heap_size);
    (void)vec_reader_skip_exact(vr, term_length);

    return;
}

int
etf_decode_atom_term(ErlNifEnv *caller_env, vterm_env_t *vtenv, bool is_external_term, vec_reader_t *orig_vr, ERL_NIF_TERM *atomp,
                     ERL_NIF_TERM *err_termp)
{
    edf_atom_translation_table_t *attab = vtenv->attab;
    vec_reader_t vr[1];
    uint8_t tag;
    size_t n;
    ErtsAtomEncoding encoding;

#define RAW_BYTES() (void *)(vec_reader_raw_bytes(vr))

#define READ_U8(val)                                                                                                               \
    do {                                                                                                                           \
        if (!vec_reader_read_u8(vr, (val))) {                                                                                      \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_read_u8() failed: unable to decode vterm\n");                  \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define READ_U16(val)                                                                                                              \
    do {                                                                                                                           \
        if (!vec_reader_read_u16(vr, (val))) {                                                                                     \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_read_u16() failed: unable to decode vterm\n");                 \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define SKIP(sz)                                                                                                                   \
    do {                                                                                                                           \
        if (!vec_reader_skip_exact(vr, (sz))) {                                                                                    \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_skip_exact() failed: unable to decode vterm\n");               \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

    (void)vec_reader_clone(vr, orig_vr);

    if (is_external_term) {
        uint8_t version_magic;
        READ_U8(&version_magic);
        if (version_magic != VERSION_MAGIC) {
            *err_termp = EXCP_ERROR_F(
                caller_env,
                "Call to etf_decode_atom_term() failed: expected version_magic=%u to be VERSION_MAGIC=%u for external term\n",
                version_magic, VERSION_MAGIC);
            return 0;
        }
    }

    READ_U8(&tag);
    switch (tag) {
    case ATOM_EXT: {
        uint16_t len;
        uint8_t *name = NULL;
        READ_U16(&len);
        n = (size_t)len;
        if (n > MAX_ATOM_CHARACTERS) {
            *err_termp = EXCP_ERROR_F(
                caller_env, "Call to etf_decode_atom_term() failed: ATOM_EXT len=%u is greater than MAX_ATOM_CHARACTERS=%u\n", n,
                MAX_ATOM_CHARACTERS);
            return 0;
        }
        encoding = ERTS_ATOM_ENC_LATIN1;
        name = RAW_BYTES();
        SKIP(n);
        *atomp = erts_atom_put(name, (signed int)len, encoding, 0);
        break;
    }
    case SMALL_ATOM_EXT: {
        uint8_t len;
        uint8_t *name = NULL;
        READ_U8(&len);
        n = (size_t)len;
        if (n > MAX_ATOM_CHARACTERS) {
            *err_termp = EXCP_ERROR_F(
                caller_env, "Call to etf_decode_atom_term() failed: SMALL_ATOM_EXT len=%u is greater than MAX_ATOM_CHARACTERS=%u\n",
                n, MAX_ATOM_CHARACTERS);
            return 0;
        }
        encoding = ERTS_ATOM_ENC_LATIN1;
        name = RAW_BYTES();
        SKIP(n);
        *atomp = erts_atom_put(name, (signed int)len, encoding, 0);
        break;
    }
    case ATOM_UTF8_EXT: {
        uint16_t len;
        uint8_t *name = NULL;
        READ_U16(&len);
        n = (size_t)len;
        if (n > MAX_ATOM_SZ_LIMIT) {
            *err_termp = EXCP_ERROR_F(
                caller_env, "Call to etf_decode_atom_term() failed: ATOM_UTF8_EXT len=%u is greater than MAX_ATOM_SZ_LIMIT=%u\n", n,
                MAX_ATOM_SZ_LIMIT);
            return 0;
        }
        encoding = ERTS_ATOM_ENC_UTF8;
        name = RAW_BYTES();
        SKIP(n);
        *atomp = erts_atom_put(name, (signed int)len, encoding, 0);
        break;
    }
    case SMALL_ATOM_UTF8_EXT: {
        uint8_t len;
        uint8_t *name = NULL;
        READ_U8(&len);
        n = (size_t)len;
        if (n > MAX_ATOM_SZ_LIMIT) {
            *err_termp = EXCP_ERROR_F(
                caller_env,
                "Call to etf_decode_atom_term() failed: SMALL_ATOM_UTF8_EXT len=%u is greater than MAX_ATOM_SZ_LIMIT=%u\n", n,
                MAX_ATOM_SZ_LIMIT);
            return 0;
        }
        encoding = ERTS_ATOM_ENC_UTF8;
        name = RAW_BYTES();
        SKIP(n);
        *atomp = erts_atom_put(name, (signed int)len, encoding, 0);
        break;
    }
    case ATOM_CACHE_REF: {
        uint8_t index;
        READ_U8(&index);
        n = (size_t)index;
        if (attab == NULL || attab->size <= n || attab->entries[n].atom == THE_NON_VALUE) {
            *err_termp =
                EXCP_ERROR_F(caller_env, "Call to etf_decode_vterm() failed: ATOM_CACHE_REF index=%u not found in atom cache\n", n);
            return 0;
        }
        *atomp = attab->entries[n].atom;
        break;
    }
    default: {
        *err_termp =
            EXCP_ERROR_F(caller_env, "Call to etf_decode_atom_term() failed: expected tag=%u to be a valid ATOM tag\n", tag);
        return 0;
    }
    }

    (void)vec_reader_clone(orig_vr, vr);
    return 1;

#undef SKIP
#undef READ_U16
#undef READ_U8
#undef RAW_BYTES
}

int
etf_decode_fixed_integer(ErlNifEnv *caller_env, vterm_env_t *vtenv, bool is_external_term, vec_reader_t *orig_vr, int32_t *integerp,
                         ERL_NIF_TERM *err_termp)
{
    vec_reader_t vr[1];
    uint8_t tag;

#define READ_U8(val)                                                                                                               \
    do {                                                                                                                           \
        if (!vec_reader_read_u8(vr, (val))) {                                                                                      \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_read_u8() failed: unable to decode vterm\n");                  \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define READ_I32(val)                                                                                                              \
    do {                                                                                                                           \
        if (!vec_reader_read_i32(vr, (val))) {                                                                                     \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_read_i32() failed: unable to decode vterm\n");                 \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

    (void)vec_reader_clone(vr, orig_vr);

    if (is_external_term) {
        uint8_t version_magic;
        READ_U8(&version_magic);
        if (version_magic != VERSION_MAGIC) {
            *err_termp = EXCP_ERROR_F(
                caller_env,
                "Call to etf_decode_fixed_integer() failed: expected version_magic=%u to be VERSION_MAGIC=%u for external term\n",
                version_magic, VERSION_MAGIC);
            return 0;
        }
    }

    READ_U8(&tag);
    switch (tag) {
    case SMALL_INTEGER_EXT: {
        uint8_t small_integer;
        READ_U8(&small_integer);
        *integerp = (int32_t)(small_integer);
        break;
    }
    case INTEGER_EXT: {
        READ_I32(integerp);
        break;
    }
    default: {
        *err_termp = EXCP_ERROR_F(
            caller_env, "Call to etf_decode_fixed_integer() failed: expected tag=%u to be a valid FIXED_INTEGER tag\n", tag);
        return 0;
    }
    }

    (void)vec_reader_clone(orig_vr, vr);
    return 1;

#undef READ_I32
#undef READ_U8
}

int
etf_decode_list_header(ErlNifEnv *caller_env, vterm_env_t *vtenv, bool is_external_term, vec_reader_t *orig_vr, bool *is_nilp,
                       uint32_t *lengthp, ERL_NIF_TERM *err_termp)
{
    vec_reader_t vr[1];
    uint8_t tag;

#define READ_U8(val)                                                                                                               \
    do {                                                                                                                           \
        if (!vec_reader_read_u8(vr, (val))) {                                                                                      \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_read_u8() failed: unable to decode vterm\n");                  \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define READ_U32(val)                                                                                                              \
    do {                                                                                                                           \
        if (!vec_reader_read_u32(vr, (val))) {                                                                                     \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_read_u32() failed: unable to decode vterm\n");                 \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

    (void)vec_reader_clone(vr, orig_vr);

    if (is_external_term) {
        uint8_t version_magic;
        READ_U8(&version_magic);
        if (version_magic != VERSION_MAGIC) {
            *err_termp = EXCP_ERROR_F(
                caller_env,
                "Call to etf_decode_list_header() failed: expected version_magic=%u to be VERSION_MAGIC=%u for external term\n",
                version_magic, VERSION_MAGIC);
            return 0;
        }
    }

    READ_U8(&tag);
    switch (tag) {
    case NIL_EXT: {
        *is_nilp = true;
        *lengthp = 0;
        break;
    }
    case LIST_EXT: {
        READ_U32(lengthp);
        *is_nilp = false;
        break;
    }
    default: {
        *err_termp =
            EXCP_ERROR_F(caller_env, "Call to etf_decode_list_header() failed: expected tag=%u to be a valid TUPLE tag\n", tag);
        return 0;
    }
    }

    (void)vec_reader_clone(orig_vr, vr);
    return 1;

#undef READ_U32
#undef READ_U8
}

int
etf_decode_pid_term(ErlNifEnv *caller_env, vterm_env_t *vtenv, bool is_external_term, vec_reader_t *orig_vr, ERL_NIF_TERM *pidp,
                    ERL_NIF_TERM *err_termp)
{
    vec_reader_t vr[1];
    uint8_t tag;
    uint8_t *head = NULL;
    uint8_t *tail = NULL;
    size_t term_length = 0;
    uint8_t restore;
    int retval;
    ERL_NIF_TERM temp_term;

#define RAW_BYTES() (void *)(vec_reader_raw_bytes(vr))

#define READ_U8(val)                                                                                                               \
    do {                                                                                                                           \
        if (!vec_reader_read_u8(vr, (val))) {                                                                                      \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_read_u8() failed: unable to decode vterm\n");                  \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define PEEK_U8(val)                                                                                                               \
    do {                                                                                                                           \
        if (!vec_reader_peek_u8(vr, (val))) {                                                                                      \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_peek_u8() failed: unable to decode vterm\n");                  \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

    (void)vec_reader_clone(vr, orig_vr);

    head = RAW_BYTES();

    if (is_external_term) {
        uint8_t version_magic;
        READ_U8(&version_magic);
        if (version_magic != VERSION_MAGIC) {
            *err_termp = EXCP_ERROR_F(
                caller_env,
                "Call to etf_decode_pid_term() failed: expected version_magic=%u to be VERSION_MAGIC=%u for external term\n",
                version_magic, VERSION_MAGIC);
            return 0;
        }
    }

    PEEK_U8(&tag);
    switch (tag) {
    case PID_EXT:
        [[fallthrough]];
    case NEW_PID_EXT: {
        if (!etf_fast_skip_terms(caller_env, false, vr, 1, err_termp)) {
            return 0;
        }
        tail = RAW_BYTES();
        term_length = (size_t)(tail - head);
        if (!is_external_term) {
            /* Unsafe, since head - 1 is not known to exist,
             * but given current dist term encoding,
             * we always have at least one free byte to the left.
             */
            head -= 1;
            restore = head[0];
            head[0] = VERSION_MAGIC;
            term_length += 1;
        }
        retval = vterm_env_dist_ext_to_term(vtenv, head, term_length, &temp_term);
        if (!is_external_term) {
            head[0] = restore;
        }
        if (!retval) {
            *err_termp = EXCP_ERROR(caller_env, "Call to etf_decode_pid_term() failed: vterm_env_dist_ext_to_term raised badarg\n");
            return 0;
        }
        *pidp = enif_make_copy(vtenv->nif_env, temp_term);
        break;
    }
    default: {
        *err_termp = EXCP_ERROR_F(caller_env, "Call to etf_decode_pid_term() failed: expected tag=%u to be a valid PID tag\n", tag);
        return 0;
    }
    }

    (void)vec_reader_clone(orig_vr, vr);
    return 1;

#undef PEEK_U8
#undef READ_U8
#undef RAW_BYTES
}

int
etf_decode_port_term(ErlNifEnv *caller_env, vterm_env_t *vtenv, bool is_external_term, vec_reader_t *orig_vr, ERL_NIF_TERM *portp,
                     ERL_NIF_TERM *err_termp)
{
    vec_reader_t vr[1];
    uint8_t tag;
    uint8_t *head = NULL;
    uint8_t *tail = NULL;
    size_t term_length = 0;
    uint8_t restore;
    int retval;
    ERL_NIF_TERM temp_term;

#define RAW_BYTES() (void *)(vec_reader_raw_bytes(vr))

#define READ_U8(val)                                                                                                               \
    do {                                                                                                                           \
        if (!vec_reader_read_u8(vr, (val))) {                                                                                      \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_read_u8() failed: unable to decode vterm\n");                  \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define PEEK_U8(val)                                                                                                               \
    do {                                                                                                                           \
        if (!vec_reader_peek_u8(vr, (val))) {                                                                                      \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_peek_u8() failed: unable to decode vterm\n");                  \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

    (void)vec_reader_clone(vr, orig_vr);

    head = RAW_BYTES();

    if (is_external_term) {
        uint8_t version_magic;
        READ_U8(&version_magic);
        if (version_magic != VERSION_MAGIC) {
            *err_termp = EXCP_ERROR_F(
                caller_env,
                "Call to etf_decode_port_term() failed: expected version_magic=%u to be VERSION_MAGIC=%u for external term\n",
                version_magic, VERSION_MAGIC);
            return 0;
        }
    }

    PEEK_U8(&tag);
    switch (tag) {
    case PORT_EXT:
        [[fallthrough]];
    case NEW_PORT_EXT:
        [[fallthrough]];
    case V4_PORT_EXT: {
        if (!etf_fast_skip_terms(caller_env, false, vr, 1, err_termp)) {
            return 0;
        }
        tail = RAW_BYTES();
        term_length = (size_t)(tail - head);
        if (!is_external_term) {
            /* Unsafe, since head - 1 is not known to exist,
             * but given current dist term encoding,
             * we always have at least one free byte to the left.
             */
            head -= 1;
            restore = head[0];
            head[0] = VERSION_MAGIC;
            term_length += 1;
        }
        retval = vterm_env_dist_ext_to_term(vtenv, head, term_length, &temp_term);
        if (!is_external_term) {
            head[0] = restore;
        }
        if (!retval) {
            *err_termp =
                EXCP_ERROR(caller_env, "Call to etf_decode_port_term() failed: vterm_env_dist_ext_to_term raised badarg\n");
            return 0;
        }
        *portp = enif_make_copy(vtenv->nif_env, temp_term);
        break;
    }
    default: {
        *err_termp =
            EXCP_ERROR_F(caller_env, "Call to etf_decode_port_term() failed: expected tag=%u to be a valid PORT tag\n", tag);
        return 0;
    }
    }

    (void)vec_reader_clone(orig_vr, vr);
    return 1;

#undef PEEK_U8
#undef READ_U8
#undef RAW_BYTES
}

int
etf_decode_reference_term(ErlNifEnv *caller_env, vterm_env_t *vtenv, bool is_external_term, vec_reader_t *orig_vr,
                          ERL_NIF_TERM *refp, ERL_NIF_TERM *err_termp)
{
    vec_reader_t vr[1];
    uint8_t tag;
    uint8_t *head = NULL;
    uint8_t *tail = NULL;
    size_t term_length = 0;
    uint8_t restore;
    int retval;
    ERL_NIF_TERM temp_term;

#define RAW_BYTES() (void *)(vec_reader_raw_bytes(vr))

#define READ_U8(val)                                                                                                               \
    do {                                                                                                                           \
        if (!vec_reader_read_u8(vr, (val))) {                                                                                      \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_read_u8() failed: unable to decode vterm\n");                  \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define PEEK_U8(val)                                                                                                               \
    do {                                                                                                                           \
        if (!vec_reader_peek_u8(vr, (val))) {                                                                                      \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_peek_u8() failed: unable to decode vterm\n");                  \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

    (void)vec_reader_clone(vr, orig_vr);

    head = RAW_BYTES();

    if (is_external_term) {
        uint8_t version_magic;
        READ_U8(&version_magic);
        if (version_magic != VERSION_MAGIC) {
            *err_termp = EXCP_ERROR_F(
                caller_env,
                "Call to etf_decode_reference_term() failed: expected version_magic=%u to be VERSION_MAGIC=%u for external term\n",
                version_magic, VERSION_MAGIC);
            return 0;
        }
    }

    PEEK_U8(&tag);
    switch (tag) {
    case REFERENCE_EXT:
        [[fallthrough]];
    case NEW_REFERENCE_EXT:
        [[fallthrough]];
    case NEWER_REFERENCE_EXT: {
        if (!etf_fast_skip_terms(caller_env, false, vr, 1, err_termp)) {
            return 0;
        }
        tail = RAW_BYTES();
        term_length = (size_t)(tail - head);
        if (!is_external_term) {
            /* Unsafe, since head - 1 is not known to exist,
             * but given current dist term encoding,
             * we always have at least one free byte to the left.
             */
            head -= 1;
            restore = head[0];
            head[0] = VERSION_MAGIC;
            term_length += 1;
        }
        retval = vterm_env_dist_ext_to_term(vtenv, head, term_length, &temp_term);
        if (!is_external_term) {
            head[0] = restore;
        }
        if (!retval) {
            *err_termp =
                EXCP_ERROR(caller_env, "Call to etf_decode_reference_term() failed: vterm_env_dist_ext_to_term raised badarg\n");
            return 0;
        }
        *refp = enif_make_copy(vtenv->nif_env, temp_term);
        break;
    }
    default: {
        *err_termp =
            EXCP_ERROR_F(caller_env, "Call to etf_decode_reference_term() failed: expected tag=%u to be a valid PID tag\n", tag);
        return 0;
    }
    }

    (void)vec_reader_clone(orig_vr, vr);
    return 1;

#undef PEEK_U8
#undef READ_U8
#undef RAW_BYTES
}

int
etf_decode_tuple_header(ErlNifEnv *caller_env, vterm_env_t *vtenv, bool is_external_term, vec_reader_t *orig_vr, uint32_t *arityp,
                        ERL_NIF_TERM *err_termp)
{
    vec_reader_t vr[1];
    uint8_t tag;

#define READ_U8(val)                                                                                                               \
    do {                                                                                                                           \
        if (!vec_reader_read_u8(vr, (val))) {                                                                                      \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_read_u8() failed: unable to decode vterm\n");                  \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define READ_U32(val)                                                                                                              \
    do {                                                                                                                           \
        if (!vec_reader_read_u32(vr, (val))) {                                                                                     \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_read_u32() failed: unable to decode vterm\n");                 \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

    (void)vec_reader_clone(vr, orig_vr);

    if (is_external_term) {
        uint8_t version_magic;
        READ_U8(&version_magic);
        if (version_magic != VERSION_MAGIC) {
            *err_termp = EXCP_ERROR_F(
                caller_env,
                "Call to etf_decode_tuple_header() failed: expected version_magic=%u to be VERSION_MAGIC=%u for external term\n",
                version_magic, VERSION_MAGIC);
            return 0;
        }
    }

    READ_U8(&tag);
    switch (tag) {
    case SMALL_TUPLE_EXT: {
        uint8_t small_arity;
        READ_U8(&small_arity);
        *arityp = (uint32_t)(small_arity);
        break;
    }
    case LARGE_TUPLE_EXT: {
        READ_U32(arityp);
        break;
    }
    default: {
        *err_termp =
            EXCP_ERROR_F(caller_env, "Call to etf_decode_tuple_header() failed: expected tag=%u to be a valid TUPLE tag\n", tag);
        return 0;
    }
    }

    (void)vec_reader_clone(orig_vr, vr);
    return 1;

#undef READ_U32
#undef READ_U8
}
