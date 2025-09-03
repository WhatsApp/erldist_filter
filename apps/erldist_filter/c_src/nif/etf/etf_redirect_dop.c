/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "etf_redirect_dop.h"

#include "etf_decode.h"

#include "../../primitive/unreachable.h"
#include "../channel/edf_channel.h"

static const uint8_t c_tuple4_reg_send_header[] = {
    // clang-format off
    SMALL_TUPLE_EXT, 4,
        SMALL_INTEGER_EXT, DOP_REG_SEND,
        NEW_PID_EXT,                          // elements[1].From
            SMALL_ATOM_UTF8_EXT, 0,           // elements[1].From.Node
            0, 0, 0, 0,                       // elements[1].From.ID
            0, 0, 0, 0,                       // elements[1].From.Serial
            0, 0, 0, 0,                       // elements[1].From.Creation
        SMALL_ATOM_UTF8_EXT, 0,               // elements[2].Unused
    // clang-format on
};
// Insert: elements[3].ToName
static const uint8_t c_tuple5_reg_send_tt_header[] = {
    // clang-format off
    SMALL_TUPLE_EXT, 5,
        SMALL_INTEGER_EXT, DOP_REG_SEND_TT,
        NEW_PID_EXT,                          // elements[1].From
            SMALL_ATOM_UTF8_EXT, 0,           // elements[1].From.Node
            0, 0, 0, 0,                       // elements[1].From.ID
            0, 0, 0, 0,                       // elements[1].From.Serial
            0, 0, 0, 0,                       // elements[1].From.Creation
        SMALL_ATOM_UTF8_EXT, 0,               // elements[2].Unused
    // clang-format on
};
// Insert: elements[3].ToName
// Insert: elements[4].TraceToken
static const uint8_t c_tuple3_payload_header[] = {SMALL_TUPLE_EXT, 3};
// Insert: elements[0].Sysname
// Insert: elements[1].Sort
// Insert: elements[2].Control
static const uint8_t c_tuple4_payload_header[] = {SMALL_TUPLE_EXT, 4};
// Insert: elements[0].Sysname
// Insert: elements[1].Sort
// Insert: elements[2].Control
// Insert: elements[2].Payload

static int etf_redirect_dop_resolve_atom(ErlNifEnv *caller_env, edf_external_t *ext, ERL_NIF_TERM atom, vec_writer_t *vw);
static int etf_redirect_dop_resolve_atom_length(ErlNifEnv *caller_env, edf_external_t *ext, ERL_NIF_TERM atom,
                                                size_t *atom_lengthp);
static int etf_redirect_dop_resolve_u64(ErlNifEnv *caller_env, edf_external_t *ext, uint64_t val, vec_writer_t *vw);
static int etf_redirect_dop_resolve_u64_length(ErlNifEnv *caller_env, edf_external_t *ext, uint64_t val, size_t *lengthp);

int
etf_redirect_dop(ErlNifEnv *caller_env, edf_external_t *ext, ERL_NIF_TERM *err_termp)
{
    size_t name_length;
    size_t node_length;
    uint64_t sort;
    size_t sort_length;
    vec_t vec[1];
    size_t new_length = 0;
    slice_t old_framing[1] = {SLICE_INIT_EMPTY()};
    slice_t old_headers[1] = {SLICE_INIT_EMPTY()};
    slice_t old_control[1] = {SLICE_INIT_EMPTY()};
    slice_t old_payload[1] = {SLICE_INIT_EMPTY()};
    slice_t old_token[1] = {SLICE_INIT_EMPTY()};

#define CLEANUP()                                                                                                                  \
    do {                                                                                                                           \
        (void)vec_destroy(vec);                                                                                                    \
    } while (0)

#define RAW_BYTES() (vec_writer_raw_bytes(vw))

#define WRITE_U8(val)                                                                                                              \
    do {                                                                                                                           \
        if (!vec_writer_write_u8(vw, (val))) {                                                                                     \
            CLEANUP();                                                                                                             \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_writer_write_u8() failed: unable to redirect dop\n");                 \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define WRITE_BYTES(buf, len)                                                                                                      \
    do {                                                                                                                           \
        if (!vec_writer_write_exact(vw, (buf), (len))) {                                                                           \
            CLEANUP();                                                                                                             \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_writer_write_exact() failed: unable to redirect dop\n");              \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

    (void)vec_init_free(vec);
    old_framing->head = vec_buf(&ext->slices.framing.vec);
    old_framing->tail = vec_buf_tail(&ext->slices.framing.vec);
    old_headers->head = vec_buf(&ext->slices.headers.vec);
    old_headers->tail = vec_buf_tail(&ext->slices.headers.vec);
    old_control->head = vec_buf(&ext->slices.control.vec);
    old_control->tail = vec_buf_tail(&ext->slices.control.vec);
    if (ext->up->info.payload) {
        old_payload->head = vec_buf(&ext->slices.payload.vec);
        old_payload->tail = vec_buf_tail(&ext->slices.payload.vec);
    }

    if (!etf_redirect_dop_resolve_atom_length(caller_env, ext, ext->channel->rx.router_name, &name_length)) {
        CLEANUP();
        *err_termp = EXCP_ERROR(caller_env, "Call to etf_redirect_dop_resolve_atom_length() failed: unable to redirect dop\n");
        return 0;
    }

    if (!etf_redirect_dop_resolve_atom_length(caller_env, ext, ext->channel->sysname, &node_length)) {
        CLEANUP();
        *err_termp = EXCP_ERROR(caller_env, "Call to etf_redirect_dop_resolve_atom_length() failed: unable to redirect dop\n");
        return 0;
    }

    sort = ext->channel->rx.sort++;

    if (!etf_redirect_dop_resolve_u64_length(caller_env, ext, sort, &sort_length)) {
        CLEANUP();
        *err_termp = EXCP_ERROR(caller_env, "Call to etf_redirect_dop_resolve_u64_length() failed: unable to redirect dop\n");
        return 0;
    }

    if (ext->up->info.token_offset != -1) {
        bool is_external_term = false;
        vec_t control_slice;
        vec_reader_t vr[1];
        uint32_t token_offset = (uint32_t)(ext->up->info.token_offset);
        uint32_t arity;
        (void)vec_init_free(&control_slice);
        if (!edf_external_slice_control_get(ext, &is_external_term, &control_slice)) {
            CLEANUP();
            *err_termp = EXCP_ERROR(caller_env,
                                    "Call to edf_external_slice_control_get() failed: unable to get slice for control message\n");
            return 0;
        }
        if (!vec_reader_create(vr, &control_slice, 0)) {
            CLEANUP();
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_create() failed: unable to decode control message\n");
            return 0;
        }
        if (!etf_decode_tuple_header(caller_env, ext->vtenv, is_external_term, vr, &arity, err_termp)) {
            CLEANUP();
            return 0;
        }
        if (arity <= token_offset) {
            CLEANUP();
            *err_termp =
                EXCP_ERROR_F(caller_env, "Call to etf_redirect_dop() failed: control arity=%u does not match token_offset=%u\n",
                             arity, token_offset);
            return 0;
        }
        if (token_offset > 0 && !etf_fast_skip_terms(caller_env, false, vr, token_offset, err_termp)) {
            CLEANUP();
            return 0;
        }
        old_token->head = vec_reader_raw_bytes(vr);
        if (!etf_fast_skip_terms(caller_env, false, vr, 1, err_termp)) {
            CLEANUP();
            return 0;
        }
        old_token->tail = vec_reader_raw_bytes(vr);
        (void)vec_reader_destroy(vr);
        (void)vec_destroy(&control_slice);
    }

    if (edf_external_is_pass_through(ext)) {
        new_length += 1;
        old_control->head += 1;
        if (!slice_is_empty(old_payload)) {
            old_payload->head += 1;
        }
    }
    new_length += slice_len(old_framing);
    new_length += slice_len(old_headers);
    if (ext->up->info.token_offset != -1) {
        new_length += sizeof(c_tuple5_reg_send_tt_header) + slice_len(old_token);
    } else {
        new_length += sizeof(c_tuple4_reg_send_header);
    }
    new_length += node_length;
    if (edf_external_is_pass_through(ext)) {
        new_length += 1;
    }
    if (slice_is_empty(old_payload)) {
        new_length += sizeof(c_tuple3_payload_header);
        new_length += name_length;
        new_length += sort_length;
        new_length += slice_len(old_control);
    } else {
        new_length += sizeof(c_tuple4_payload_header);
        new_length += name_length;
        new_length += sort_length;
        new_length += slice_len(old_control);
        new_length += slice_len(old_payload);
    }

    do {
        vec_writer_t vw[1];
        slice_t new_framing[1];
        slice_t new_headers[1];
        slice_t new_control[1];
        slice_t new_payload[1];

        if (!vec_create_owned(vec, new_length)) {
            CLEANUP();
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_create_owned() failed: unable to redirect dop\n");
            return 0;
        }
        if (!vec_writer_create(vw, vec, 0)) {
            CLEANUP();
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_writer_create() failed: unable to redirect dop\n");
            return 0;
        }
        new_framing->head = RAW_BYTES();
        if (old_framing->head != NULL && slice_len(old_framing) > 0) {
            WRITE_BYTES(old_framing->head, slice_len(old_framing));
        }
        new_framing->tail = RAW_BYTES();
        new_headers->head = RAW_BYTES();
        if (old_headers->head != NULL && slice_len(old_headers) > 0) {
            WRITE_BYTES(old_headers->head, slice_len(old_headers));
        }
        new_headers->tail = RAW_BYTES();
        new_control->head = RAW_BYTES();
        if (edf_external_is_pass_through(ext)) {
            WRITE_U8(VERSION_MAGIC);
        }
        if (!slice_is_empty(old_token)) {
            WRITE_BYTES(c_tuple5_reg_send_tt_header, sizeof(c_tuple5_reg_send_tt_header));
        } else {
            WRITE_BYTES(c_tuple4_reg_send_header, sizeof(c_tuple4_reg_send_header));
        }
        if (!etf_redirect_dop_resolve_atom(caller_env, ext, ext->channel->rx.router_name, vw)) {
            CLEANUP();
            *err_termp = EXCP_ERROR(caller_env, "Call to etf_redirect_dop_resolve_atom() failed: unable to redirect dop\n");
            return 0;
        }
        if (!slice_is_empty(old_token)) {
            WRITE_BYTES(old_token->head, slice_len(old_token));
        }
        new_control->tail = RAW_BYTES();
        new_payload->head = RAW_BYTES();
        if (edf_external_is_pass_through(ext)) {
            WRITE_U8(VERSION_MAGIC);
        }
        if (slice_is_empty(old_payload)) {
            WRITE_BYTES(c_tuple3_payload_header, sizeof(c_tuple3_payload_header));
        } else {
            WRITE_BYTES(c_tuple4_payload_header, sizeof(c_tuple4_payload_header));
        }
        if (!etf_redirect_dop_resolve_atom(caller_env, ext, ext->channel->sysname, vw)) {
            CLEANUP();
            *err_termp = EXCP_ERROR(caller_env, "Call to etf_redirect_dop_resolve_atom() failed: unable to redirect dop\n");
            return 0;
        }
        if (!etf_redirect_dop_resolve_u64(caller_env, ext, sort, vw)) {
            CLEANUP();
            *err_termp = EXCP_ERROR(caller_env, "Call to etf_redirect_dop_resolve_u64() failed: unable to redirect dop\n");
            return 0;
        }
        WRITE_BYTES(old_control->head, slice_len(old_control));
        if (!slice_is_empty(old_payload)) {
            WRITE_BYTES(old_payload->head, slice_len(old_payload));
        }
        new_payload->tail = RAW_BYTES();
        if (vec_is_writable(vec)) {
            *err_termp = EXCP_ERROR_F(caller_env,
                                      "Call to etf_redirect_dop() failed: vec is still writable after rewriting the "
                                      "dop (len=%llu, remaining_writable_bytes=%llu)\n",
                                      vec_len(vec), vec_remaining_writable_bytes(vec));
            CLEANUP();
            return 0;
        }
        (void)vec_destroy(&ext->primary->vec);
        if (!vec_move(&ext->primary->vec, vec)) {
            CLEANUP();
            *err_termp = EXCP_ERROR(caller_env, "Call to etf_redirect_dop() failed: unable to move vec after rewriting the dop\n");
            return 0;
        }
        ext->slices.headers.vec.data.slice.head = new_headers->head;
        ext->slices.headers.vec.data.slice.tail = new_headers->tail;
        ext->slices.control.vec.data.slice.head = new_control->head;
        ext->slices.control.vec.data.slice.tail = new_control->tail;
        ext->slices.control.length = slice_len(new_control);
        ext->slices.payload.vec.tag = VEC_TAG_SLICE;
        ext->slices.payload.vec.data.slice.head = new_payload->head;
        ext->slices.payload.vec.data.slice.tail = new_payload->tail;
        ext->slices.payload.offset = (size_t)(new_payload->head - new_headers->head);
        ext->slices.payload.length = slice_len(new_payload);

    } while (0);

    return 1;

#undef WRITE_BYTES
#undef WRITE_U8
#undef RAW_BYTES
#undef CLEANUP
}

int
etf_redirect_dop_resolve_atom(ErlNifEnv *caller_env, edf_external_t *ext, ERL_NIF_TERM atom, vec_writer_t *vw)
{
    int internal_index;
    const uint8_t *c_name = NULL;
    uint8_t *a_name = NULL;
    size_t len;

#define CLEANUP_UTF8()                                                                                                             \
    do {                                                                                                                           \
        (void)edf_atom_text_drop_name(&c_name);                                                                                    \
    } while (0)

#define CLEANUP_LATIN1()                                                                                                           \
    do {                                                                                                                           \
        (void)enif_free((void *)a_name);                                                                                           \
    } while (0)

    if (edf_atom_translation_table_get_entry(&ext->attab, atom, &internal_index)) {
        if (!vec_writer_write_u8(vw, ATOM_CACHE_REF)) {
            return 0;
        }
        if (!vec_writer_write_u8(vw, (uint8_t)(internal_index))) {
            return 0;
        }
        return 1;
    } else if (edf_atom_text_get_name(atom, ERL_NIF_UTF8, &c_name, &len)) {
        if (len > 255) {
            if (!vec_writer_write_u8(vw, ATOM_UTF8_EXT)) {
                CLEANUP_UTF8();
                return 0;
            }
            if (!vec_writer_write_u16(vw, (uint16_t)(len))) {
                CLEANUP_UTF8();
                return 0;
            }
        } else {
            if (!vec_writer_write_u8(vw, SMALL_ATOM_UTF8_EXT)) {
                CLEANUP_UTF8();
                return 0;
            }
            if (!vec_writer_write_u8(vw, (uint8_t)(len))) {
                CLEANUP_UTF8();
                return 0;
            }
        }
        if (!vec_writer_write_exact(vw, c_name, len)) {
            CLEANUP_UTF8();
            return 0;
        }
        CLEANUP_UTF8();
        return 1;
    } else {
        unsigned int ui_len;
        if (!enif_get_atom_length(caller_env, atom, &ui_len, ERL_NIF_LATIN1)) {
            return 0;
        }
        len = (size_t)(ui_len);
        a_name = (void *)enif_alloc(len + 1);
        if (a_name == NULL) {
            return 0;
        }
        if (!enif_get_atom(caller_env, atom, (char *)a_name, ui_len + 1, ERL_NIF_LATIN1)) {
            CLEANUP_LATIN1();
            return 0;
        }
        if (len > 255) {
            if (!vec_writer_write_u8(vw, ATOM_UTF8_EXT)) {
                CLEANUP_LATIN1();
                return 0;
            }
            if (!vec_writer_write_u16(vw, (uint16_t)(len))) {
                CLEANUP_LATIN1();
                return 0;
            }
        } else {
            if (!vec_writer_write_u8(vw, SMALL_ATOM_UTF8_EXT)) {
                CLEANUP_LATIN1();
                return 0;
            }
            if (!vec_writer_write_u8(vw, (uint8_t)(len))) {
                CLEANUP_LATIN1();
                return 0;
            }
        }
        if (!vec_writer_write_exact(vw, a_name, len)) {
            CLEANUP_LATIN1();
            return 0;
        }
        CLEANUP_LATIN1();
        return 1;
    }

    unreachable();

#undef CLEANUP_LATIN1
#undef CLEANUP_UTF8
}

int
etf_redirect_dop_resolve_atom_length(ErlNifEnv *caller_env, edf_external_t *ext, ERL_NIF_TERM atom, size_t *atom_lengthp)
{
    int internal_index;
    size_t atom_length;
    if (edf_atom_translation_table_get_entry(&ext->attab, atom, &internal_index)) {
        *atom_lengthp = 1 + 1;
        return 1;
    } else if (edf_atom_text_get_length(atom, ERL_NIF_UTF8, &atom_length)) {
        if (atom_length > 255) {
            *atom_lengthp = 1 + 2 + atom_length;
        } else {
            *atom_lengthp = 1 + 1 + atom_length;
        }
        return 1;
    } else {
        unsigned int ui_len;
        if (!enif_get_atom_length(caller_env, atom, &ui_len, ERL_NIF_LATIN1)) {
            return 0;
        }
        atom_length = (size_t)(ui_len);
        if (atom_length > 255) {
            *atom_lengthp = 1 + 2 + atom_length;
        } else {
            *atom_lengthp = 1 + 1 + atom_length;
        }
        return 1;
    }
    unreachable();
}

int
etf_redirect_dop_resolve_node(ErlNifEnv *caller_env, edf_external_t *ext, vec_t *node_vec)
{
    size_t node_length;
    vec_writer_t vw[1];

    if (!etf_redirect_dop_resolve_atom_length(caller_env, ext, ext->channel->sysname, &node_length)) {
        return 0;
    }
    if (!vec_create_owned_mem(node_vec, node_length)) {
        return 0;
    }
    if (!vec_writer_create(vw, node_vec, 0)) {
        (void)vec_destroy(node_vec);
        return 0;
    }
    if (!etf_redirect_dop_resolve_atom(caller_env, ext, ext->channel->sysname, vw)) {
        (void)vec_writer_destroy(vw);
        (void)vec_destroy(node_vec);
        return 0;
    }
    if (vec_is_writable(node_vec)) {
        (void)vec_writer_destroy(vw);
        (void)vec_destroy(node_vec);
        return 0;
    }
    (void)vec_writer_destroy(vw);
    return 1;
}

int
etf_redirect_dop_resolve_u64(ErlNifEnv *caller_env, edf_external_t *ext, uint64_t val, vec_writer_t *vw)
{
    (void)caller_env;
    (void)ext;
    if (val < 256) {
        if (!vec_writer_write_u8(vw, SMALL_INTEGER_EXT)) {
            return 0;
        }
        if (!vec_writer_write_u8(vw, (uint8_t)(val))) {
            return 0;
        }
        return 1;
    } else if (val < 2147483648) {
        if (!vec_writer_write_u8(vw, INTEGER_EXT)) {
            return 0;
        }
        if (!vec_writer_write_i32(vw, (int32_t)(val))) {
            return 0;
        }
        return 1;
    } else {
        if (!vec_writer_write_u8(vw, SMALL_BIG_EXT)) {
            return 0;
        }
        if (!vec_writer_write_u8(vw, 8)) {
            return 0;
        }
        if (!vec_writer_write_u8(vw, 0)) {
            return 0;
        }
        if (!vec_writer_write_u64(vw, htobe64(val))) {
            return 0;
        }
        return 1;
    }
    unreachable();
}

int
etf_redirect_dop_resolve_u64_length(ErlNifEnv *caller_env, edf_external_t *ext, uint64_t val, size_t *lengthp)
{
    (void)caller_env;
    (void)ext;
    if (val < 256) {
        *lengthp = 2;
    } else if (val < 2147483648) {
        *lengthp = 5;
    } else {
        *lengthp = 11;
    }
    return 1;
}
