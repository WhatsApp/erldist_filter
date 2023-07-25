/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "etf_redirect_dop.h"

#include "etf_decode.h"

#include "../core/unreachable.h"
#include "../channel/edf_channel.h"

// Node = vterm_small_atom_utf8_ext:new(0, <<>>),
// ReqId = vterm_newer_reference_ext:new(1, Node, 0, [0]),
// From = vterm_new_pid_ext:new(Node, 0, 0, 0),
// GroupLeader = From,
// Module = vterm_small_atom_utf8_ext:new(3, <<"edf">>),
// Function = vterm_small_atom_utf8_ext:new(3, <<"dop">>),
// Arity = vterm_small_integer_ext:new(3),
// OptList = [],
// SpawnRequest = vdist_dop_spawn_request:new(ReqId, From, GroupLeader, Module, Function, Arity, OptList),
// SpawnRequestVTerm = vdist_dop:dop_to_control_message_vterm(SpawnRequest),
// erlang:display(vterm_encode:internal_vterm_to_binary(SpawnRequestVTerm, #{})).

static const uint8_t c_atom_undefined[] = {SMALL_ATOM_UTF8_EXT, 9, 'u', 'n', 'd', 'e', 'f', 'i', 'n', 'e', 'd'};
static const uint8_t c_tuple6_spawn_request_header[] = {SMALL_TUPLE_EXT, 6, SMALL_INTEGER_EXT, DOP_SPAWN_REQUEST};
static const uint8_t c_tuple7_spawn_request_tt_header[] = {SMALL_TUPLE_EXT, 7, SMALL_INTEGER_EXT, DOP_SPAWN_REQUEST_TT};
static const uint8_t c_tuple_elements_0[] = {
    // clang-format off
    NEWER_REFERENCE_EXT, 0, 1,            // elements[1].ReqId
    // clang-format on
};
// Insert: elements[1].ReqId.Node
static const uint8_t c_tuple_elements_1[] = {
    // clang-format off
        0, 0, 0, 0,                       // elements[1].ReqId.Creation
        0, 0, 0, 0,                       // elements[1].ReqId.ID...
    NEW_PID_EXT,                          // elements[2].From
    // clang-format on
};
// Insert: elements[2].From.Node
static const uint8_t c_tuple_elements_2[] = {
    // clang-format off
        0, 0, 0, 0,                       // elements[2].From.ID
        0, 0, 0, 0,                       // elements[2].From.Serial
        0, 0, 0, 0,                       // elements[2].From.Creation
    NEW_PID_EXT,                          // elements[3].GroupLeader
    // clang-format on
};
// Insert: elements[3].GroupLeader.Node
static const uint8_t c_tuple_elements_3[] = {
    // clang-format off
        0, 0, 0, 0,                       // elements[3].GroupLeader.ID
        0, 0, 0, 0,                       // elements[3].GroupLeader.Serial
        0, 0, 0, 0,                       // elements[3].GroupLeader.Creation
    SMALL_TUPLE_EXT, 3,                   // elements[4].tuple3
        SMALL_ATOM_UTF8_EXT, 3,           // elements[4].tuple3.elements[0].Module
            'e', 'd', 'f',
        SMALL_ATOM_UTF8_EXT, 3,           // elements[4].tuple3.elements[1].Function
            'd', 'o', 'p',
        SMALL_INTEGER_EXT, 3,             // elements[4].tuple3.elements[2].Arity
    NIL_EXT,                              // elements[5].OptList
    // clang-format on
};
static const uint8_t c_list3_header[] = {LIST_EXT, 0, 0, 0, 3};
static const uint8_t c_list3_footer[] = {NIL_EXT};

int
etf_redirect_dop(ErlNifEnv *caller_env, edf_external_t *ext, ERL_NIF_TERM *err_termp)
{
    vec_t node_vec[1];
    vec_t vec[1];
    size_t new_length = 0;
    slice_t old_framing[1] = {SLICE_INIT_EMPTY()};
    slice_t old_headers[1] = {SLICE_INIT_EMPTY()};
    slice_t old_control[1] = {SLICE_INIT_EMPTY()};
    slice_t old_payload[1] = {SLICE_INIT_EMPTY()};
    slice_t old_token[1] = {SLICE_INIT_EMPTY()};

#define CLEANUP()                                                                                                                  \
    do {                                                                                                                           \
        (void)vec_destroy(node_vec);                                                                                               \
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

    (void)vec_init_free(node_vec);
    (void)vec_init_free(vec);
    old_framing->head = vec_buf(&ext->primary->vec);
    old_framing->tail = vec_buf(&ext->slices.headers.vec);
    if (old_framing->tail == NULL) {
        old_framing->head = NULL;
    }
    old_headers->head = vec_buf(&ext->slices.headers.vec);
    old_headers->tail = vec_buf_tail(&ext->slices.headers.vec);
    old_control->head = vec_buf(&ext->slices.control.vec);
    old_control->tail = vec_buf_tail(&ext->slices.control.vec);
    if (ext->up->info.payload) {
        old_payload->head = vec_buf(&ext->slices.payload.vec);
        old_payload->tail = vec_buf_tail(&ext->slices.payload.vec);
    }

    if (!etf_redirect_dop_resolve_node(caller_env, ext, node_vec)) {
        CLEANUP();
        *err_termp = EXCP_ERROR(caller_env, "Call to etf_redirect_dop_resolve_node() failed: unable to redirect dop\n");
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
    }
    new_length += slice_len(old_framing);
    new_length += slice_len(old_headers);
    if (ext->up->info.token_offset != -1) {
        new_length += sizeof(c_tuple7_spawn_request_tt_header) + slice_len(old_token);
    } else {
        new_length += sizeof(c_tuple6_spawn_request_header);
    }
    new_length += sizeof(c_tuple_elements_0) + sizeof(c_tuple_elements_1) + sizeof(c_tuple_elements_2) + sizeof(c_tuple_elements_3);
    new_length += (vec_len(node_vec) * 4);
    new_length += sizeof(c_list3_header) + slice_len(old_control) + sizeof(c_list3_footer);
    if (!slice_is_empty(old_payload)) {
        new_length += slice_len(old_payload);
    } else {
        new_length += sizeof(c_atom_undefined);
        if (edf_external_is_pass_through(ext)) {
            new_length += 1;
        }
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
        if (edf_external_is_pass_through(ext)) {
            WRITE_U8(PASS_THROUGH);
        }
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
            WRITE_BYTES(c_tuple7_spawn_request_tt_header, sizeof(c_tuple7_spawn_request_tt_header));
        } else {
            WRITE_BYTES(c_tuple6_spawn_request_header, sizeof(c_tuple6_spawn_request_header));
        }
        WRITE_BYTES(c_tuple_elements_0, sizeof(c_tuple_elements_0));
        WRITE_BYTES(vec_buf(node_vec), vec_len(node_vec));
        WRITE_BYTES(c_tuple_elements_1, sizeof(c_tuple_elements_1));
        WRITE_BYTES(vec_buf(node_vec), vec_len(node_vec));
        WRITE_BYTES(c_tuple_elements_2, sizeof(c_tuple_elements_2));
        WRITE_BYTES(vec_buf(node_vec), vec_len(node_vec));
        WRITE_BYTES(c_tuple_elements_3, sizeof(c_tuple_elements_3));
        if (!slice_is_empty(old_token)) {
            WRITE_BYTES(old_token->head, slice_len(old_token));
        }
        new_control->tail = RAW_BYTES();
        new_payload->head = RAW_BYTES();
        if (edf_external_is_pass_through(ext)) {
            old_control->head += 1;
            if (!slice_is_empty(old_payload)) {
                old_payload->head += 1;
            }
            WRITE_U8(VERSION_MAGIC);
        }
        WRITE_BYTES(c_list3_header, sizeof(c_list3_header));
        WRITE_BYTES(vec_buf(node_vec), vec_len(node_vec));
        WRITE_BYTES(old_control->head, slice_len(old_control));
        if (!slice_is_empty(old_payload)) {
            WRITE_BYTES(old_payload->head, slice_len(old_payload));
        } else {
            WRITE_BYTES(c_atom_undefined, sizeof(c_atom_undefined));
        }
        WRITE_BYTES(c_list3_footer, sizeof(c_list3_footer));
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

    (void)vec_destroy(node_vec);

    return 1;

#undef WRITE_BYTES
#undef WRITE_U8
#undef RAW_BYTES
#undef CLEANUP
}

int
etf_redirect_dop_resolve_node(ErlNifEnv *caller_env, edf_external_t *ext, vec_t *node_vec)
{
    int internal_index;
    const uint8_t *c_name = NULL;
    uint8_t *a_name = NULL;
    size_t len;
    size_t node_vec_len;
    vec_writer_t vw[1];

#define CLEANUP_UTF8()                                                                                                             \
    do {                                                                                                                           \
        (void)edf_atom_text_drop_name(&c_name);                                                                                    \
    } while (0)

#define CLEANUP_LATIN1()                                                                                                           \
    do {                                                                                                                           \
        (void)enif_free((void *)a_name);                                                                                           \
    } while (0)

    if (edf_atom_translation_table_get_entry(&ext->attab, ext->channel->sysname, &internal_index)) {
        node_vec_len = 1 + 1;
        if (!vec_create_owned(node_vec, node_vec_len)) {
            return 0;
        }
        if (!vec_writer_create(vw, node_vec, 0)) {
            return 0;
        }
        if (!vec_writer_write_u8(vw, ATOM_CACHE_REF)) {
            return 0;
        }
        if (!vec_writer_write_u8(vw, (uint8_t)(internal_index))) {
            return 0;
        }
        return 1;
    } else if (edf_atom_text_get_name(ext->channel->sysname, ERTS_ATOM_ENC_UTF8, &c_name, &len)) {
        if (len > 255) {
            node_vec_len = 1 + 2 + len;
        } else {
            node_vec_len = 1 + 1 + len;
        }
        if (!vec_create_owned(node_vec, node_vec_len)) {
            CLEANUP_UTF8();
            return 0;
        }
        if (!vec_writer_create(vw, node_vec, 0)) {
            CLEANUP_UTF8();
            return 0;
        }
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
        if (vec_is_writable(node_vec)) {
            CLEANUP_UTF8();
            return 0;
        }
        CLEANUP_UTF8();
        return 1;
    } else {
        unsigned int ui_len;
        if (!enif_get_atom_length(caller_env, ext->channel->sysname, &ui_len, ERL_NIF_LATIN1)) {
            return 0;
        }
        len = (size_t)(ui_len);
        if (len > 255) {
            node_vec_len = 1 + 2 + len;
        } else {
            node_vec_len = 1 + 1 + len;
        }
        a_name = (void *)enif_alloc(len + 1);
        if (a_name == NULL) {
            return 0;
        }
        if (!enif_get_atom(caller_env, ext->channel->sysname, (char *)a_name, ui_len + 1, ERL_NIF_LATIN1)) {
            CLEANUP_LATIN1();
            return 0;
        }
        if (!vec_create_owned(node_vec, node_vec_len)) {
            CLEANUP_LATIN1();
            return 0;
        }
        if (!vec_writer_create(vw, node_vec, 0)) {
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
        if (vec_is_writable(node_vec)) {
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
