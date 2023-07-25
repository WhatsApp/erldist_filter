/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "etf_rewrite_as_drop.h"

#include "etf_decode.h"

// Node = vterm_small_atom_utf8_ext:new(0, <<>>),
// Unused = vterm_small_atom_utf8_ext:new(0, <<>>),
// ToName = vterm_small_atom_utf8_ext:new(9, <<"undefined">>),
// FromPid = vterm_new_pid_ext:new(Node, 0, 0, 0),
// RegSend = vdist_dop_reg_send:new(FromPid, Unused, ToName),
// RegSendVTerm = vdist_dop:dop_to_control_message_vterm(RegSend),
// erlang:display(vterm_encode:internal_vterm_to_binary(RegSendVTerm, #{})).

static const uint8_t c_reg_send_control[] = {
    // clang-format off
    SMALL_TUPLE_EXT, 4,
        SMALL_INTEGER_EXT, DOP_REG_SEND,
        NEW_PID_EXT,
            SMALL_ATOM_UTF8_EXT, 0,
            0, 0, 0, 0,
            0, 0, 0, 0,
            0, 0, 0, 0,
        SMALL_ATOM_UTF8_EXT, 0,
        SMALL_ATOM_UTF8_EXT, 9,
            'u', 'n', 'd', 'e', 'f', 'i', 'n', 'e', 'd',
    // clang-format on
};
static const uint8_t c_reg_send_payload[] = {NIL_EXT};

int
etf_rewrite_as_drop(ErlNifEnv *caller_env, edf_external_t *ext, ERL_NIF_TERM *err_termp)
{
    vec_t vec[1];
    size_t new_length = 0;
    slice_t old_framing[1] = {SLICE_INIT_EMPTY()};
    slice_t old_headers[1] = {SLICE_INIT_EMPTY()};
    slice_t old_control[1] = {SLICE_INIT_EMPTY()};
    slice_t old_payload[1] = {SLICE_INIT_EMPTY()};

#define CLEANUP()                                                                                                                  \
    do {                                                                                                                           \
        (void)vec_destroy(vec);                                                                                                    \
    } while (0)

#define RAW_BYTES() (vec_writer_raw_bytes(vw))

#define WRITE_U8(val)                                                                                                              \
    do {                                                                                                                           \
        if (!vec_writer_write_u8(vw, (val))) {                                                                                     \
            CLEANUP();                                                                                                             \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_writer_write_u8() failed: unable to rewrite as drop\n");              \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define WRITE_BYTES(buf, len)                                                                                                      \
    do {                                                                                                                           \
        if (!vec_writer_write_exact(vw, (buf), (len))) {                                                                           \
            CLEANUP();                                                                                                             \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_writer_write_exact() failed: unable to rewrite as drop\n");           \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

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

    new_length += slice_len(old_framing);
    new_length += slice_len(old_headers);
    new_length += sizeof(c_reg_send_control) + sizeof(c_reg_send_payload);
    if (edf_external_is_pass_through(ext)) {
        new_length += 2;
    }

    do {
        vec_writer_t vw[1];
        slice_t new_framing[1];
        slice_t new_headers[1];
        slice_t new_control[1];
        slice_t new_payload[1];

        if (!vec_create_owned(vec, new_length)) {
            CLEANUP();
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_create_owned() failed: unable to rewrite as drop\n");
            return 0;
        }
        if (!vec_writer_create(vw, vec, 0)) {
            CLEANUP();
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_writer_create() failed: unable to rewrite as drop\n");
            return 0;
        }
        new_framing->head = RAW_BYTES();
        if (old_framing->head != NULL && slice_len(old_framing) > 0) {
            WRITE_BYTES(old_framing->head, slice_len(old_framing));
            new_framing->tail = RAW_BYTES();
        }
        new_headers->head = RAW_BYTES();
        if (old_headers->head != NULL && slice_len(old_headers) > 0) {
            WRITE_BYTES(old_headers->head, slice_len(old_headers));
        }
        new_headers->tail = RAW_BYTES();
        new_control->head = RAW_BYTES();
        if (edf_external_is_pass_through(ext)) {
            WRITE_U8(VERSION_MAGIC);
        }
        WRITE_BYTES(c_reg_send_control, sizeof(c_reg_send_control));
        new_control->tail = RAW_BYTES();
        new_payload->head = RAW_BYTES();
        if (edf_external_is_pass_through(ext)) {
            WRITE_U8(VERSION_MAGIC);
        }
        WRITE_BYTES(c_reg_send_payload, sizeof(c_reg_send_payload));
        new_payload->tail = RAW_BYTES();
        if (vec_is_writable(vec)) {
            *err_termp = EXCP_ERROR_F(caller_env,
                                      "Call to etf_rewrite_as_drop() failed: vec is still writable after rewriting as drop "
                                      "(len=%llu, remaining_writable_bytes=%llu)\n",
                                      vec_len(vec), vec_remaining_writable_bytes(vec));
            CLEANUP();
            return 0;
        }
        (void)vec_destroy(&ext->primary->vec);
        if (!vec_move(&ext->primary->vec, vec)) {
            CLEANUP();
            *err_termp =
                EXCP_ERROR(caller_env, "Call to etf_rewrite_as_drop() failed: unable to move vec after rewriting as drop\n");
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
