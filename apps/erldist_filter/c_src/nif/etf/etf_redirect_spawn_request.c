/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "etf_redirect_spawn_request.h"

#include "etf_decode.h"
#include "etf_redirect_dop.h"

static const uint8_t c_tuple3_mfa[14] = {
    SMALL_TUPLE_EXT, 3, SMALL_ATOM_UTF8_EXT, 3, 'e', 'd', 'f', SMALL_ATOM_UTF8_EXT, 3, 'r', 'e', 'q', SMALL_INTEGER_EXT, 3};
static const uint8_t c_list3_header[5] = {LIST_EXT, 0, 0, 0, 3};
static const uint8_t c_list3_footer[1] = {NIL_EXT};

int
etf_redirect_spawn_request(ErlNifEnv *caller_env, edf_external_t *ext, ERL_NIF_TERM *err_termp)
{
    vec_t node_vec[1];
    vec_t vec[1];
    size_t mfa_offset = 0;
    size_t mfa_length = 0;
    size_t new_length = 0;
    slice_t old_framing[1] = {SLICE_INIT_EMPTY()};
    slice_t old_headers[1] = {SLICE_INIT_EMPTY()};
    slice_t old_control[1] = {SLICE_INIT_EMPTY()};
    slice_t old_mfa[1] = {SLICE_INIT_EMPTY()};
    slice_t old_payload[1] = {SLICE_INIT_EMPTY()};

#define CLEANUP()                                                                                                                  \
    do {                                                                                                                           \
        (void)vec_destroy(node_vec);                                                                                               \
        (void)vec_destroy(vec);                                                                                                    \
    } while (0)

#define WRITER_RAW_BYTES() (vec_writer_raw_bytes(vw))

#define READER_RAW_BYTES() (vec_reader_raw_bytes(vr))

#define READER_SKIP(skip)                                                                                                          \
    do {                                                                                                                           \
        if (!vec_reader_skip_exact(vr, (skip))) {                                                                                  \
            CLEANUP();                                                                                                             \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_skip_exact() failed: unable to redirect spawn request\n");     \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define WRITE_U8(val)                                                                                                              \
    do {                                                                                                                           \
        if (!vec_writer_write_u8(vw, (val))) {                                                                                     \
            CLEANUP();                                                                                                             \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_writer_write_u8() failed: unable to redirect spawn request\n");       \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define WRITE_BYTES(buf, len)                                                                                                      \
    do {                                                                                                                           \
        if (!vec_writer_write_exact(vw, (buf), (len))) {                                                                           \
            CLEANUP();                                                                                                             \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_writer_write_exact() failed: unable to redirect spawn request\n");    \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

    (void)vec_init_free(node_vec);
    (void)vec_init_free(vec);

    if (ext->up->info.dop != DOP_SPAWN_REQUEST && ext->up->info.dop != DOP_SPAWN_REQUEST_TT) {
        CLEANUP();
        *err_termp = EXCP_ERROR(
            caller_env,
            "Call to etf_redirect_spawn_request() failed: only DOP_SPAWN_REQUEST or DOP_SPAWN_REQUEST_TT may be redirected\n");
        return 0;
    }

    old_framing->head = vec_buf(&ext->primary->vec);
    old_framing->tail = vec_buf(&ext->slices.headers.vec);
    if (old_framing->tail == NULL) {
        old_framing->head = NULL;
    }
    old_headers->head = vec_buf(&ext->slices.headers.vec);
    old_headers->tail = vec_buf_tail(&ext->slices.headers.vec);
    old_control->head = vec_buf(&ext->slices.control.vec);
    old_control->tail = vec_buf_tail(&ext->slices.control.vec);
    old_payload->head = vec_buf(&ext->slices.payload.vec);
    old_payload->tail = vec_buf_tail(&ext->slices.payload.vec);

    if (!etf_redirect_dop_resolve_node(caller_env, ext, node_vec)) {
        CLEANUP();
        *err_termp = EXCP_ERROR(caller_env, "Call to etf_redirect_dop_resolve_node() failed: unable to redirect spawn request\n");
        return 0;
    }

    do {
        vec_t control_slice;
        vec_reader_t vr[1];
        bool is_external_term = false;
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
        if (arity != 6 && arity != 7) {
            CLEANUP();
            *err_termp =
                EXCP_ERROR(caller_env, "Call to etf_redirect_spawn_request() failed: control message tuple arity must be 6 or 7\n");
            return 0;
        }
        if (!etf_fast_skip_terms(caller_env, false, vr, 4, err_termp)) {
            CLEANUP();
            return 0;
        }
        old_mfa->head = READER_RAW_BYTES();
        if (!etf_fast_skip_terms(caller_env, false, vr, 1, err_termp)) {
            CLEANUP();
            return 0;
        }
        old_mfa->tail = READER_RAW_BYTES();
        mfa_offset = (size_t)(old_mfa->head - old_control->head);
        mfa_length = slice_len(old_mfa);
        if (old_framing->head == NULL || old_headers->head == NULL) {
            mfa_offset += 1;
        }
        (void)vec_reader_destroy(vr);
        (void)vec_destroy(&control_slice);
    } while (0);

    new_length =
        vec_len(&ext->primary->vec) + vec_len(node_vec) + sizeof(c_tuple3_mfa) + sizeof(c_list3_header) + sizeof(c_list3_footer);

    do {
        vec_reader_t vr[1];
        vec_writer_t vw[1];
        slice_t new_framing[1];
        slice_t new_headers[1];
        slice_t new_control[1];
        slice_t new_payload[1];
        size_t remaining_bytes_to_write;

        if (!vec_reader_create(vr, &ext->primary->vec, 0)) {
            CLEANUP();
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_create() failed: unable to redirect spawn request\n");
            return 0;
        }
        if (!vec_create_owned(vec, new_length)) {
            CLEANUP();
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_create_owned() failed: unable to redirect spawn request\n");
            return 0;
        }
        if (!vec_writer_create(vw, vec, 0)) {
            CLEANUP();
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_writer_create() failed: unable to redirect spawn request\n");
            return 0;
        }
        // BEGIN
        // WRITE [framing]
        new_framing->head = WRITER_RAW_BYTES();
        if (old_framing->head != NULL && slice_len(old_framing) > 0) {
            WRITE_BYTES(old_framing->head, slice_len(old_framing));
            READER_SKIP(slice_len(old_framing));
        }
        new_framing->tail = WRITER_RAW_BYTES();
        // WRITE [headers]
        new_headers->head = WRITER_RAW_BYTES();
        if (old_headers->head != NULL && slice_len(old_headers) > 0) {
            WRITE_BYTES(old_headers->head, slice_len(old_headers));
            READER_SKIP(slice_len(old_headers));
        }
        new_headers->tail = WRITER_RAW_BYTES();
        // WRITE [control]: (old_control->head[0]...old_control->head[mfa_offset])
        new_control->head = WRITER_RAW_BYTES();
        WRITE_BYTES(READER_RAW_BYTES(), mfa_offset);
        READER_SKIP(mfa_offset);
        // WRITE [control]: c_tuple3_mfa
        WRITE_BYTES(c_tuple3_mfa, sizeof(c_tuple3_mfa));
        READER_SKIP(mfa_length);
        // WRITE [control]: (old_control->head[mfa_offset + mfa_length]...old_control->tail)
        remaining_bytes_to_write = (size_t)(old_control->tail - READER_RAW_BYTES());
        WRITE_BYTES(READER_RAW_BYTES(), remaining_bytes_to_write);
        READER_SKIP(remaining_bytes_to_write);
        new_control->tail = WRITER_RAW_BYTES();
        // WRITE [payload]: VERSION_MAGIC (maybe)
        new_payload->head = WRITER_RAW_BYTES();
        if (edf_external_is_pass_through(ext)) {
            old_payload->head += 1;
            WRITE_U8(VERSION_MAGIC);
        }
        // WRITE [payload]: c_list3_header
        WRITE_BYTES(c_list3_header, sizeof(c_list3_header));
        // WRITE [payload]: elements[0] -> node_vec
        WRITE_BYTES(vec_buf(node_vec), vec_len(node_vec));
        // WRITE [payload]: elements[1] -> old_mfa
        WRITE_BYTES(old_mfa->head, slice_len(old_mfa));
        // WRITE [payload]: elements[2] -> old_payload
        WRITE_BYTES(old_payload->head, slice_len(old_payload));
        // WRITE [payload]: tail -> c_list3_footer
        WRITE_BYTES(c_list3_footer, sizeof(c_list3_footer));
        new_payload->tail = WRITER_RAW_BYTES();
        // END
        if (vec_is_writable(vec)) {
            *err_termp = EXCP_ERROR_F(caller_env,
                                      "Call to etf_redirect_spawn_request() failed: vec is still writable after rewriting the "
                                      "spawn request (len=%llu, remaining_writable_bytes=%llu)\n",
                                      vec_len(vec), vec_remaining_writable_bytes(vec));
            CLEANUP();
            return 0;
        }
        (void)vec_destroy(&ext->primary->vec);
        if (!vec_move(&ext->primary->vec, vec)) {
            CLEANUP();
            *err_termp = EXCP_ERROR(
                caller_env, "Call to etf_redirect_spawn_request() failed: unable to move vec after rewriting the spawn request\n");
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
#undef READER_SKIP
#undef READER_RAW_BYTES
#undef WRITER_RAW_BYTES
#undef CLEANUP
}
