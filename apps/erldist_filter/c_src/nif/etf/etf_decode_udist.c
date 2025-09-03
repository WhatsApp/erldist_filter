/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "etf_decode_udist.h"
#include "etf_decode_term_length.h"
#include "etf_decode_vterm.h"

#include "../core/xnif_trace.h"

int
etf_decode_udist_control(ErlNifEnv *caller_env, vterm_env_t *vtenv, bool is_external, bool skip_slow_terms, vec_t *slice,
                         udist_t *up, ERL_NIF_TERM *err_termp)
{
    vec_reader_t vr[1];
    uint8_t tag;
    uint32_t arity;
    int32_t dop_code;

#define RAW_BYTES() (void *)(vec_reader_raw_bytes(vr))

#define READ_U8(val)                                                                                                               \
    do {                                                                                                                           \
        if (!vec_reader_read_u8(vr, (val))) {                                                                                      \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_read_u8() failed: unable to decode dist operation\n");         \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define READ_I32(val)                                                                                                              \
    do {                                                                                                                           \
        if (!vec_reader_read_i32(vr, (val))) {                                                                                     \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_read_i32() failed: unable to decode dist operation\n");        \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define READ_U32(val)                                                                                                              \
    do {                                                                                                                           \
        if (!vec_reader_read_u32(vr, (val))) {                                                                                     \
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_read_u32() failed: unable to decode dist operation\n");        \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define SKIP_TERMS(skip)                                                                                                           \
    do {                                                                                                                           \
        if (!etf_fast_skip_terms(caller_env, false, vr, (skip), err_termp)) {                                                      \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define READ_ATOM_TERM(atomp)                                                                                                      \
    do {                                                                                                                           \
        if (!etf_decode_atom_term(caller_env, vtenv, false, vr, (atomp), err_termp)) {                                             \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define READ_PID_TERM(pidp)                                                                                                        \
    do {                                                                                                                           \
        if (!etf_decode_pid_term(caller_env, vtenv, false, vr, (pidp), err_termp)) {                                               \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define SKIP_PID_TERM(pidp)                                                                                                        \
    do {                                                                                                                           \
        if (!etf_skip_pid_term(caller_env, vtenv, false, vr, (pidp), err_termp)) {                                                 \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define READ_REFERENCE_TERM(refp)                                                                                                  \
    do {                                                                                                                           \
        if (!etf_decode_reference_term(caller_env, vtenv, false, vr, (refp), err_termp)) {                                         \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define SKIP_REFERENCE_TERM(refp)                                                                                                  \
    do {                                                                                                                           \
        if (!etf_skip_reference_term(caller_env, vtenv, false, vr, (refp), err_termp)) {                                           \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define READ_FIXED_INTEGER(tagp, integerp)                                                                                         \
    do {                                                                                                                           \
        READ_U8((tagp));                                                                                                           \
        switch (*(tagp)) {                                                                                                         \
        case SMALL_INTEGER_EXT: {                                                                                                  \
            uint8_t small_integer;                                                                                                 \
            READ_U8(&small_integer);                                                                                               \
            *(integerp) = (int32_t)small_integer;                                                                                  \
            break;                                                                                                                 \
        }                                                                                                                          \
        case INTEGER_EXT: {                                                                                                        \
            READ_I32((integerp));                                                                                                  \
            break;                                                                                                                 \
        }                                                                                                                          \
        default:                                                                                                                   \
            *err_termp = EXCP_ERROR_F(caller_env,                                                                                  \
                                      "Call to etf_decode_udist_control() failed: expected tag=%u to be SMALL_INTEGER_EXT=%u or "  \
                                      "INTEGER_EXT=%u for internal term\n",                                                        \
                                      *(tagp), SMALL_INTEGER_EXT, INTEGER_EXT);                                                    \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define READ_TUPLE_HEADER(tagp, arityp)                                                                                            \
    do {                                                                                                                           \
        READ_U8((tagp));                                                                                                           \
        switch (*(tagp)) {                                                                                                         \
        case SMALL_TUPLE_EXT: {                                                                                                    \
            uint8_t small_arity;                                                                                                   \
            READ_U8(&small_arity);                                                                                                 \
            *(arityp) = (uint32_t)small_arity;                                                                                     \
            break;                                                                                                                 \
        }                                                                                                                          \
        case LARGE_TUPLE_EXT: {                                                                                                    \
            READ_U32((arityp));                                                                                                    \
            break;                                                                                                                 \
        }                                                                                                                          \
        default:                                                                                                                   \
            *err_termp = EXCP_ERROR_F(caller_env,                                                                                  \
                                      "Call to etf_decode_udist_control() failed: expected tag=%u to be SMALL_TUPLE_EXT=%u or "    \
                                      "LARGE_TUPLE_EXT=%u for internal term\n",                                                    \
                                      *(tagp), SMALL_TUPLE_EXT, LARGE_TUPLE_EXT);                                                  \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

    if (!vec_reader_create(vr, slice, 0)) {
        return 0;
    }

    if (is_external) {
        uint8_t version_magic;
        READ_U8(&version_magic);
        if (version_magic != VERSION_MAGIC) {
            *err_termp = EXCP_ERROR_F(
                caller_env,
                "Call to etf_decode_udist_control() failed: expected version_magic=%u to be VERSION_MAGIC=%u for external term\n",
                version_magic, VERSION_MAGIC);
            return 0;
        }
    }

    READ_TUPLE_HEADER(&tag, &arity);

    if (arity == 0) {
        *err_termp =
            EXCP_ERROR_F(caller_env, "Call to etf_decode_udist_control() failed: expected arity=%u to be greater than 0\n", arity);
        return 0;
    }

    READ_FIXED_INTEGER(&tag, &dop_code);

    switch (dop_code) {
    case DOP_LINK:
        if (arity != 3) {
            *err_termp = EXCP_ERROR_F(
                caller_env, "Call to etf_decode_udist_control() failed: expected tuple arity=%u to be exactly 3 for DOP_LINK\n",
                arity);
            return 0;
        }
        up->info.dop = DOP_LINK;
        up->info.arity = arity;
        up->info.token_offset = -1;
        up->info.payload = false;
        up->control.tag = UDIST_CONTROL_TAG_LINK;
        break;
    case DOP_SEND:
        if (arity != 3) {
            *err_termp = EXCP_ERROR_F(
                caller_env, "Call to etf_decode_udist_control() failed: expected tuple arity=%u to be exactly 3 for DOP_SEND\n",
                arity);
            return 0;
        }
        up->info.dop = DOP_SEND;
        up->info.arity = arity;
        up->info.token_offset = -1;
        up->info.payload = true;
        SKIP_TERMS(1);
        if (skip_slow_terms) {
            SKIP_PID_TERM(&up->control.data.send.to);
        } else {
            READ_PID_TERM(&up->control.data.send.to);
        }
        up->control.tag = UDIST_CONTROL_TAG_SEND_TO_PID;
        break;
    case DOP_EXIT:
        if (arity != 4) {
            *err_termp = EXCP_ERROR_F(
                caller_env, "Call to etf_decode_udist_control() failed: expected tuple arity=%u to be exactly 4 for DOP_EXIT\n",
                arity);
            return 0;
        }
        up->info.dop = DOP_EXIT;
        up->info.arity = arity;
        up->info.token_offset = -1;
        up->info.payload = false;
        up->control.tag = UDIST_CONTROL_TAG_EXIT;
        break;
    case DOP_UNLINK:
        if (arity != 3) {
            *err_termp = EXCP_ERROR_F(
                caller_env, "Call to etf_decode_udist_control() failed: expected tuple arity=%u to be exactly 3 for DOP_UNLINK\n",
                arity);
            return 0;
        }
        up->info.dop = DOP_UNLINK;
        up->info.arity = arity;
        up->info.token_offset = -1;
        up->info.payload = false;
        up->control.tag = UDIST_CONTROL_TAG_UNLINK;
        break;
    case DOP_REG_SEND:
        if (arity != 4) {
            *err_termp = EXCP_ERROR_F(
                caller_env, "Call to etf_decode_udist_control() failed: expected tuple arity=%u to be exactly 4 for DOP_REG_SEND\n",
                arity);
            return 0;
        }
        up->info.dop = DOP_REG_SEND;
        up->info.arity = arity;
        up->info.token_offset = -1;
        up->info.payload = true;
        SKIP_TERMS(2);
        READ_ATOM_TERM(&up->control.data.send.to);
        up->control.tag = UDIST_CONTROL_TAG_SEND_TO_NAME;
        break;
    case DOP_GROUP_LEADER:
        if (arity != 3) {
            *err_termp = EXCP_ERROR_F(
                caller_env,
                "Call to etf_decode_udist_control() failed: expected tuple arity=%u to be exactly 3 for DOP_GROUP_LEADER\n", arity);
            return 0;
        }
        up->info.dop = DOP_GROUP_LEADER;
        up->info.arity = arity;
        up->info.token_offset = -1;
        up->info.payload = false;
        up->control.tag = UDIST_CONTROL_TAG_GROUP_LEADER;
        break;
    case DOP_EXIT2:
        if (arity != 4) {
            *err_termp = EXCP_ERROR_F(
                caller_env, "Call to etf_decode_udist_control() failed: expected tuple arity=%u to be exactly 4 for DOP_EXIT2\n",
                arity);
            return 0;
        }
        up->info.dop = DOP_EXIT2;
        up->info.arity = arity;
        up->info.token_offset = -1;
        up->info.payload = false;
        up->control.tag = UDIST_CONTROL_TAG_EXIT2;
        break;
    case DOP_SEND_TT:
        if (arity != 4) {
            *err_termp = EXCP_ERROR_F(
                caller_env, "Call to etf_decode_udist_control() failed: expected tuple arity=%u to be exactly 4 for DOP_SEND_TT\n",
                arity);
            return 0;
        }
        up->info.dop = DOP_SEND_TT;
        up->info.arity = arity;
        up->info.token_offset = 3;
        up->info.payload = true;
        SKIP_TERMS(1);
        if (skip_slow_terms) {
            SKIP_PID_TERM(&up->control.data.send.to);
        } else {
            READ_PID_TERM(&up->control.data.send.to);
        }
        up->control.tag = UDIST_CONTROL_TAG_SEND_TO_PID;
        break;
    case DOP_EXIT_TT:
        if (arity != 5) {
            *err_termp = EXCP_ERROR_F(
                caller_env, "Call to etf_decode_udist_control() failed: expected tuple arity=%u to be exactly 5 for DOP_EXIT_TT\n",
                arity);
            return 0;
        }
        up->info.dop = DOP_EXIT_TT;
        up->info.arity = arity;
        up->info.token_offset = 3;
        up->info.payload = false;
        up->control.tag = UDIST_CONTROL_TAG_EXIT;
        break;
    case DOP_REG_SEND_TT:
        if (arity != 5) {
            *err_termp = EXCP_ERROR_F(
                caller_env,
                "Call to etf_decode_udist_control() failed: expected tuple arity=%u to be exactly 5 for DOP_REG_SEND_TT\n", arity);
            return 0;
        }
        up->info.dop = DOP_REG_SEND_TT;
        up->info.arity = arity;
        up->info.token_offset = 4;
        up->info.payload = true;
        SKIP_TERMS(2);
        READ_ATOM_TERM(&up->control.data.send.to);
        up->control.tag = UDIST_CONTROL_TAG_SEND_TO_NAME;
        break;
    case DOP_EXIT2_TT:
        if (arity != 5) {
            *err_termp = EXCP_ERROR_F(
                caller_env, "Call to etf_decode_udist_control() failed: expected tuple arity=%u to be exactly 5 for DOP_EXIT2_TT\n",
                arity);
            return 0;
        }
        up->info.dop = DOP_EXIT2_TT;
        up->info.arity = arity;
        up->info.token_offset = 3;
        up->info.payload = false;
        up->control.tag = UDIST_CONTROL_TAG_EXIT2;
        break;
    case DOP_MONITOR_P:
        if (arity != 4) {
            *err_termp = EXCP_ERROR_F(
                caller_env,
                "Call to etf_decode_udist_control() failed: expected tuple arity=%u to be exactly 4 for DOP_MONITOR_P\n", arity);
            return 0;
        }
        up->info.dop = DOP_MONITOR_P;
        up->info.arity = arity;
        up->info.token_offset = -1;
        up->info.payload = false;
        up->control.tag = UDIST_CONTROL_TAG_MONITOR_RELATED;
        break;
    case DOP_DEMONITOR_P:
        if (arity != 4) {
            *err_termp = EXCP_ERROR_F(
                caller_env,
                "Call to etf_decode_udist_control() failed: expected tuple arity=%u to be exactly 4 for DOP_DEMONITOR_P\n", arity);
            return 0;
        }
        up->info.dop = DOP_DEMONITOR_P;
        up->info.arity = arity;
        up->info.token_offset = -1;
        up->info.payload = false;
        up->control.tag = UDIST_CONTROL_TAG_MONITOR_RELATED;
        break;
    case DOP_MONITOR_P_EXIT:
        if (arity != 5) {
            *err_termp = EXCP_ERROR_F(
                caller_env,
                "Call to etf_decode_udist_control() failed: expected tuple arity=%u to be exactly 5 for DOP_MONITOR_P_EXIT\n",
                arity);
            return 0;
        }
        up->info.dop = DOP_MONITOR_P_EXIT;
        up->info.arity = arity;
        up->info.token_offset = -1;
        up->info.payload = false;
        up->control.tag = UDIST_CONTROL_TAG_MONITOR_RELATED;
        break;
    case DOP_SEND_SENDER:
        if (arity != 3) {
            *err_termp = EXCP_ERROR_F(
                caller_env,
                "Call to etf_decode_udist_control() failed: expected tuple arity=%u to be exactly 3 for DOP_SEND_SENDER\n", arity);
            return 0;
        }
        up->info.dop = DOP_SEND_SENDER;
        up->info.arity = arity;
        up->info.token_offset = -1;
        up->info.payload = true;
        SKIP_TERMS(1);
        if (skip_slow_terms) {
            SKIP_PID_TERM(&up->control.data.send.to);
        } else {
            READ_PID_TERM(&up->control.data.send.to);
        }
        up->control.tag = UDIST_CONTROL_TAG_SEND_TO_PID;
        break;
    case DOP_SEND_SENDER_TT:
        if (arity != 4) {
            *err_termp = EXCP_ERROR_F(
                caller_env,
                "Call to etf_decode_udist_control() failed: expected tuple arity=%u to be exactly 4 for DOP_SEND_SENDER_TT\n",
                arity);
            return 0;
        }
        up->info.dop = DOP_SEND_SENDER_TT;
        up->info.arity = arity;
        up->info.token_offset = 3;
        up->info.payload = true;
        SKIP_TERMS(1);
        if (skip_slow_terms) {
            SKIP_PID_TERM(&up->control.data.send.to);
        } else {
            READ_PID_TERM(&up->control.data.send.to);
        }
        up->control.tag = UDIST_CONTROL_TAG_SEND_TO_PID;
        break;
    case DOP_PAYLOAD_EXIT:
        if (arity != 3) {
            *err_termp = EXCP_ERROR_F(
                caller_env,
                "Call to etf_decode_udist_control() failed: expected tuple arity=%u to be exactly 3 for DOP_PAYLOAD_EXIT\n", arity);
            return 0;
        }
        up->info.dop = DOP_PAYLOAD_EXIT;
        up->info.arity = arity;
        up->info.token_offset = -1;
        up->info.payload = true;
        up->control.tag = UDIST_CONTROL_TAG_EXIT;
        break;
    case DOP_PAYLOAD_EXIT_TT:
        if (arity != 4) {
            *err_termp = EXCP_ERROR_F(
                caller_env,
                "Call to etf_decode_udist_control() failed: expected tuple arity=%u to be exactly 4 for DOP_PAYLOAD_EXIT_TT\n",
                arity);
            return 0;
        }
        up->info.dop = DOP_PAYLOAD_EXIT_TT;
        up->info.arity = arity;
        up->info.token_offset = 3;
        up->info.payload = true;
        up->control.tag = UDIST_CONTROL_TAG_EXIT;
        break;
    case DOP_PAYLOAD_EXIT2:
        if (arity != 3) {
            *err_termp = EXCP_ERROR_F(
                caller_env,
                "Call to etf_decode_udist_control() failed: expected tuple arity=%u to be exactly 3 for DOP_PAYLOAD_EXIT2\n",
                arity);
            return 0;
        }
        up->info.dop = DOP_PAYLOAD_EXIT2;
        up->info.arity = arity;
        up->info.token_offset = -1;
        up->info.payload = true;
        up->control.tag = UDIST_CONTROL_TAG_EXIT2;
        break;
    case DOP_PAYLOAD_EXIT2_TT:
        if (arity != 4) {
            *err_termp = EXCP_ERROR_F(
                caller_env,
                "Call to etf_decode_udist_control() failed: expected tuple arity=%u to be exactly 4 for DOP_PAYLOAD_EXIT2_TT\n",
                arity);
            return 0;
        }
        up->info.dop = DOP_PAYLOAD_EXIT2_TT;
        up->info.arity = arity;
        up->info.token_offset = 3;
        up->info.payload = true;
        up->control.tag = UDIST_CONTROL_TAG_EXIT2;
        break;
    case DOP_PAYLOAD_MONITOR_P_EXIT:
        if (arity != 4) {
            *err_termp = EXCP_ERROR_F(caller_env,
                                      "Call to etf_decode_udist_control() failed: expected tuple arity=%u to be exactly 4 for "
                                      "DOP_PAYLOAD_MONITOR_P_EXIT\n",
                                      arity);
            return 0;
        }
        up->info.dop = DOP_PAYLOAD_MONITOR_P_EXIT;
        up->info.arity = arity;
        up->info.token_offset = -1;
        up->info.payload = true;
        up->control.tag = UDIST_CONTROL_TAG_MONITOR_RELATED;
        break;
    case DOP_SPAWN_REQUEST:
        if (arity != 6) {
            *err_termp = EXCP_ERROR_F(
                caller_env,
                "Call to etf_decode_udist_control() failed: expected tuple arity=%u to be exactly 6 for DOP_SPAWN_REQUEST\n",
                arity);
            return 0;
        }
        up->info.dop = DOP_SPAWN_REQUEST;
        up->info.arity = arity;
        up->info.token_offset = -1;
        up->info.payload = true;
        SKIP_TERMS(3);
        READ_TUPLE_HEADER(&tag, &arity);
        if (arity != 3) {
            *err_termp =
                EXCP_ERROR_F(caller_env, "Call to etf_decode_udist_control() failed: expected arity=%u to be exactly 3\n", arity);
            return 0;
        }
        READ_ATOM_TERM(&up->control.data.spawn_request.module);
        READ_ATOM_TERM(&up->control.data.spawn_request.function);
        READ_FIXED_INTEGER(&tag, &up->control.data.spawn_request.arity);
        up->control.tag = UDIST_CONTROL_TAG_SPAWN_REQUEST;
        break;
    case DOP_SPAWN_REQUEST_TT:
        if (arity != 7) {
            *err_termp = EXCP_ERROR_F(
                caller_env,
                "Call to etf_decode_udist_control() failed: expected tuple arity=%u to be exactly 7 for DOP_SPAWN_REQUEST_TT\n",
                arity);
            return 0;
        }
        up->info.dop = DOP_SPAWN_REQUEST_TT;
        up->info.arity = arity;
        up->info.token_offset = 6;
        up->info.payload = true;
        SKIP_TERMS(3);
        READ_TUPLE_HEADER(&tag, &arity);
        if (arity != 3) {
            *err_termp =
                EXCP_ERROR_F(caller_env, "Call to etf_decode_udist_control() failed: expected arity=%u to be exactly 3\n", arity);
            return 0;
        }
        READ_ATOM_TERM(&up->control.data.spawn_request.module);
        READ_ATOM_TERM(&up->control.data.spawn_request.function);
        READ_FIXED_INTEGER(&tag, &up->control.data.spawn_request.arity);
        up->control.tag = UDIST_CONTROL_TAG_SPAWN_REQUEST;
        break;
    case DOP_SPAWN_REPLY:
        if (arity != 5) {
            *err_termp = EXCP_ERROR_F(
                caller_env,
                "Call to etf_decode_udist_control() failed: expected tuple arity=%u to be exactly 5 for DOP_SPAWN_REPLY\n", arity);
            return 0;
        }
        up->info.dop = DOP_SPAWN_REPLY;
        up->info.arity = arity;
        up->info.token_offset = -1;
        up->info.payload = false;
        up->control.tag = UDIST_CONTROL_TAG_SPAWN_REPLY;
        break;
    case DOP_SPAWN_REPLY_TT:
        if (arity != 6) {
            *err_termp = EXCP_ERROR_F(
                caller_env,
                "Call to etf_decode_udist_control() failed: expected tuple arity=%u to be exactly 6 for DOP_SPAWN_REPLY_TT\n",
                arity);
            return 0;
        }
        up->info.dop = DOP_SPAWN_REPLY_TT;
        up->info.arity = arity;
        up->info.token_offset = 5;
        up->info.payload = false;
        up->control.tag = UDIST_CONTROL_TAG_SPAWN_REPLY;
        break;
    case DOP_ALIAS_SEND:
        if (arity != 3) {
            *err_termp = EXCP_ERROR_F(
                caller_env,
                "Call to etf_decode_udist_control() failed: expected tuple arity=%u to be exactly 3 for DOP_ALIAS_SEND\n", arity);
            return 0;
        }
        up->info.dop = DOP_ALIAS_SEND;
        up->info.arity = arity;
        up->info.token_offset = -1;
        up->info.payload = true;
        SKIP_TERMS(1);
        if (skip_slow_terms) {
            SKIP_REFERENCE_TERM(&up->control.data.send.to);
        } else {
            READ_REFERENCE_TERM(&up->control.data.send.to);
        }
        up->control.tag = UDIST_CONTROL_TAG_SEND_TO_ALIAS;
        break;
    case DOP_ALIAS_SEND_TT:
        if (arity != 4) {
            *err_termp = EXCP_ERROR_F(
                caller_env,
                "Call to etf_decode_udist_control() failed: expected tuple arity=%u to be exactly 4 for DOP_ALIAS_SEND_TT\n",
                arity);
            return 0;
        }
        up->info.dop = DOP_ALIAS_SEND_TT;
        up->info.arity = arity;
        up->info.token_offset = 3;
        up->info.payload = true;
        SKIP_TERMS(1);
        if (skip_slow_terms) {
            SKIP_REFERENCE_TERM(&up->control.data.send.to);
        } else {
            READ_REFERENCE_TERM(&up->control.data.send.to);
        }
        up->control.tag = UDIST_CONTROL_TAG_SEND_TO_ALIAS;
        break;
    case DOP_UNLINK_ID:
        if (arity != 4) {
            *err_termp = EXCP_ERROR_F(
                caller_env,
                "Call to etf_decode_udist_control() failed: expected tuple arity=%u to be exactly 4 for DOP_UNLINK_ID\n", arity);
            return 0;
        }
        up->info.dop = DOP_UNLINK_ID;
        up->info.arity = arity;
        up->info.token_offset = -1;
        up->info.payload = false;
        up->control.tag = UDIST_CONTROL_TAG_UNLINK;
        break;
    case DOP_UNLINK_ID_ACK:
        if (arity != 4) {
            *err_termp = EXCP_ERROR_F(
                caller_env,
                "Call to etf_decode_udist_control() failed: expected tuple arity=%u to be exactly 4 for DOP_UNLINK_ID_ACK\n",
                arity);
            return 0;
        }
        up->info.dop = DOP_UNLINK_ID_ACK;
        up->info.arity = arity;
        up->info.token_offset = -1;
        up->info.payload = false;
        up->control.tag = UDIST_CONTROL_TAG_UNLINK;
        break;
    case DOP_ALTACT_SIG_SEND:
        if (arity != 4 && arity != 5) {
            *err_termp = EXCP_ERROR_F(
                caller_env,
                "Call to etf_decode_udist_control() failed: expected tuple arity=%u to be either 4 or 5 for DOP_ALTACT_SIG_SEND\n",
                arity);
            return 0;
        }
        up->info.dop = DOP_ALTACT_SIG_SEND;
        up->info.arity = arity;
        if (arity == 5) {
            up->info.token_offset = 4;
        } else {
            up->info.token_offset = -1;
        }
        up->info.payload = true;
        READ_FIXED_INTEGER(&tag, &up->control.data.send.flags);
        SKIP_TERMS(1);
        if ((up->control.data.send.flags & ERTS_DOP_ALTACT_SIG_FLG_EXIT) == ERTS_DOP_ALTACT_SIG_FLG_EXIT) {
            up->control.tag = UDIST_CONTROL_TAG_EXIT2;
            if ((up->control.data.send.flags & ERTS_DOP_ALTACT_SIG_FLG_ALIAS) == ERTS_DOP_ALTACT_SIG_FLG_ALIAS) {
                if (skip_slow_terms) {
                    SKIP_REFERENCE_TERM(&up->control.data.send.to);
                } else {
                    READ_REFERENCE_TERM(&up->control.data.send.to);
                }
            } else {
                if (skip_slow_terms) {
                    SKIP_PID_TERM(&up->control.data.send.to);
                } else {
                    READ_PID_TERM(&up->control.data.send.to);
                }
            }
        } else if ((up->control.data.send.flags & ERTS_DOP_ALTACT_SIG_FLG_NAME) == ERTS_DOP_ALTACT_SIG_FLG_NAME) {
            up->control.tag = UDIST_CONTROL_TAG_SEND_TO_NAME;
            READ_ATOM_TERM(&up->control.data.send.to);
        } else if ((up->control.data.send.flags & ERTS_DOP_ALTACT_SIG_FLG_ALIAS) == ERTS_DOP_ALTACT_SIG_FLG_ALIAS) {
            up->control.tag = UDIST_CONTROL_TAG_SEND_TO_ALIAS;
            if (skip_slow_terms) {
                SKIP_REFERENCE_TERM(&up->control.data.send.to);
            } else {
                READ_REFERENCE_TERM(&up->control.data.send.to);
            }
        } else {
            up->control.tag = UDIST_CONTROL_TAG_SEND_TO_PID;
            if (skip_slow_terms) {
                SKIP_PID_TERM(&up->control.data.send.to);
            } else {
                READ_PID_TERM(&up->control.data.send.to);
            }
        }
        break;
    default:
        *err_termp = EXCP_ERROR_F(caller_env, "Call to etf_decode_udist_control() failed: dop_code=%u is unrecognized\n", dop_code);
        return 0;
    }
    (void)vec_reader_destroy(vr);
    return 1;

#undef READ_TUPLE_HEADER
#undef READ_FIXED_INTEGER
#undef SKIP_REFERENCE_TERM
#undef READ_REFERENCE_TERM
#undef SKIP_PID_TERM
#undef READ_PID_TERM
#undef READ_ATOM_TERM
#undef SKIP_TERMS
#undef READ_U32
#undef READ_I32
#undef READ_U8
#undef RAW_BYTES
}
