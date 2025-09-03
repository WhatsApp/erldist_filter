/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 1996-2025. All Rights Reserved.
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * %CopyrightEnd%
 */

#ifndef EDF_ERTS_DIST_H
#define EDF_ERTS_DIST_H

#ifdef __cplusplus
extern "C" {
#endif

/// See [erts/emulator/beam/dist.h](https://github.com/erlang/otp/blob/OTP-28.0.2/erts/emulator/beam/dist.h) in the
/// Erlang/OTP source code.

#define DFLAG_PUBLISHED ((uint64_t)0x01)
#define DFLAG_ATOM_CACHE ((uint64_t)0x02)
#define DFLAG_EXTENDED_REFERENCES ((uint64_t)0x04)
#define DFLAG_DIST_MONITOR ((uint64_t)0x08)
#define DFLAG_FUN_TAGS ((uint64_t)0x10)
#define DFLAG_DIST_MONITOR_NAME ((uint64_t)0x20)
#define DFLAG_HIDDEN_ATOM_CACHE ((uint64_t)0x40)
#define DFLAG_NEW_FUN_TAGS ((uint64_t)0x80)
#define DFLAG_EXTENDED_PIDS_PORTS ((uint64_t)0x100)
#define DFLAG_EXPORT_PTR_TAG ((uint64_t)0x200)
#define DFLAG_BIT_BINARIES ((uint64_t)0x400)
#define DFLAG_NEW_FLOATS ((uint64_t)0x800)
#define DFLAG_UNICODE_IO ((uint64_t)0x1000)
#define DFLAG_DIST_HDR_ATOM_CACHE ((uint64_t)0x2000)
#define DFLAG_SMALL_ATOM_TAGS ((uint64_t)0x4000)
#define DFLAG_ETS_COMPRESSED ((uint64_t)0x8000) /* internal */
#define DFLAG_UTF8_ATOMS ((uint64_t)0x10000)
#define DFLAG_MAP_TAG ((uint64_t)0x20000)
#define DFLAG_BIG_CREATION ((uint64_t)0x40000)
#define DFLAG_SEND_SENDER ((uint64_t)0x80000)
#define DFLAG_BIG_SEQTRACE_LABELS ((uint64_t)0x100000)
#define DFLAG_PENDING_CONNECT ((uint64_t)0x200000) /* internal */
#define DFLAG_EXIT_PAYLOAD ((uint64_t)0x400000)
#define DFLAG_FRAGMENTS ((uint64_t)0x800000)
#define DFLAG_HANDSHAKE_23 ((uint64_t)0x1000000)
#define DFLAG_UNLINK_ID ((uint64_t)0x2000000)
#define DFLAG_MANDATORY_25_DIGEST ((uint64_t)0x4000000)
#define DFLAG_RESERVED ((uint64_t)0xf8000000)
/*
 * As the old handshake only support 32 flag bits, we reserve the remaining
 * bits in the lower 32 for changes in the handshake protocol or potentially
 * new capabilities that we also want to backport to OTP-22 or older.
 */
#define DFLAG_SPAWN (((uint64_t)0x1) << 32)
#define DFLAG_NAME_ME (((uint64_t)0x2) << 32)
#define DFLAG_V4_NC (((uint64_t)0x4) << 32)
#define DFLAG_ALIAS (((uint64_t)0x8) << 32)
#define DFLAG_LOCAL_EXT (((uint64_t)0x10) << 32) /* internal */
#define DFLAG_ALTACT_SIG (((uint64_t)0x20) << 32)
/*
 * In term_to_binary/2, we will use DFLAG_ATOM_CACHE to mean
 * DFLAG_DETERMINISTIC.
 */

#define DFLAG_DETERMINISTIC DFLAG_ATOM_CACHE
/* Mandatory flags for distribution in OTP 25. */
#define DFLAG_DIST_MANDATORY_25                                                                                                    \
    (DFLAG_EXTENDED_REFERENCES | DFLAG_FUN_TAGS | DFLAG_EXTENDED_PIDS_PORTS | DFLAG_UTF8_ATOMS | DFLAG_NEW_FUN_TAGS |              \
     DFLAG_BIG_CREATION | DFLAG_NEW_FLOATS | DFLAG_MAP_TAG | DFLAG_EXPORT_PTR_TAG | DFLAG_BIT_BINARIES | DFLAG_HANDSHAKE_23)
/* New mandatory flags for distribution in OTP 26 */
#define DFLAG_DIST_MANDATORY_26 (DFLAG_V4_NC | DFLAG_UNLINK_ID)

/* Mandatory flags for distribution. */
#define DFLAG_DIST_MANDATORY (DFLAG_DIST_MANDATORY_25 | DFLAG_DIST_MANDATORY_26)

/*
 * Additional optimistic flags when encoding toward pending connection.
 * If remote node (erl_interface) does not support these then we may need
 * to transcode messages enqueued before connection setup was finished.
 */
#define DFLAG_DIST_HOPEFULLY (DFLAG_DIST_MONITOR | DFLAG_DIST_MONITOR_NAME | DFLAG_SPAWN | DFLAG_ALTACT_SIG | DFLAG_ALIAS)

/* Our preferred set of flags. Used for connection setup handshake */
#define DFLAG_DIST_DEFAULT                                                                                                         \
    (DFLAG_DIST_MANDATORY | DFLAG_DIST_HOPEFULLY | DFLAG_UNICODE_IO | DFLAG_DIST_HDR_ATOM_CACHE | DFLAG_SMALL_ATOM_TAGS |          \
     DFLAG_SEND_SENDER | DFLAG_BIG_SEQTRACE_LABELS | DFLAG_EXIT_PAYLOAD | DFLAG_FRAGMENTS | DFLAG_SPAWN | DFLAG_ALIAS |            \
     DFLAG_MANDATORY_25_DIGEST)

/* Flags addable by local distr implementations */
#define DFLAG_DIST_ADDABLE DFLAG_DIST_DEFAULT

/* Flags rejectable by local distr implementation */
#define DFLAG_DIST_REJECTABLE (DFLAG_DIST_HDR_ATOM_CACHE | DFLAG_HIDDEN_ATOM_CACHE | DFLAG_FRAGMENTS | DFLAG_ATOM_CACHE)

/* Flags for all features needing strict order delivery */
#define DFLAG_DIST_STRICT_ORDER DFLAG_DIST_HDR_ATOM_CACHE

/* All flags that should be enabled when term_to_binary/1 is used. */
#define TERM_TO_BINARY_DFLAGS (DFLAG_NEW_FLOATS | DFLAG_UTF8_ATOMS)

/* opcodes used in distribution messages */
enum dop {
    /* This is the default value, and should never be used. */
    DOP_UNKNOWN = 0,
    DOP_LINK = 1,
    DOP_SEND = 2,
    DOP_EXIT = 3,
    DOP_UNLINK = 4,
    /* Ancient DOP_NODE_LINK (5) was here, can be reused */
    DOP_REG_SEND = 6,
    DOP_GROUP_LEADER = 7,
    DOP_EXIT2 = 8,

    DOP_SEND_TT = 12,
    DOP_EXIT_TT = 13,
    DOP_REG_SEND_TT = 16,
    DOP_EXIT2_TT = 18,

    DOP_MONITOR_P = 19,
    DOP_DEMONITOR_P = 20,
    DOP_MONITOR_P_EXIT = 21,

    DOP_SEND_SENDER = 22,
    DOP_SEND_SENDER_TT = 23,

    /* These are used when DFLAG_EXIT_PAYLOAD is detected */
    DOP_PAYLOAD_EXIT = 24,
    DOP_PAYLOAD_EXIT_TT = 25,
    DOP_PAYLOAD_EXIT2 = 26,
    DOP_PAYLOAD_EXIT2_TT = 27,
    DOP_PAYLOAD_MONITOR_P_EXIT = 28,

    DOP_SPAWN_REQUEST = 29,
    DOP_SPAWN_REQUEST_TT = 30,
    DOP_SPAWN_REPLY = 31,
    DOP_SPAWN_REPLY_TT = 32,

    DOP_ALIAS_SEND = 33,
    DOP_ALIAS_SEND_TT = 34,

    DOP_UNLINK_ID = 35,
    DOP_UNLINK_ID_ACK = 36,

    DOP_ALTACT_SIG_SEND = 37
};

#define ERTS_DIST_SPAWN_FLAG_LINK (1 << 0)
#define ERTS_DIST_SPAWN_FLAG_MONITOR (1 << 1)

#define ERTS_DOP_ALTACT_SIG_FLG_PRIO (1 << 0)
#define ERTS_DOP_ALTACT_SIG_FLG_TOKEN (1 << 1)
#define ERTS_DOP_ALTACT_SIG_FLG_ALIAS (1 << 2)
#define ERTS_DOP_ALTACT_SIG_FLG_NAME (1 << 3)
#define ERTS_DOP_ALTACT_SIG_FLG_EXIT (1 << 4)

#ifdef __cplusplus
}
#endif

#endif
