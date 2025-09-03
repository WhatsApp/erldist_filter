/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef EDF_EXTERNAL_RECV_H
#define EDF_EXTERNAL_RECV_H

#ifdef __cplusplus
extern "C" {
#endif

#include "edf_channel.h"
#include "../trap/edf_trap.h"
#include "../config/edf_config.h"
#include "../etf/etf_decode.h"
#include "../etf/etf_encode.h"
#include "../vdist/vdist.h"

/* Macro Definitions */

/* Type Definitions */

enum edf_external_recv_trap_state_t {
    EDF_EXTERNAL_RECV_TRAP_STATE_NONE = 0,
    EDF_EXTERNAL_RECV_TRAP_STATE_INIT,
    EDF_EXTERNAL_RECV_TRAP_STATE_COMPACT_REALLOC,
    EDF_EXTERNAL_RECV_TRAP_STATE_COMPACT_COPY,
    EDF_EXTERNAL_RECV_TRAP_STATE_MAYBE_INSPECT,
    EDF_EXTERNAL_RECV_TRAP_STATE_DECODE_PAYLOAD_LENGTH,
    EDF_EXTERNAL_RECV_TRAP_STATE_INSPECT,
    EDF_EXTERNAL_RECV_TRAP_STATE_MAYBE_LOG_EVENT,
    EDF_EXTERNAL_RECV_TRAP_STATE_LOG_EVENT,
    EDF_EXTERNAL_RECV_TRAP_STATE_MAYBE_REDIRECT,
    EDF_EXTERNAL_RECV_TRAP_STATE_REDIRECT_DOP,
    EDF_EXTERNAL_RECV_TRAP_STATE_REDIRECT_SPAWN_REQUEST,
    EDF_EXTERNAL_RECV_TRAP_STATE_EMIT,
    EDF_EXTERNAL_RECV_TRAP_STATE_DROP,
    EDF_EXTERNAL_RECV_TRAP_STATE_DONE,
};

typedef struct edf_external_recv_trap_s edf_external_recv_trap_t;
typedef enum edf_external_recv_trap_state_t edf_external_recv_trap_state_t;

struct edf_external_recv_trap_s {
    edf_trap_t super;
    edf_external_recv_trap_state_t state;
    edf_external_t *external;
    size_t fragment_index;
    size_t old_frag_size;
    struct {
        size_t skip;
        size_t framing_length;
        size_t headers_length;
        size_t control_length;
        size_t payload_length;
        size_t total;
    } new_frag_size;
};

/* Global Declarations */

/* Function Declarations */

extern ERL_NIF_TERM edf_external_recv_trap_open(ErlNifEnv *env, edf_external_t *external, edf_external_recv_trap_t **trapp);

/* Inline Function Definitions */

#ifdef __cplusplus
}
#endif

#endif
