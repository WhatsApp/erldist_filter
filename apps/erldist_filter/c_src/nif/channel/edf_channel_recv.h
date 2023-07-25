/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef EDF_CHANNEL_RECV_H
#define EDF_CHANNEL_RECV_H

#ifdef __cplusplus
extern "C" {
#endif

#include "edf_channel.h"
#include "../trap/edf_trap.h"
#include "../etf/etf_decode.h"
#include "../etf/etf_encode.h"
#include "../avec.h"

/* Macro Definitions */

/* Type Definitions */

typedef struct edf_channel_recv_trap_s edf_channel_recv_trap_t;

struct edf_channel_recv_trap_s {
    edf_trap_t super;
    edf_channel_resource_t *resource;
    edf_channel_t *channel;
    avec_t actions;
    edf_external_t *external;
    size_t fragment_index;
    size_t packet_count;
};

/* Global Declarations */

/* Function Declarations */

extern ERL_NIF_TERM edf_channel_recv_trap_open(ErlNifEnv *env, edf_channel_resource_t *resource, edf_channel_t *channel,
                                               edf_channel_recv_trap_t **trapp);

/* Inline Function Definitions */

#ifdef __cplusplus
}
#endif

#endif
