/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef ETF_DECODE_DIST_HEADER_H
#define ETF_DECODE_DIST_HEADER_H

#ifdef __cplusplus
extern "C" {
#endif

#include "etf_common.h"

/* Macro Definitions */

/* Type Definitions */

typedef struct etf_decode_dist_header_trap_s etf_decode_dist_header_trap_t;
typedef enum etf_decode_dist_header_trap_state_t etf_decode_dist_header_trap_state_t;

enum etf_decode_dist_header_trap_state_t {
    ETF_DECODE_DIST_HEADER_TRAP_STATE_INIT = 0,
    ETF_DECODE_DIST_HEADER_TRAP_STATE_READ_FLAGS,
    ETF_DECODE_DIST_HEADER_TRAP_STATE_DONE,
};

struct etf_decode_dist_header_trap_s {
    edf_trap_t super;
    etf_decode_dist_header_trap_state_t state;
    edf_external_t *external;
    vec_t vec;
    size_t skip;
    uint8_t number_of_atom_cache_refs;
    size_t flags_size;
    const uint8_t *flagsp;
    bool long_atoms;
    int got_flags;
    int table_index;
    const uint8_t *head;
    const uint8_t *tail;
};

/* Function Declarations */

extern ERL_NIF_TERM etf_decode_dist_header_trap_open(ErlNifEnv *env, edf_external_t *external,
                                                     etf_decode_dist_header_trap_t **trapp);

/* Inline Function Definitions */

#ifdef __cplusplus
}
#endif

#endif
