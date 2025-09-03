/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef ETF_REWRITE_FRAGMENT_HEADER_H
#define ETF_REWRITE_FRAGMENT_HEADER_H

#ifdef __cplusplus
extern "C" {
#endif

#include "etf_common.h"

/* Macro Definitions */

/* Type Definitions */

enum etf_rewrite_fragment_header_trap_state_t {
    ETF_REWRITE_FRAGMENT_HEADER_TRAP_STATE_INIT = 0,
    ETF_REWRITE_FRAGMENT_HEADER_TRAP_STATE_FIND_CONFLICTS,
    ETF_REWRITE_FRAGMENT_HEADER_TRAP_STATE_RESOLVE_ATOMS,
    ETF_REWRITE_FRAGMENT_HEADER_TRAP_STATE_ALLOC,
    ETF_REWRITE_FRAGMENT_HEADER_TRAP_STATE_ENCODE,
    ETF_REWRITE_FRAGMENT_HEADER_TRAP_STATE_DONE,
};

typedef struct etf_rewrite_fragment_header_trap_s etf_rewrite_fragment_header_trap_t;
typedef enum etf_rewrite_fragment_header_trap_state_t etf_rewrite_fragment_header_trap_state_t;

struct etf_rewrite_fragment_header_trap_s {
    edf_trap_t super;
    etf_rewrite_fragment_header_trap_state_t state;
    edf_external_t *external;
    int internal_index;
    bool maybe_rewrite;
    size_t rewrite_size;
};

/* Function Declarations */

extern ERL_NIF_TERM etf_rewrite_fragment_header_trap_open(ErlNifEnv *env, edf_external_t *external,
                                                          etf_rewrite_fragment_header_trap_t **trapp);

/* Inline Function Definitions */

#ifdef __cplusplus
}
#endif

#endif
