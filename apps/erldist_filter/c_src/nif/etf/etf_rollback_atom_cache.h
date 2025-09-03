/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef ETF_ROLLBACK_ATOM_CACHE_H
#define ETF_ROLLBACK_ATOM_CACHE_H

#ifdef __cplusplus
extern "C" {
#endif

#include "etf_common.h"

/* Macro Definitions */

/* Type Definitions */

enum etf_rollback_atom_cache_trap_state_t {
    ETF_ROLLBACK_ATOM_CACHE_TRAP_STATE_INIT = 0,
    ETF_ROLLBACK_ATOM_CACHE_TRAP_STATE_ALLOC,
    ETF_ROLLBACK_ATOM_CACHE_TRAP_STATE_ENCODE,
    ETF_ROLLBACK_ATOM_CACHE_TRAP_STATE_DONE,
};

typedef struct etf_rollback_atom_cache_trap_s etf_rollback_atom_cache_trap_t;
typedef enum etf_rollback_atom_cache_trap_state_t etf_rollback_atom_cache_trap_state_t;

struct etf_rollback_atom_cache_trap_s {
    edf_trap_t super;
    etf_rollback_atom_cache_trap_state_t state;
    edf_external_t *external;
    int internal_index;
    size_t rollback_size;
};

/* Function Declarations */

extern ERL_NIF_TERM etf_rollback_atom_cache_trap_open(ErlNifEnv *env, edf_external_t *external,
                                                      etf_rollback_atom_cache_trap_t **trapp);

/* Inline Function Definitions */

#ifdef __cplusplus
}
#endif

#endif
