/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef ETF_DECODE_TERM_LENGTH_H
#define ETF_DECODE_TERM_LENGTH_H

#ifdef __cplusplus
extern "C" {
#endif

#include "etf_common.h"

/* Macro Definitions */

#define ETF_DECODE_TERM_LENGTH_FLAG_NONE (0)
#define ETF_DECODE_TERM_LENGTH_FLAG_HAS_EXPORT_EXT (1 << 0)
#define ETF_DECODE_TERM_LENGTH_FLAG_HAS_NEW_FUN_EXT (1 << 1)

/* Type Definitions */

typedef struct etf_decode_term_length_trap_s etf_decode_term_length_trap_t;
typedef enum etf_decode_term_length_trap_state_t etf_decode_term_length_trap_state_t;

typedef void (*etf_decode_term_length_done_t)(ErlNifEnv *caller_env, etf_decode_term_length_trap_t *trap, void *arg,
                                              edf_trap_result_t *result);

enum etf_decode_term_length_trap_state_t {
    ETF_DECODE_TERM_LENGTH_TRAP_STATE_INIT = 0,
    ETF_DECODE_TERM_LENGTH_TRAP_STATE_VERSION_MAGIC,
    ETF_DECODE_TERM_LENGTH_TRAP_STATE_DECODE,
    ETF_DECODE_TERM_LENGTH_TRAP_STATE_DONE,
};

struct etf_decode_term_length_trap_s {
    edf_trap_t super;
    etf_decode_term_length_trap_state_t state;
    bool is_external_term;
    vec_t slice;
    vec_reader_t reader;
    struct {
        etf_decode_term_length_done_t cb;
        void *arg;
    } done;
    int flags;
    size_t heap_size;
    size_t atom_extra_skip;
    /* Keep track of the current number of sub terms remaining to be decoded.
     *
     * We limit the number of sub terms to 2^32-1, even on 64-bit
     * machines, because a term that has many sub-terms must be truly
     * ginormous and is proably a mistake.
     *
     * This means that a map with 2^31 or more elements cannot be decoded,
     * even on a 64-bit machine.
     */
    uint32_t terms;
    const uint8_t *head;
    const uint8_t *tail;
};

/* Function Declarations */

extern ERL_NIF_TERM etf_decode_term_length_trap_open(ErlNifEnv *env, bool is_external_term, vec_t *slice,
                                                     etf_decode_term_length_done_t done_cb, void *done_arg,
                                                     etf_decode_term_length_trap_t **trapp);
extern int etf_fast_skip_terms(ErlNifEnv *caller_env, bool is_external_term, vec_reader_t *vr, uint32_t skip,
                               ERL_NIF_TERM *err_termp);

/* Inline Function Definitions */

#ifdef __cplusplus
}
#endif

#endif
